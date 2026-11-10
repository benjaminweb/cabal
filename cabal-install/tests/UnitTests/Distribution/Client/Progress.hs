{-# LANGUAGE OverloadedStrings #-}
module UnitTests.Distribution.Client.Progress (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Data.Either      (isLeft)
import Data.Maybe       (isJust)
import System.IO        (hGetLine, hPutStrLn, hClose, openTempFile, hSetBuffering, BufferMode (LineBuffering))
import System.Directory (getTemporaryDirectory, removeFile) 
import System.Timeout   (timeout)
import System.Process   (createPipe)
import Text.Parsec      (parse)
import qualified Data.Map.Strict as M
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async)
import Control.Concurrent.STM  (readTVarIO, atomically)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Exception (try, IOException)
import Data.IORef        (newIORef, readIORef)
import Data.List         (isInfixOf)
import qualified Data.Text as T

import Distribution.Parsec                   (simpleParsec)
import Distribution.Types.PackageId          (PackageIdentifier (..))
import Distribution.Types.Progress           (ComponentEvent(..), ProgressEvent(..))

import Distribution.Client.Progress          (bracketProgress, drainGhc, progressVerbosity)
import Distribution.Client.Progress.Parser   (rectBracket, ghcLineParser, parseGhcLine, progressEventParser)
import Distribution.Client.Progress.Runner   (drawOnce)
import Distribution.Client.Progress.Render   (filledSegments, clearToEndOfScreen, moveCursorLinesUp, defaultColumnWidths, componentLine, renderState)
import Distribution.Client.Progress.State    (Progress(..), ComponentsCompiling (..), initProgress, makingProgress, completedCount)
import Distribution.Client.Utils.Handle      (withPipeFor, withTee)

import Distribution.Verbosity
  ( VerbosityHandles (..)
  , defaultVerbosityHandles
  , mkVerbosity
  , normal
  , verbosityHandles
  )

mkPkgId :: String -> PackageIdentifier
mkPkgId s = case simpleParsec s of
  Just p  -> p
  Nothing -> error ("test setup: bad package id " ++ s)

tests :: [TestTree]
tests = [testGroup "Distribution.Client.Progress" 
  [ progress, parser, withProgressBarParts, tvTests, stickyTests, completedCountTests
  , progressVerbosityTests, componentLineTests, drainGhcTests, ansiTests, renderStateTests
  , drawOnceTests, runnerTests, withPipeForTests, withTeeTests
  ]] 

progress = testGroup "progress lib"
 [ testCase "filledSegments computes scaled filled count correctly" $ do
     filledSegments 40 2 10 @?= 8
     filledSegments 40 1 80 @?= 0
     filledSegments 40 2 80 @?= 1
     filledSegments 40 79 80 @?= 39
     filledSegments 40 80 80 @?= 40
 ]

parser = testGroup "ghc line parser"
 [ testCase "rectBracket parser `[x of n]` correctly" $ do
    parse rectBracket "" "[1 of 3]"   @?= Right (1,3)
    parse rectBracket "" "[1 of 99]"  @?= Right (1,99)
    parse rectBracket "" "[22 of 99]" @?= Right (22,99)
 , testCase "ghcLineParser parses lines correctly: [1 of 3]" $ do
      parse ghcLineParser "" "[1 of 3] Compiling Some.Fancy.Module other"
      @?= Right (CompilingModule {ceDoneCount = 0, ceTotalCount = 3, ceModuleName = "Some.Fancy.Module"})
 , testCase "ghcLineParser parses lines correctly: [3 of 3]" $ do
      parse ghcLineParser "" "[3 of 3] Compiling Some.Fancy.Module other"
      @?= Right (CompilingModule {ceDoneCount = 2, ceTotalCount = 3, ceModuleName = "Some.Fancy.Module"})
 , testCase "progressEvent parses full line" $
      parse progressEventParser "" "text-2.0.2: [2 of 5] Compiling Data.Text ( src, obj )"
      @?= Right (ProgressEvent
                   (mkPkgId "text-2.0.2")
                   (CompilingModule "Data.Text" 1 5))
 , testCase "progressEventParser parses Configuring" $
      parse progressEventParser "" "text-2.0.2: Configuring text-2.0.2..."
      @?= Right (ProgressEvent (mkPkgId "text-2.0.2") Configuring)
 , testCase "progressEventParser parses Installing" $
      parse progressEventParser "" "text-2.0.2: Installing library in ..."
      @?= Right (ProgressEvent (mkPkgId "text-2.0.2") Installing)
 , testCase "progressEventParser rejects bad package prefix" $
      isLeft (parse progressEventParser "" "bad pkg!: [1 of 1] Compiling Foo ( … )")
      @? "expected Left"
 , testCase "progressEventParser rejects Linking" $
      isLeft (parse progressEventParser "" "text-2.0.2: Linking foo")
      @? "expected Left"
 , testCase "parseGhcLine collapses failures to Nothing" $ do
      parseGhcLine "text-2.0.2: Linking foo"   @?= Nothing
      parseGhcLine "no colon"                  @?= Nothing
      parseGhcLine ""                          @?= Nothing
 , testCase "parseGhcLine: real line with dotted version" $
      parseGhcLine "indexed-traversable-0.1.5: [1 of 7] Compiling CoerceCompat     ( src/CoerceCompat.hs, dist/build/CoerceCompat.o, dist/build/CoerceCompat.dyn_o )"
      @?= Just (ProgressEvent
                  (mkPkgId "indexed-traversable-0.1.5")
                  (CompilingModule "CoerceCompat" 0 7))
 , testCase "ghcLineParser handles GHC's space-padded counter" $
      parse ghcLineParser "" "[ 1 of 14] Compiling Foo ( src, obj )"
        @?= Right (CompilingModule "Foo" 0 14)
 , testCase "parseGhcLine: multi-hyphen package name" $
      isJust (parseGhcLine "haskell-src-exts-1.24.0: [1 of 5] Compiling Foo ( src, obj )")
        @? "expected Just"
 , testCase "parseGhcLine: four-component version" $
      isJust (parseGhcLine "foo-1.2.3.4: [1 of 3] Compiling Foo ( src, obj )")
        @? "expected Just"
 ]

withProgressBarParts = testGroup "withProgressBar tests"
  [ testCase "redirects vCabalStdout to stderr, preserves vCabalStderr and installs sink/pipe" $ do
      progress <- initProgress 0
      (r, w)   <- createPipe
      let v0  = mkVerbosity defaultVerbosityHandles normal
          v'  = progressVerbosity progress v0 w
          hs0 = verbosityHandles v0
          hs' = verbosityHandles v'
      vCabalStdout hs' @?= vCabalStderr hs0
      vCabalStderr hs' @?= vCabalStderr hs0
      vGHCOutput   hs' @?= w
      isJust (vProgressSink hs') @? "sink installed"
      hClose r >> hClose w
  , testCase "drainGhc writes CompilingModule events" $ do
      progress <- initProgress 1
      (r, w) <- createPipe
      hPutStrLn w "text-2.0.2: [2 of 5] Compiling Data.Text.Internal ( src, obj )"
      hPutStrLn w "text-2.0.2: Linking something"
      hClose w
      drainGhc progress r
      hClose r
      cc <- readTVarIO (pComponentsCompiling progress)
      case M.lookup (mkPkgId "text-2.0.2") (ccComponentEvents cc) of
        Just (CompilingModule "Data.Text.Internal" 1 5) -> pure ()
        other -> assertFailure ("unexpected: " <> show other)
  , testCase "bracketProgress cancels renderer on exception" $ do
      tmpDir <- getTemporaryDirectory
      (path, h) <- openTempFile tmpDir "cabal-bracket"
      r <- try $ bracketProgress h 0 $ \_ -> ioError (userError "boom")
      hClose h
      removeFile path
      case r :: Either IOException () of
        Left  _ -> pure ()
        Right _ -> assertFailure "expected exception"
  ]

tvTests :: TestTree
tvTests = testGroup "TVar storage"
  [ testCase "CompilingModule lands in the map and updates ccModuleCounts" $ do
      progress <- initProgress 3
      let Just ev = parseGhcLine "text-2.0.2: [2 of 5] Compiling Data.Text ( src, obj )"
      atomically $ makingProgress progress ev
      cc <- readTVarIO (pComponentsCompiling progress)
      case M.lookup (pePackageId ev) (ccComponentEvents cc) of
        Just (CompilingModule "Data.Text" 1 5) -> pure ()
        other -> assertFailure ("unexpected: " ++ show other)
      M.lookup (pePackageId ev) (ccModuleCounts cc) @?= Just 5
  , testCase "successive CompilingModule events overwrite the entry" $ do
      progress <- initProgress 3
      let Just ev1 = parseGhcLine "text-2.0.2: [1 of 5] Compiling Data.Text.Internal ( src, obj )"
          Just ev2 = parseGhcLine "text-2.0.2: [2 of 5] Compiling Data.Text ( src, obj )"
      atomically $ makingProgress progress ev1
      atomically $ makingProgress progress ev2
      cc <- readTVarIO (pComponentsCompiling progress)
      M.lookup (pePackageId ev2) (ccComponentEvents cc)
        @?= Just (CompilingModule "Data.Text" 1 5)
  , testCase "ComponentDone overwrites CompilingModule" $ do
      progress <- initProgress 1
      let Just ev1 = parseGhcLine "text-2.0.2: [5 of 5] Compiling Data.Text ( src, obj )"
          Just pid = simpleParsec "text-2.0.2"
      atomically $ makingProgress progress ev1
      atomically $ makingProgress progress (ProgressEvent pid ComponentDone)
      cc <- readTVarIO (pComponentsCompiling progress)
      M.lookup pid (ccComponentEvents cc) @?= Just ComponentDone
  ]

stickyTests :: TestTree
stickyTests = testGroup "sticky ComponentDone/Failed"
  [ testCase "CompilingModule after ComponentDone is ignored" $ do
      progress <- initProgress 1
      let pid      = mkPkgId "text-2.0.2"
          Just ev  = parseGhcLine "text-2.0.2: [5 of 5] Compiling Data.Text ( src, obj )"
      atomically $ makingProgress progress (ProgressEvent pid ComponentDone)
      atomically $ makingProgress progress ev
      cc <- readTVarIO (pComponentsCompiling progress)
      M.lookup pid (ccComponentEvents cc) @?= Just ComponentDone
  , testCase "Building after CompilingModule is ignored" $ do
      progress <- initProgress 1
      let pid      = mkPkgId "text-2.0.2"
          Just ev  = parseGhcLine "text-2.0.2: [3 of 5] Compiling Data.Text ( src, obj )"
      atomically $ makingProgress progress ev
      atomically $ makingProgress progress (ProgressEvent pid Building)
      cc <- readTVarIO (pComponentsCompiling progress)
      M.lookup pid (ccComponentEvents cc) @?= Just (CompilingModule "Data.Text" 2 5)
  , testCase "CompilingModule after ComponentFailed is ignored" $ do
      progress <- initProgress 1
      let pid      = mkPkgId "text-2.0.2"
          Just ev  = parseGhcLine "text-2.0.2: [3 of 5] Compiling Data.Text ( src, obj )"
      atomically $ makingProgress progress (ProgressEvent pid (ComponentFailed "boom"))
      atomically $ makingProgress progress ev
      cc <- readTVarIO (pComponentsCompiling progress)
      M.lookup pid (ccComponentEvents cc) @?= Just (ComponentFailed "boom")
  ]

completedCountTests :: TestTree
completedCountTests = testGroup "completedCount"
  [ testCase "empty map → 0" $
      completedCount (ComponentsCompiling M.empty M.empty) @?= 0
  , testCase "only CompilingModule → 0" $ do
      progress <- initProgress 1
      let Just ev = parseGhcLine "text-2.0.2: [1 of 5] Compiling Foo ( src, obj )"
      atomically $ makingProgress progress ev
      cc <- readTVarIO (pComponentsCompiling progress)
      completedCount cc @?= 0
  , testCase "one ComponentDone → 1" $ do
      progress <- initProgress 1
      atomically $ makingProgress progress (ProgressEvent (mkPkgId "text-2.0.2") ComponentDone)
      cc <- readTVarIO (pComponentsCompiling progress)
      completedCount cc @?= 1
  , testCase "ComponentFailed counts as done" $ do
      progress <- initProgress 1
      atomically $ makingProgress progress (ProgressEvent (mkPkgId "text-2.0.2") (ComponentFailed "boom"))
      cc <- readTVarIO (pComponentsCompiling progress)
      completedCount cc @?= 1
  , testCase "mixed map → sum of terminal entries" $ do
      progress <- initProgress 3
      let Just ev = parseGhcLine "a-1.0: [1 of 2] Compiling Foo ( src, obj )"
      atomically $ makingProgress progress ev
      atomically $ makingProgress progress (ProgressEvent (mkPkgId "b-1.0") ComponentDone)
      atomically $ makingProgress progress (ProgressEvent (mkPkgId "c-1.0") (ComponentFailed "boom"))
      cc <- readTVarIO (pComponentsCompiling progress)
      completedCount cc @?= 2
  ]

progressVerbosityTests :: TestTree
progressVerbosityTests = testGroup "progressVerbosity"
  [ testCase "sets pipe/sink and redirects vCabalStdout to stderr" $ do
      progress <- initProgress 0
      (r, w)   <- createPipe
      let v0  = mkVerbosity defaultVerbosityHandles normal
          v'  = progressVerbosity progress v0 w
          hs0 = verbosityHandles v0
          hs' = verbosityHandles v'
      vCabalStdout hs' @?= vCabalStderr hs0
      vCabalStderr hs' @?= vCabalStderr hs0
      vGHCOutput   hs' @?= w
      isJust (vProgressSink hs') @? "sink installed"
      hClose r >> hClose w
  ]

componentLineTests :: TestTree
componentLineTests = testGroup "componentLine"
  [ testCase "CompilingModule → Just" $
      isJust (componentLine defaultColumnWidths (mkPkgId "text-2.0.2") (CompilingModule "Data.Text" 2 5))
        @? "expected Just"
  , testCase "ComponentDone → Just done" $
      case componentLine defaultColumnWidths (mkPkgId "text-2.0.2") ComponentDone of
        Just line -> assertBool "contains done" ("Done" `T.isInfixOf` line)
        Nothing   -> assertFailure "expected Just"
  , testCase "ComponentFailed → Just Failed" $
      case componentLine defaultColumnWidths (mkPkgId "text-2.0.2") (ComponentFailed "boom") of
        Just line -> assertBool "contains Failed" ("Failed" `T.isInfixOf` line)
        Nothing   -> assertFailure "expected Just"
  ]

drainGhcTests :: TestTree
drainGhcTests = testGroup "drainGhc"
  [ testCase "stores parsed events, skips unparseable" $ do
      progress <- initProgress 2
      (r, w)   <- createPipe
      hPutStrLn w "text-2.0.2: [1 of 5] Compiling Foo ( src, obj )"
      hPutStrLn w "text-2.0.2: Linking foo"                     -- ignored
      hPutStrLn w "bytestring-0.12.1.0: [1 of 3] Compiling Bar ( src, obj )"
      hClose w
      drainGhc progress r
      hClose r
      cc <- readTVarIO (pComponentsCompiling progress)
      M.size (ccComponentEvents cc) @?= 2
      M.lookup (mkPkgId "text-2.0.2") (ccComponentEvents cc)
        @?= Just (CompilingModule "Foo" 0 5)
      M.lookup (mkPkgId "bytestring-0.12.1.0") (ccComponentEvents cc)
        @?= Just (CompilingModule "Bar" 0 3)
  , testCase "empty pipe → empty map" $ do
      progress <- initProgress 0
      (r, w)   <- createPipe
      hClose w
      drainGhc progress r
      hClose r
      cc <- readTVarIO (pComponentsCompiling progress)
      M.size (ccComponentEvents cc) @?= 0
  ]

ansiTests :: TestTree
ansiTests = testGroup "ANSI"
  [ testCase "moveCursorLinesUp 0 → empty" $
      moveCursorLinesUp 0 @?= ""
  , testCase "moveCursorLinesUp 3 → ESC[3F" $
      moveCursorLinesUp 3 @?= "\ESC[3F"
  , testCase "clearToEndOfScreen → ESC[J" $
      clearToEndOfScreen @?= "\ESC[J"
  ]

renderStateTests :: TestTree
renderStateTests = testGroup "renderState"
  [ testCase "empty map → just total line" $
      length (renderState (ComponentsCompiling M.empty M.empty) 0 3) @?= 1
  , testCase "CompilingModule → total + local line" $ do
      progress <- initProgress 2
      let Just ev = parseGhcLine "text-2.0.2: [3 of 5] Compiling Data.Text ( src, obj )"
      atomically $ makingProgress progress ev
      cc <- readTVarIO (pComponentsCompiling progress)
      let ls = renderState cc (completedCount cc) 2
      length ls @?= 2
  , testCase "ComponentDone → total + completed line" $ do
      progress <- initProgress 1
      atomically $ makingProgress progress (ProgressEvent (mkPkgId "text-2.0.2") ComponentDone)
      cc <- readTVarIO (pComponentsCompiling progress)
      let lines = renderState cc (completedCount cc) 1
      length lines @?= 2
  ]

drawOnceTests :: TestTree
drawOnceTests = testGroup "drawOnce"
  [ testCase "writes local line to handle" $ do
      progress <- initProgress 2
      let Just ev = parseGhcLine "text-2.0.2: [3 of 5] Compiling Data.Text ( src, obj )"
      atomically $ makingProgress progress ev
      linesRef <- newIORef 0
      tmpDir   <- getTemporaryDirectory
      (path, h) <- openTempFile tmpDir "cabal-draw"
      drawOnce h progress linesRef
      hClose h
      contents <- readFile path
      removeFile path
      assertBool "local name"  $ "text"      `isInfixOf` contents
      assertBool "local ver"   $ "2.0.2"     `isInfixOf` contents
      assertBool "module"      $ "Data.Text" `isInfixOf` contents
      n <- readIORef linesRef
      n @?= 2
  ]

runnerTests :: TestTree
runnerTests = testGroup "runner"
  [ testCase "renders local lines for a compiling package" $ do
      tmpDir <- getTemporaryDirectory
      (path, h) <- openTempFile tmpDir "cabal-runner"
      bracketProgress h 1 $ \progress -> do
        let Just ev = parseGhcLine "text-2.0.2: [3 of 5] Compiling Data.Text ( src, obj )"
        atomically $ makingProgress progress ev
        threadDelay 200000
      hClose h
      contents <- readFile path
      removeFile path
      assertBool "local name" $ "text"      `isInfixOf` contents
      assertBool "local ver"  $ "2.0.2"     `isInfixOf` contents
      assertBool "module"     $ "Data.Text" `isInfixOf` contents
  ]

withPipeForTests :: TestTree
withPipeForTests = testGroup "withPipeFor"
  [ testCase "line buffering: delivers lines before write end is closed" $ do
      received <- newEmptyMVar
      let consumer h = do
            line <- hGetLine h
            putMVar received line
      mres <- timeout 1000000 $ withPipeFor consumer $ \writeH -> do
        hPutStrLn writeH "hello"
        takeMVar received
      case mres of
        Nothing  -> assertFailure "Timeout: line was not flushed to the consumer"
        Just line -> line @?= "hello"
  ]

withTeeTests :: TestTree
withTeeTests = testGroup "withTee"
  [ testCase "line buffering: delivers lines to sink before write end is closed" $ do
      received <- newEmptyMVar
      (sinkR, sinkW) <- createPipe
      hSetBuffering sinkW LineBuffering
      _ <- async $ do
        line <- hGetLine sinkR
        putMVar received line
      mres <- timeout 1000000 $ withTee "pkg" [(sinkW, False)] $ \writeH -> do
        hPutStrLn writeH "hello"
        takeMVar received
      hClose sinkR
      hClose sinkW
      case mres of
        Nothing  -> assertFailure "Timeout: withTee did not flush to the sink"
        Just line -> line @?= "hello"
  ]
