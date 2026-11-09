{-# LANGUAGE OverloadedStrings #-}
module UnitTests.Distribution.Client.Utils.Handle (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Control.Exception (try, throwIO, IOException)
import Control.Monad     (unless)
import Data.IORef
import System.Directory  (getTemporaryDirectory, removeFile)
import System.IO
import System.Process    (createPipe)
import System.IO.Error   (userError, isUserError)

import Distribution.Client.Utils.Handle (withPipeFor, withTee, forwardLines)

tests :: [TestTree]
tests = [testGroup "Distribution.Client.Utils.Handle"
          [ withPipeForTests
          , withTeeTests
          , forwardLinesTests
          ]
        ]

-- | Run @k@ with a fresh temp-file handle, then return both the action's
-- result and the file's final contents. The handle is closed before
-- reading, so buffered writes are flushed.
withSink :: (Handle -> IO a) -> IO (a, String)
withSink k = do
  tmpDir <- getTemporaryDirectory
  (path, h) <- openTempFile tmpDir "cabal-tee-sink"
  a <- k h
  hClose h
  contents <- readFile path
  removeFile path
  pure (a, contents)

withTwoSinks :: (Handle -> Handle -> IO a) -> IO (a, String, String)
withTwoSinks k = do
  tmpDir <- getTemporaryDirectory
  (p1, h1) <- openTempFile tmpDir "cabal-tee-sink1"
  (p2, h2) <- openTempFile tmpDir "cabal-tee-sink2"
  a <- k h1 h2
  hClose h1
  hClose h2
  c1 <- readFile p1
  c2 <- readFile p2
  removeFile p1
  removeFile p2
  pure (a, c1, c2)

----------------------------------------------------------------------
-- withPipeFor
----------------------------------------------------------------------

withPipeForTests :: TestTree
withPipeForTests = testGroup "withPipeFor"
  [ testCase "consumer sees lines written by the action, in order" $ do
      ref <- newIORef ([] :: [String])
      withPipeFor (drainInto ref) $ \writeH -> do
        hPutStrLn writeH "alpha"
        hPutStrLn writeH "beta"
        hPutStrLn writeH "gamma"
      got <- readIORef ref
      reverse got @?= ["alpha", "beta", "gamma"]

  , testCase "consumer completes before withPipeFor returns" $ do
      ref <- newIORef False
      let consumer h = drainDiscard h >> writeIORef ref True
      withPipeFor consumer $ \writeH -> hPutStrLn writeH "x"
      readIORef ref >>= assertBool "consumer finished"

  , testCase "empty action produces no lines" $ do
      ref <- newIORef ([] :: [String])
      withPipeFor (drainInto ref) $ \_writeH -> pure ()
      readIORef ref >>= (@?= [])

  , testCase "exception in action still drains lines already written" $ do
      ref <- newIORef ([] :: [String])
      result <- try $ withPipeFor (drainInto ref) $ \writeH -> do
        hPutStrLn writeH "before"
        hPutStrLn writeH "throw"
        throwIO (userError "boom")
      case result :: Either IOException () of
        Left e  -> assertBool "userError" (isUserError e)
        Right _ -> assertFailure "expected exception"
      got <- readIORef ref
      reverse got @?= ["before", "throw"]

  , testCase "consumer receives many lines without loss" $ do
      ref <- newIORef ([] :: [String])
      let n = 1000
      withPipeFor (drainInto ref) $ \writeH ->
        mapM_ (\i -> hPutStrLn writeH ("line-" ++ show i)) [1 .. n :: Int]
      got <- readIORef ref
      length got @?= n
      reverse got @?= ["line-" ++ show i | i <- [1 .. n]]
  ]

drainInto :: IORef [String] -> Handle -> IO ()
drainInto ref h = loop
  where
    loop = do
      eof <- hIsEOF h
      unless eof $ hGetLine h >>= \l -> modifyIORef' ref (l :) >> loop

drainDiscard :: Handle -> IO ()
drainDiscard h = loop
  where
    loop = do
      eof <- hIsEOF h
      unless eof $ hGetLine h >> loop

----------------------------------------------------------------------
-- withTee
----------------------------------------------------------------------

withTeeTests :: TestTree
withTeeTests = testGroup "withTee"
  [ testCase "single sink gets all lines" $ do
      (_, contents) <- withSink $ \sink ->
        withTee "pkg" [(sink, False)] $ \writeH -> do
          hPutStrLn writeH "one"
          hPutStrLn writeH "two"
      contents @?= "one\ntwo\n"

  , testCase "two sinks both get all lines" $ do
      (_, c1, c2) <- withTwoSinks $ \s1 s2 ->
        withTee "pkg" [(s1, False), (s2, False)] $ \writeH -> do
          hPutStrLn writeH "a"
          hPutStrLn writeH "b"
      c1 @?= "a\nb\n"
      c2 @?= "a\nb\n"

  , testCase "prefix applied only to sinks marked True" $ do
      (_, prefixed, raw) <- withTwoSinks $ \s1 s2 ->
        withTee "pkg-1.2.3" [(s1, True), (s2, False)] $ \writeH -> do
          hPutStrLn writeH "hello"
          hPutStrLn writeH "world"
      prefixed @?= "pkg-1.2.3: hello\npkg-1.2.3: world\n"
      raw      @?= "hello\nworld\n"

  , testCase "empty action produces empty sinks" $ do
      (_, c1, c2) <- withTwoSinks $ \s1 s2 ->
        withTee "pkg" [(s1, True), (s2, False)] $ \_writeH -> pure ()
      c1 @?= ""
      c2 @?= ""

  , testCase "long lines (larger than PIPE_BUF) are forwarded intact" $ do
      let longLine = replicate 10000 'x'
      (_, contents) <- withSink $ \sink ->
        withTee "pkg" [(sink, False)] $ \writeH ->
          hPutStrLn writeH longLine
      contents @?= longLine ++ "\n"

  , testCase "many lines across two sinks, order preserved" $ do
      let n = 500
          lines' = ["line-" ++ show i | i <- [1 .. n :: Int]]
      (_, c1, c2) <- withTwoSinks $ \s1 s2 ->
        withTee "pkg" [(s1, False), (s2, False)] $ \writeH ->
          mapM_ (hPutStrLn writeH) lines'
      c1 @?= unlines lines'
      c2 @?= unlines lines'
  ]

----------------------------------------------------------------------
-- forwardLines
----------------------------------------------------------------------

forwardLinesTests :: TestTree
forwardLinesTests = testGroup "forwardLines"
  [ testCase "forwards pre-existing lines from a source handle" $ do
      (srcR, srcW) <- createPipe
      hPutStrLn srcW "one"
      hPutStrLn srcW "two"
      hClose srcW                               -- EOF for srcR
      (_, contents) <- withSink $ \sink ->
        forwardLines "pkg" srcR [(sink, False)]
      hClose srcR
      contents @?= "one\ntwo\n"

  , testCase "prefix applied per-sink" $ do
      (srcR, srcW) <- createPipe
      hPutStrLn srcW "hello"
      hClose srcW
      (_, prefixed, raw) <- withTwoSinks $ \s1 s2 ->
        forwardLines "pkg" srcR [(s1, True), (s2, False)]
      hClose srcR
      prefixed @?= "pkg: hello\n"
      raw      @?= "hello\n"

  , testCase "empty source produces empty sinks" $ do
      (srcR, srcW) <- createPipe
      hClose srcW                               -- immediate EOF
      (_, contents) <- withSink $ \sink ->
        forwardLines "pkg" srcR [(sink, True)]
      hClose srcR
      contents @?= ""
  ]
