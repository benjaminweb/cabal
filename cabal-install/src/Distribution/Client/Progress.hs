module Distribution.Client.Progress (withProgressBar, bracketProgress, progressVerbosity, drainGhc) where

import Control.Monad   (unless, forM_)
import System.IO       (Handle, hGetLine, hPutStrLn, hIsEOF, stderr, stdout)
import System.IO.Error (catchIOError)
import qualified Data.Text.IO as TIO
import Control.Concurrent.Async (withAsync)
import Control.Concurrent.STM (atomically)
import qualified Data.Text as T

import Distribution.Types.Progress
import Distribution.Client.Progress.Parser
import Distribution.Client.Progress.Runner
import Distribution.Client.Progress.State
import Distribution.Client.Utils.Handle
import Distribution.Verbosity
  (Verbosity, VerbosityHandles(..), mkVerbosity, verbosityFlags, verbosityHandles)

-- | Run @k@ with a 'Verbosity' whose child stdout is captured into a
-- progress bar, and whose lifecycle events update that bar.
withProgressBar :: Verbosity -> Int -> (Verbosity -> IO a) -> IO a
withProgressBar verbosity0 totalComponents k =
  bracketProgress stdout totalComponents $ \progress ->
    withPipeFor (drainGhc progress) $ \ghcWrite ->
      k (progressVerbosity progress verbosity0 ghcWrite)

-- | Bracket the lifecycle of a progress bar: create the 'Progress' TVar,
-- start the renderer thread, and cancel it (clearing the display via
-- 'runner'\'s own @finally@) when the body exits — normally or by
-- exception.
bracketProgress :: Handle -> Int -> (Progress -> IO a) -> IO a
bracketProgress h totalComponents k = do
  progress <- initProgress totalComponents
  withAsync (runner h progress) $ \_ -> k progress

-- | Rewire a 'Verbosity' so that
--
--   * child stdout/stderr goes into @ghcWrite@ (drained by 'drainGhc'),
--   * cabal's lifecycle events go through 'makingProgress' on @progress@.
--
-- Leaves 'vhCabalNormal' / 'vhCabalError' untouched, so Cabal's own log
-- messages keep their existing destination.
progressVerbosity :: Progress -> Verbosity -> Handle -> Verbosity
progressVerbosity progress verbosity0 ghcWrite =
  let hs = (verbosityHandles verbosity0)
      hs' = hs
             { vGHCOutput    = ghcWrite
             , vProgressSink = Just (atomically . makingProgress progress)
             , vCabalStdout  = vCabalStderr hs
             }
  in mkVerbosity hs' (verbosityFlags verbosity0)

-- | Drain the GHC pipe into @progress@, line by line, until EOF.
drainGhc :: Progress -> Handle -> IO ()
drainGhc progress h = loop
  where
    loop = do
      eof <- hIsEOF h
      unless eof $ do
        line <- TIO.hGetLine h
        -- hPutStrLn stderr ("[drain] " ++ T.unpack line)
        forM_ (parseGhcLine line) $ \ge -> do
          -- hPutStrLn stderr ("[ge] " ++ show ge)        -- ← add this
          atomically (makingProgress progress ge)
        loop
