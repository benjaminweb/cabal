module Distribution.Client.Progress (withProgressBar) where

import Control.Monad   (unless)
import System.IO       (hGetLine, hPutStrLn, hIsEOF, stderr, stdout)
import System.IO.Error (catchIOError)

import Distribution.Client.Progress.Parser
import Distribution.Client.Progress.Runner
import Distribution.Client.Progress.State
import Distribution.Client.Progress.Types
import Distribution.Client.Utils.Handle
import Distribution.Verbosity
  (Verbosity, VerbosityHandles(..), mkVerbosity, verbosityFlags, verbosityHandles)

withProgressBar :: Verbosity -> (Verbosity -> IO a) -> IO a
withProgressBar verbosity0 k = do
  hPutStrLn stderr "[dbg] withProgressBar entered"
  withPipeFor (forwardTo (vGHCOutput (verbosityHandles verbosity0))) $ \ghcWrite -> do
    hPutStrLn stderr ("[dbg] dest = " ++ show (vGHCOutput (verbosityHandles verbosity0)))
    k $ mkVerbosity
          ((verbosityHandles verbosity0) { vGHCOutput = ghcWrite })
          (verbosityFlags verbosity0)
  where
    forwardTo dest h = do
      let loop = do
            eof <- hIsEOF h
            unless eof $ do
              line <- hGetLine h
              hPutStrLn dest line `catchIOError` \_ -> return ()
              loop
      loop
