module Distribution.Client.Utils.Handle
 ( withTee
 , withPipeFor
 , withProgressBar
 ) where

import Control.Concurrent.Async  (withAsync, waitCatch)
import Control.Exception         (finally, onException)
import Control.Monad             (unless, void)
import Data.Foldable             (for_)
import System.IO
  ( BufferMode (LineBuffering)
  , Handle
  , hClose, hGetLine, hIsEOF, hPutStrLn, hSetBuffering
  )
import System.IO.Error           (catchIOError)
import System.Process            (createPipe)

import Distribution.Verbosity
  ( Verbosity
  , VerbosityHandles (..)
  , mkVerbosity
  , verbosityFlags
  , verbosityHandles
  )

-- | Close a handle, ignoring any 'IOError' (e.g. the handle is already
-- closed). Safe to use in cleanup paths where raising a fresh exception
-- would mask the original one.
closeQuietly :: Handle -> IO ()
closeQuietly h = hClose h `catchIOError` \_ -> return ()

-- | Write @line@ (with trailing newline) to every sink. Per-sink
-- 'IOError's are swallowed so a single broken sink cannot block the
-- remaining sinks or stall the pipe being drained.
writeToAll :: String -> [(Handle, Bool)] -> String -> IO ()
writeToAll pkgname sinks line =
  for_ sinks $ \(sink, withPkgName) -> do
    let line' | withPkgName = pkgname ++ ": " ++ line
              | otherwise   = line
    hPutStrLn sink line' `catchIOError` \_ -> return ()

-- | Drain @h@ line-by-line into every sink until EOF.
forwardLines :: String -> Handle -> [(Handle, Bool)] -> IO ()
forwardLines pkgname h sinks = loop
  where
    loop = do
      eof <- hIsEOF h
      unless eof $ do
        hGetLine h >>= writeToAll pkgname sinks
        loop

-- | Run @action@ with the write end of a freshly-created pipe whose
-- read end is consumed by @consumer@.
--
-- The write end is closed when @action@ returns (or throws), signalling
-- EOF to the consumer. On the success path the consumer is joined
-- before control returns, so every byte written by @action@ has been
-- processed by @consumer@ by the time this function returns.
--
-- @action@ must not close the write end itself.
withPipeFor :: (Handle -> IO ()) -> (Handle -> IO a) -> IO a
withPipeFor consumer action = do
  (readHndl, writeHndl) <- createPipe
  hSetBuffering readHndl LineBuffering
  let run = withAsync (consumer readHndl) $ \reader -> do
              let finish = closeQuietly writeHndl >> void (waitCatch reader)
              result <- action writeHndl `onException` finish
              finish
              return result
  run `finally` closeQuietly readHndl
  
-- | Run @action@ with the write end of a pipe whose read end is
-- forwarded line-by-line to every sink.
withTee :: String -> [(Handle, Bool)] -> (Handle -> IO a) -> IO a
withTee pkgname sinks = withPipeFor $ \h -> forwardLines pkgname h sinks

withProgressBar :: Verbosity -> (Verbosity -> IO a) -> IO a
withProgressBar verbosity0 k =
  withPipeFor (forwardTo (vGHCOutput (verbosityHandles verbosity0))) $ \ghcWrite ->
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
