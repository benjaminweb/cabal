{-# LANGUAGE RecordWildCards #-}
module Distribution.Client.Progress.Runner (runner, drawOnce) where

import qualified Data.Map.Strict as M
import Control.Monad               (unless)
import Control.Concurrent.STM      (atomically, check, readTVar)
import Control.Concurrent.STM.TVar (readTVar, readTVarIO)
import Control.Exception           (finally)
import Data.IORef
import System.IO                   (hPutStrLn, stderr, Handle)
import qualified Data.Text as T

import Distribution.Types.Progress
import Distribution.Client.Progress.State (Progress(..), ComponentsCompiling(..), completedCount)
import Distribution.Client.Progress.Render

drawState :: Handle -> Progress -> IORef Int -> ComponentsCompiling -> IO ()
drawState h progress linesRef cc = do
  let newLines = renderState (ccComponentEvents cc)
                   (completedCount cc) (pTotalComponentCount progress)
  prev <- readIORef linesRef
  redraw h prev newLines
  writeIORef linesRef (length newLines)

drawOnce :: Handle -> Progress -> IORef Int -> IO ()
drawOnce h progress linesRef = do
  cc <- readTVarIO (pComponentsCompiling progress)
  drawState h progress linesRef cc

-- | Whenever the TVars `pComponentsCompiling` and `pComponentCompiledCount` change, we clear and redraw the output.
runner :: Handle -> Progress -> IO ()
runner h progress@Progress{..} = do
  linesRef <- newIORef (0 :: Int)

  let go lastVersion = do
        (version', cc') <- atomically $ do
          v  <- readTVar pVersion
          check (v /= lastVersion)
          cc <- readTVar pComponentsCompiling
          return (v, cc)
        drawState h progress linesRef cc'
        unless (completedCount cc' == pTotalComponentCount) $
          go version'

      teardown = readIORef linesRef >>= \n -> redraw h n []

      body = do
        cc0      <- readTVarIO pComponentsCompiling
        version0 <- readTVarIO pVersion
        drawState h progress linesRef cc0
        unless (completedCount cc0 == pTotalComponentCount) $
          go version0

  body `finally` teardown
