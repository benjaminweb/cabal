{-# LANGUAGE RecordWildCards #-}
module Distribution.Client.Progress.Runner (runner) where

import qualified Data.Map.Strict as M
import Control.Monad               (unless)
import Control.Concurrent.STM      (atomically, check, readTVar)
import Control.Concurrent.STM.TVar (readTVar, readTVarIO)
import Control.Exception           (finally)
import Data.IORef
import System.IO (hPutStrLn, stderr)

import Distribution.Types.Progress
import Distribution.Client.Progress.State (Progress(..), ComponentsCompiling(..), completedCount)
import Distribution.Client.Progress.Render

-- | Whenever the TVars `pComponentsCompiling` and `pComponentCompiledCount` change, we clear and redraw the output.
runner :: Progress -> IO ()
runner progress@Progress{..} = do
  linesRef <- newIORef (0 :: Int)
  let isCompiling (CompilingModule _ _ _) = True
      isCompiling _                       = False

  let draw cc doneCount = do
        let events = M.elems (ccComponentEvents cc)
            active = length (filter isCompiling events)
            done   = length events - active
            newLines = totalLine defaultColumnWidths doneCount pTotalComponentCount
                : componentLines (M.assocs (ccComponentEvents cc))
        -- hPutStrLn stderr ("[draw] active=" ++ show active ++ " done=" ++ show done ++ " lines=" ++ show (length newLines))
        prev <- readIORef linesRef
        redraw prev newLines
        writeIORef linesRef (length newLines)

      go lastVersion = do
        (version', cc', done') <- atomically $ do
          v  <- readTVar pVersion
          check (v /= lastVersion)
          cc <- readTVar pComponentsCompiling
          return (v, cc, completedCount cc)
        -- hPutStrLn stderr ("[map] " ++ show (M.toList (ccComponentEvents cc')))  -- ← add this
        draw cc' done'
        unless (done' == pTotalComponentCount) $
          go version'

      teardown = readIORef linesRef >>= \n -> redraw n []

      body = do
        cc0      <- readTVarIO pComponentsCompiling
        version0 <- readTVarIO pVersion
        let done0 = completedCount cc0
        draw cc0 done0
        unless (done0 == pTotalComponentCount) $ go version0

  body `finally` teardown
