module Distribution.Client.Progress.Runner (runner) where

import Distribution.Client.Progress.State (Progress, ComponentsCompiling(..), completedCount)
import Distribution.Client.Progress.Render
import Distribution.Client.Progress.Types

-- | Whenever the TVars `pComponentsCompiling` and `pComponentCompiledCount` change, we clear and redraw the output.
runner :: Progress
       -> IO ()
runner Progress {..} = do
  cc                <- readTVarIO pComponentsCompiling
  firstVersion      <- readTVarIO pVersion
  let startLines    = [totalLine defaultColumnWidths (completedCount cc) pTotalComponentCount] ++ componentLines (M.assocs (ccComponentEvents cc))
  redraw 0 startLines
  go firstVersion (length startLines)
    where
      go lastVersion lastLinesCount = do
        (version', cc', doneCount') <- atomically $ do
                              currentVersion <- readTVar pVersion
                              check (currentVersion /= lastVersion)
                              cc'        <-  readTVar pComponentsCompiling
                              return (currentVersion, cc', completedCount cc')
        let newLines = [totalLine defaultColumnWidths doneCount' pTotalComponentCount] ++ componentLines (M.assocs (ccComponentEvents cc'))
        redraw lastLinesCount newLines
        unless (doneCount' == pTotalComponentCount) $ go version' (length newLines)
