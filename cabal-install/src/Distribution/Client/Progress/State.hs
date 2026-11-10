{-# LANGUAGE RecordWildCards #-}
module Distribution.Client.Progress.State where

import Distribution.Types.Progress
import Distribution.Types.PackageId (PackageIdentifier)

import qualified Data.Map.Strict as M
import Control.Concurrent.STM (STM, TVar, modifyTVar', newTVarIO)

data Progress =
     Progress { pComponentsCompiling    :: TVar ComponentsCompiling
              , pTotalComponentCount    :: Int
              , pVersion                :: TVar Int
              }

data ComponentsCompiling =
     ComponentsCompiling { ccComponentEvents :: M.Map PackageIdentifier ComponentEvent -- ^ Updated states of components gets overwritten, `CompletedCompilingComponent _` effects deletion (as the component is no longer being compiled).
                         } deriving Show

initProgress :: Int         -- ^ Total component count.
             -> IO Progress
initProgress pTotalComponentCount = do
  pComponentsCompiling    <- newTVarIO $ ComponentsCompiling M.empty
  pVersion                <- newTVarIO 0
  return $ Progress {..}

-- | Updates trackers for local and total progress.
makingProgress :: Progress -> ProgressEvent -> STM ()
makingProgress Progress{..} ge = do
  modifyTVar' pComponentsCompiling $ \cc ->
    case M.lookup (pePackageId ge) (ccComponentEvents cc) of
      Just ComponentDone       | not (isTerminal (peEvent ge)) -> cc
      Just (ComponentFailed _) | not (isTerminal (peEvent ge)) -> cc
      _ -> updateComponentsCompiling ge cc
  modifyTVar' pVersion (+ 1)
  where
    isTerminal ComponentDone       = True
    isTerminal (ComponentFailed _) = True
    isTerminal _                   = False

updateComponentsCompiling :: ProgressEvent -> ComponentsCompiling -> ComponentsCompiling
updateComponentsCompiling ProgressEvent {..} cc@ComponentsCompiling {..} =
  cc { ccComponentEvents = M.insert pePackageId peEvent ccComponentEvents }

completedCount :: ComponentsCompiling -> Int
completedCount (ComponentsCompiling m) =
  M.size $ M.filter isDone m
  where
    isDone ComponentDone       = True
    isDone (ComponentFailed _) = True
    isDone _                   = False
