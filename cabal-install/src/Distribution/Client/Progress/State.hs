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

data ComponentsCompiling = ComponentsCompiling
  { ccComponentEvents :: M.Map PackageIdentifier ComponentEvent -- ^ Updated states of components gets overwritten, `CompletedCompilingComponent _` effects deletion (as the component is no longer being compiled).
  , ccModuleCounts    :: M.Map PackageIdentifier Int  -- ^ Tracks the total modules for each package
  } deriving (Show, Eq)

initProgress :: Int -> IO Progress
initProgress pTotalComponentCount = do
  pComponentsCompiling <- newTVarIO $ ComponentsCompiling M.empty M.empty
  pVersion             <- newTVarIO 0
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

rank :: ComponentEvent -> Int
rank Downloading               = 1
rank Downloaded                = 2
rank Configuring               = 3
rank Building                  = 4
rank (CompilingModule _ _ _)   = 5
rank Installing                = 6
rank ComponentDone             = 7
rank (ComponentFailed _)       = 8

updateComponentsCompiling :: ProgressEvent -> ComponentsCompiling -> ComponentsCompiling
updateComponentsCompiling ge@ProgressEvent{..} cc@ComponentsCompiling{..} =
  case M.lookup pePackageId ccComponentEvents of
    Just old | rank old > rank peEvent -> cc
    _ ->
      let newMods = case peEvent of
            CompilingModule _ _ total -> M.insert pePackageId total ccModuleCounts
            _                         -> ccModuleCounts
      in cc { ccComponentEvents = M.insert pePackageId peEvent ccComponentEvents
            , ccModuleCounts    = newMods
            }

completedCount :: ComponentsCompiling -> Int
completedCount cc = 
  M.size $ M.filter isDone (ccComponentEvents cc)
  where
    isDone ComponentDone       = True
    isDone (ComponentFailed _) = True
    isDone _                   = False
