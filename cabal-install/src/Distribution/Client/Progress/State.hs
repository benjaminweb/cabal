module Distribution.Client.Progress.State where

import Distribution.Client.Progress.Types

data Progress =
     Progress { pComponentsCompiling    :: TVar ComponentsCompiling
              , pTotalComponentCount    :: Int
              , pVersion                :: TVar Int
              }

data ComponentsCompiling =
     ComponentsCompiling { ccComponentEvents :: M.Map Text ComponentEvent -- ^ Updated states of components gets overwritten, `CompletedCompilingComponent _` effects deletion (as the component is no longer being compiled).
                         } deriving Show

initProgress :: Int         -- ^ Total component count.
             -> IO Progress
initProgress pTotalComponentCount = do
  pComponentsCompiling    <- newTVarIO $ ComponentsCompiling M.empty
  pVersion                <- newTVarIO 0
  return $ Progress {..}

-- | Updates trackers for local and total progress.
makingProgress :: Progress -> GHCEvent -> STM ()
makingProgress Progress {..} ge@GHCEvent {..} = case geEvent of
                                    ComponentDone      -> modifyTVar' pComponentsCompiling (updateComponentsCompiling ge) >> modifyTVar' pVersion (+1)
                                    CompilingModule {} -> modifyTVar' pComponentsCompiling (updateComponentsCompiling ge) >> modifyTVar' pVersion (+1)

updateComponentsCompiling :: GHCEvent -> ComponentsCompiling -> ComponentsCompiling
updateComponentsCompiling GHCEvent {..} cc@ComponentsCompiling {..} =
  cc { ccComponentEvents = M.insert gePackage geEvent ccComponentEvents }
