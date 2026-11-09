module Distribution.Client.Progress.Types where

import Data.Text (Text)

{-
Note [ComponentEvent]
˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜
 - 1..11 out of 12 done have all one currently compiling module => we want to display it as a line
 - 12 out of 12 done has none currently compiling module (as the component's compilation has completed) => we don't want to display it as line
 - hence: `CompilingModule` should never have `ceDoneCount` to become `ceTotalCount` (as there is no module currently being compiled which we'd need) => use ComponentDone
-}
data ComponentEvent = CompilingModule -- ^ When next module is being compiled, `ceModuleName` changes and `ceDoneCount` increments;
                                      --   when `ceDoneCount` becomes equal to `ceTotalCount` there is no `ceModuleName` hence we use `ComponentDone`.
                    { ceModuleName    :: Text  -- ^ Carries module name where compilation has been started.
                    , ceDoneCount     :: Int   -- ^ Modules compiled (excluding `ceModuleName`).
                    , ceTotalCount    :: Int   -- ^ Total module count to be compiled.
                    }
                    | ComponentDone
                    -- TODO: implement ComponentFailed
                    deriving (Show, Eq)

{-
Note [Two cases to model]
˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜˜
1. GHC outputs "[1 of 3] Compiling Module.Name" => GHCEvent "package-name" (Increment $ GHCLine 1 3 "Some.Fancy.Module")
2. GHC exits gracefully => GHCEvent "package-name" Done

We need the second case as GHC does not report explicitly when the last module has completed compiling.
-}

data GHCEvent =
     GHCEvent { gePackage :: Text -- ^ Component's name.
              , geEvent   :: ComponentEvent
              } deriving (Show, Eq)
