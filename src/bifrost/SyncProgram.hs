module SyncProgram where

import qualified Data.Map as M
import Types
import PrettyPrint

findActivity :: SyncProgram -> ActivityLabel -> Maybe Activity
findActivity prog label = M.lookup label $ bitp_activities prog

mustFindActivity :: SyncProgram -> ActivityLabel -> Activity
mustFindActivity prog label =
  case findActivity prog label of
    Just activity -> activity
    Nothing -> error $ "Could not find activity: " ++ show label
