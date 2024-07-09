{-
    This module fixes the state interaction stuff to reflect the fact that if
    action X (e.g. mem_write) is shared with action Y (e.g. arith_unit), then
    the latter should inherit the state interaction aspects of the former (e.g.
    reading [MEM] writing [MEM]).

    One option was to require that if X is shared with Y, then Y's declaration
    must include all of the state interaction that is mentioned in X's
    declaration. Then only a simple check would need to be performed here.
    However, this leads to repetition.

    Instead, we opt to propagate X's state interaction to Y. So Y ends up with
    both the state interaction it was originally declared with *and* the state
    interaction that X performs.

    Note that we can have a chain of sharing, where X is shared with Y, and Y
    is shared with Z. Proper ordering should ensure that the state interaction
    of Z correctly ends up including both that of X and that of Y (or, more
    precisely, Y is updated to include X, then Z is updated to include this new
    version of Y).
-}
module FixSharedActions (fixSharedActions) where

import Data.Maybe (fromJust)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.List (nub)

import Types
import Environment
import Message
import Compile
import PrettyPrint

-- TODO PREVENT DUPLICATE SHARES
fixSharedActions :: StructuredProgram -> Compile StructuredProgram
fixSharedActions prog = mkCompile $ \env -> ([], Just (propagateSharedActions env, prog))

propagateSharedActions :: Environment -> Environment
propagateSharedActions env =
  let actions = orderRelevantActions env
      env' = foldr propagate env actions
      -- dbg = unlines $ ("\nDEBUG" :) $ map (\(a,ai) -> a ++ " : " ++ show (ai_type ai)) $ M.toList $ env_actionInfos env'
  in env'

-- If action X is shared with action Y, then:
-- (1) Y is in the resulting list, and
-- (2) if X is in the result list then it comes after Y
-- Assumes there are no cycles!
orderRelevantActions :: Environment -> [ActionName]
orderRelevantActions env = foldr (explore $ length actions) [] actions
  where
    explore :: Int -> ActionName -> [ActionName] -> [ActionName]
    explore depth action output =
      if depth < 0
        then error $ "Action sharing cycle detected involving action " ++ show action
        else if action `elem` output || null (deps action)
          then output
          else action:(foldr (explore (depth - 1)) (deps action) output)
    actions :: [ActionName]
    actions = env_actions env
    deps :: ActionName -> [ActionName]
    deps = getShared env

-- Get all actions X shared with a given action Y.
getShared :: Environment -> ActionName -> [ActionName]
getShared env action =
  case ai_provider $ mustFindActionInfo env action of
    External -> []
    Instantiate _ as -> as

-- Propagate the state interaction from (all X shared with Y) into Y itself.
propagate :: ActionName -> Environment -> Environment
propagate action env =
  let shared = getShared env action
      effects = map (getEffect env) (action:shared)
      reads = effects >>= asi_stateRead
      writes = effects >>= asi_stateWrite
      readsUniq = nub reads
      writesUniq = nub writes
      effect' = ActionStateInteraction readsUniq writesUniq
  in setEffect action effect' env

-- Get the state interaction of a given action.
getEffect :: Environment -> ActionName -> ActionStateInteraction
getEffect env action = at_state $ ai_type $ fromJust $ M.lookup action $ env_actionInfos env

-- Update the state interaction of a given action.
setEffect :: ActionName -> ActionStateInteraction -> Environment -> Environment
setEffect action effect env = env { env_actionInfos = M.adjust foo action $ env_actionInfos env }
  where
    foo :: ActionInfo -> ActionInfo
    foo ai =
      let typ = ai_type ai
          typ' = typ { at_state = effect }
      in ai { ai_type = typ' }
