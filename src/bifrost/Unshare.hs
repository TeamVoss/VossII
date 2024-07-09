module Unshare (unshare) where

-- Inline shared expressions that are used exactly once.

import Control.Monad
import Control.Monad.State
import qualified Data.Map as M
import qualified Data.Set as S

import Types
import Expression
import Compile

unshare :: SyncProgram -> Compile SyncProgram
unshare prog = mkCompile $ \env ->
  let shared = bitp_shared prog
      zero = M.map (\_ -> 0) shared
      countMap = execState (explore prog) zero
      toInline = M.keysSet $ M.filter (== 1) countMap
      (inline, shared') = M.partitionWithKey
        (\i _ -> i `elem` toInline) shared
      activities' = M.map (inlineActivity inline) $ bitp_activities prog
      shared'' = M.map (inlineExp inline) shared'
      prog' = prog { bitp_activities = activities', bitp_shared = shared'' }
  in ([], Just (env, prog'))

----------------------------------------
-- Reference counting
----------------------------------------

type U = State (M.Map SharedRef Integer)

explore :: SyncProgram -> U ()
explore prog = do
  mapM_ exploreExp $ bitp_shared prog
  mapM_ exploreActivity $ bitp_activities prog

exploreActivity :: Activity -> U ()
exploreActivity activity = do
  mapM_ exploreExp $ actv_updates activity
  mapM_ exploreCallTree $ actv_actions activity
  mapM_ explorePowerTree $ actv_powers activity
  exploreExp $ actv_next activity
  where
    exploreCallTree tree =
      case tree of
        ADepends e t1 t2 -> do
          exploreExp e
          exploreCallTree t1
          exploreCallTree t2
        ACall args -> mapM_ exploreExp args
        ADontCall -> return ()
    explorePowerTree tree =
      case tree of
        PowDepends e t1 t2 -> do
          exploreExp e
          explorePowerTree t1
          explorePowerTree t2
        PowSet _ -> return ()
        PowDontSet -> return ()

exploreExp :: Exp -> U ()
exploreExp exp =
  case getInner exp of
    EShared index -> increment index
    _ -> mapM_ exploreExp $ children exp

increment :: SharedRef -> U ()
increment index = modify $ M.adjust (+ 1) index


----------------------------------------
-- Inlining
----------------------------------------

inlineActivity :: M.Map SharedRef Exp -> Activity -> Activity
inlineActivity inline activity =
  let updates' = M.map ie $ actv_updates activity
      actions' = M.map it $ actv_actions activity
      powers' = actv_powers activity
      next' = ie $ actv_next activity
  in Activity updates' actions' powers' next'
  where
    ie = inlineExp inline
    it tree =
      case tree of
        ADepends e t1 t2 -> ADepends (ie e) (it t1) (it t2)
        ACall args -> ACall (map ie args)
        ADontCall -> ADontCall

inlineExp :: M.Map SharedRef Exp -> Exp -> Exp
inlineExp inline = foo
  where
    foo exp =
      case getInner exp of
        EShared index ->
          case M.lookup index inline of
            Just exp' -> foo exp'
            Nothing -> exp
        _ -> uponChildren foo exp
