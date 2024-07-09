module Finite (analyzeFinite, isTrivialConstant) where

-- Analyze a SyncProgram and determine what values are possible for each
-- variable, where (easily) possible. Throws hands in the air if a variable
-- looks like it might take on anything other than a handful of constant values.
-- The primary utility of this module is in untangling control flow via return
-- addresses.

import Control.Monad
import Control.Monad.State
import Control.Monad.Writer
import qualified Data.Map as M
import qualified Data.Set as S

import Types
import Expression
import PrettyPrint

-- A formal definition of trivially constant values, for reference.
isTrivialConstant :: Exp -> Bool
isTrivialConstant exp =
  case getInner exp of
    EInt _ -> True
    EActivityLabel _ -> True
    _ -> False

-- Map variables to the finite sets of constant expressions (in the
-- isTrivialConstant sense) they can take on. Variables for which this is not
-- possible are not included in the map.
analyzeFinite :: SyncProgram -> Environment -> M.Map Var (S.Set Exp)
analyzeFinite prog env =
  let vars = M.keys $ env_vars env
      initial = M.fromList $ map (\v -> (v, S.empty)) vars
      analyze = mapM_ analyzeActivity $ M.elems $ bitp_activities prog
  in execState analyze initial

type Work = State (M.Map Var (S.Set Exp)) ()

analyzeActivity :: Activity -> Work
analyzeActivity activity = mapM_ foo $ M.toList $ actv_updates activity
  where
    foo (var, exp) =
      case getNewValues var exp of
        Just valueSet -> addValues var valueSet
        Nothing -> invalidate var

-- Look at a variable update and determine the *new* trivially constant values
-- it may take on. This does not include its *old* value, which can remain if
-- the update is e.g. x := if cond then 123 else x.
getNewValues :: Var -> Exp -> Maybe (S.Set Exp)
getNewValues var = foo
  where
    foo exp =
      case getInner exp of
        EVar v -> if v == var then Just S.empty else Nothing
        EInt _ -> return $ S.singleton exp
        EActivityLabel _ -> return $ S.singleton exp
        EIfThenElse _ e1 e2 -> do
          vs1 <- foo e1
          vs2 <- foo e2
          return $ vs1 `S.union` vs2
        _ -> Nothing

-- Add values to the set of values a variable can take on.
addValues :: Var -> S.Set Exp -> Work
addValues var vs = do
  m <- get
  case M.lookup var m of
    Just values ->
      let values' = vs `S.union` values
      in put $ M.insert var values' m
    Nothing ->
      return ()  -- variable has been invalidated, so do nothing

-- Indicate that we can't know the set of values a variable can take on.
invalidate :: Var -> Work
invalidate var = modify (M.delete var)
