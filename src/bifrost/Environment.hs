module Environment where

{-
  Helpful things for environments and variables.
-}

import Control.Monad (liftM)
import Data.List (sort, group)
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Set as S
import Types
import PrettyPrint

----------------------------------------
-- Lookups
----------------------------------------

existsStateAspect :: Environment -> GlobalStateAspect -> Bool
existsStateAspect env aspect = aspect `elem` (env_stateAspects env)

existsType :: Environment -> TypeName -> Bool
existsType env tn = isJust $ findType env tn

existsVar :: Environment -> Var -> Bool
existsVar env var = isJust $ findVarType env var

findActionInfo :: Environment -> ActionName -> Maybe ActionInfo
findActionInfo env act = M.lookup act $ env_actionInfos env

findStruct :: Environment -> TypeName -> Maybe Struct
findStruct env tn = M.lookup tn $ env_structs env

findSubroutineType :: Environment -> SubroutineName -> Maybe SubroutineType
findSubroutineType env sub = M.lookup sub $ env_subTypes env

findType :: Environment -> TypeName -> Maybe (Maybe String)
findType env tn = M.lookup tn $ env_types env

findVarInfo :: Environment -> Var -> Maybe VarInfo
findVarInfo env v = M.lookup v $ env_vars env

findVarType :: Environment -> Var -> Maybe Type
findVarType env v = liftM vi_type $ findVarInfo env v

mustFindActionInfo :: Environment -> ActionName -> ActionInfo
mustFindActionInfo env meth =
  case findActionInfo env meth of
    Just mi -> mi
    _ -> error $ "Could not find action named: " ++ show meth

mustFindStruct :: Environment -> TypeName -> Struct
mustFindStruct env tn =
  case findStruct env tn of
    Just s -> s
    _ -> error $ "Could not find struct info for type: " ++ show tn

mustFindSubroutineType :: Environment -> SubroutineName -> SubroutineType
mustFindSubroutineType env sub =
  case findSubroutineType env sub of
    Just st -> st
    _ -> error $ "Could not find type of subroutine: " ++ show sub

mustFindVarInfo :: Environment -> Var -> VarInfo
mustFindVarInfo env v =
  case findVarInfo env v of
    Just vi -> vi
    _ -> error $ "Could not find variable: " ++ printVar v

mustFindVarType :: Environment -> Var -> Type
mustFindVarType env v =
  case findVarType env v of
    Just t -> t
    _ -> error $ "Could not find type of variable: " ++ printVar v

----------------------------------------
-- More involved
----------------------------------------

{-
getLhsType :: Environment -> Lhs -> Type
getLhsType env lhs =
  case lhs of
    LhsVarField v [] ->
      let vi = mustFindVarInfo env v
      in Atomic $ vi_type vi
    LhsVarField v _ -> dontknow
    LhsTuple lhss ->
      let ts = map (getLhsType env) lhss
      in Product ts
    LhsIgnore -> dontknow
  where
    dontknow = Atomic Inferred
-}

----------------------------------------
-- Construction/modification
----------------------------------------

emptyEnvironment :: Environment
emptyEnvironment =
  Environment "" ProtocolAuto M.empty M.empty M.empty M.empty S.empty []
    M.empty M.empty M.empty Nothing

-- Add a variable to an environment, raise error if it already exists.
addVar :: Var -> VarInfo -> Environment -> Environment
addVar v vi env =
  let vars = env_vars env
  in case M.lookup v vars of
    Just _ -> error $ "Variable " ++ printVar v ++ " already exists in environment"
    Nothing -> env { env_vars = M.insert v vi vars }

-- Create a fresh variable.
allocateMiscVar :: Type -> Environment -> (Var, Environment)
allocateMiscVar typ env =
  let vars = M.keys $ env_vars env
      index = foldr max 0 $ vars >>= extract
      extract v =
        case v of
          MiscVar i -> [i+1]
          _ -> []
      var = MiscVar index
      info = VarInfo typ []
  in (var, addVar var info env)

----------------------------------------
-- Misc
----------------------------------------

-- How individual actions from an action array should be named.
actionArrayName :: ActionName -> Integer -> ActionName
actionArrayName name index = name ++ show index

-- Turn array actions into individual actions, and return a map from
-- original action names to their expanded counterparts.
expandActionArrays :: Environment ->
  (Environment, M.Map ActionName [ActionName])
expandActionArrays env =
  if null duplicates
     then (env', expansions)
     else error $ "Duplicate action names when expanding action arrays: " ++
       show duplicates
  where
    duplicates = map (!! 0) $ filter ((>= 2) . length) $ group $ sort actions'
    expansions = M.fromList $ expanded >>=
      \(b, name, pairs) -> if b then [(name, map fst pairs)] else []
    env' = env { env_actions = actions', env_actionInfos = actionInfos' }
    actions' = expanded >>= \(_,_,pairs) -> map fst pairs
    actionInfos' = M.fromList $ expanded >>= \(_,_,pairs) -> pairs
    expanded = map expand (env_actions env)
    expand :: ActionName -> (Bool, ActionName, [(ActionName, ActionInfo)])
    expand name =
      let ai = mustFindActionInfo env name
          count = ai_array ai
          ai' = ai { ai_array = 0 }
          names = map (actionArrayName name) [0..count-1]
      in if count == 0
        then (False, name, [(name, ai)])
        else (True, name, map (\n -> (n, ai')) names)

