module RefCheck (refCheck) where

-- Checks that references in a StructuralProgram are valid. Specifically:
-- - The same goto-label isn't declared twice in one subroutine.
-- - Every goto's destination exists.
-- - Every called subroutine exists.
-- - Every called action exists. (ReadAST should already enforce this.)
-- - The main subroutine exists. (ReadAST should already enforce this.)
-- - Every referenced variable exists. (ReadAST should already enforce this.)
-- - Every referenced named type exists. (ReadAST should already enforce this.)
--
-- Note that variable fields are not checked for validity; this is left to the
-- typechecker.

import Control.Monad
import Data.Maybe
import qualified Data.Map as M
import qualified Data.Set as S

import Types
import Expression
import Environment
import StructuredProgram
import Message
import Compile
import KitchenSink


refCheck :: StructuredProgram -> Compile StructuredProgram
refCheck prog = mkCompile $ \env ->
  let (mres, msgs, _) = runKitchenSink refCheck' (RCState env prog S.empty)
      errors = filter isError msgs
  in (msgs, case (errors, mres) of
    ([], Just ()) -> Just (env, prog)
    _ -> Nothing)

type RC = KitchenSink Message RCState
data RCState = RCState
  { rcs_env :: Environment
  , rcs_prog :: StructuredProgram
  , rcs_labels :: S.Set (SubroutineName, GotoLabel)
  }

refCheck' :: RC ()
refCheck' = do
  buildLabelSet
  prog <- getProg
  checkSubExists $ strp_main prog
  mapM_ checkSub $ M.toList $ strp_subs prog

buildLabelSet :: RC ()
buildLabelSet = do
  prog <- getProg
  mapM_ buildLabelSetForSub $ M.toList $ strp_subs prog

buildLabelSetForSub :: (SubroutineName, Stm) -> RC ()
buildLabelSetForSub (sub, body) = explore body
  where
    explore stm =
      case stm of
        Label label -> addLabel (sub, label)
        _ -> mapM_ explore (subStms stm)

addLabel :: (SubroutineName, GotoLabel) -> RC ()
addLabel sl@(sub, label) = do
  st <- get
  let labels = rcs_labels st
  when (sl `elem` labels) $ complain $ errRedeclare "label" label (Just sub)
  let labels' = S.insert sl labels
  put $ st { rcs_labels = labels' }

checkSub :: (SubroutineName, Stm) -> RC ()
checkSub (sub, body) = explore body
  where
    explore stm =
      case stm of
        Do (Assign lhs rhs) -> do
          checkLhs sub lhs
          checkExp sub rhs
        Do Scissors -> done
        Do (HintPower action _) -> checkActionExists action
        Nop -> done
        Seq s1 s2 -> do
          explore s1
          explore s2
        Label _ -> done
        Goto label -> checkGotoLabelExists sub label
        Return -> done
        Branch e s1 s2 -> do
          checkExp sub e
          explore s1
          explore s2
        While e s -> do
          checkExp sub e
          explore s
        For s1 e s2 s3 -> do
          explore s1
          checkExp sub e
          explore s2
          explore s3
        ForEach v e1 e2 s -> do
          checkVarExists v
          checkExp sub e1
          checkExp sub e2
          explore s
    done = return ()

checkLhs :: SubroutineName -> Lhs -> RC ()
checkLhs sub lhs =
  case lhs of
    LhsVarField v _ -> checkVarExists v  -- note: fields not checked
    LhsTuple lhss -> mapM_ (checkLhs sub) lhss
    LhsIgnore -> return ()

checkExp :: SubroutineName -> Exp -> RC ()
checkExp sub exp =
  case getInner exp of
    EVar v -> checkVarExists v
    ETypeAnn t _ -> do
      checkType t
      recurse
    EActionCall name _ -> do
      checkActionExists name
      recurse
    ESubCall name _ -> do
      checkSubExists name
      recurse
    _ -> recurse
  where
    recurse = mapM_ (checkExp sub) $ children exp

checkType :: Type -> RC ()
checkType typ =
  case typ of
    NamedType name -> checkTypeExists name
    Product ts -> mapM_ checkType ts
    Fun t1 t2 -> do
      checkType t1
      checkType t2

checkActionExists :: ActionName -> RC ()
checkActionExists name = do
  env <- getEnv
  assert (isJust $ findActionInfo env name) $ errNo "action" name Nothing

checkGotoLabelExists :: SubroutineName -> GotoLabel -> RC ()
checkGotoLabelExists sub label = do
  labels <- getLabels
  assert ((sub, label) `elem` labels) $ errNo "goto label" label (Just sub)

checkSubExists :: SubroutineName -> RC ()
checkSubExists name = do
  env <- getEnv
  assert (isJust $ findSubroutineType env name) $ errNo "subroutine" name Nothing

checkVarExists :: Var -> RC ()
checkVarExists var = do
  env <- getEnv
  assert (isJust $ findVarType env var) $ errNo "variable" var Nothing

checkTypeExists :: TypeName -> RC ()
checkTypeExists name = do
  env <- getEnv
  assert (isJust $ findType env name) $ errNo "type" name Nothing

getEnv :: RC Environment
getEnv = liftM rcs_env get

getProg :: RC StructuredProgram
getProg = liftM rcs_prog get

getLabels :: RC (S.Set (SubroutineName, GotoLabel))
getLabels = liftM rcs_labels get

assert :: Bool -> Message -> RC ()
assert condition msg = do
  when (not condition) $ complain msg

complain :: Message -> RC ()
complain msg = logMessages [msg]

errNo :: Show a => String -> a -> Maybe SubroutineName -> Message
errNo variety thing mloc = Error $ unwords $
  [variety, show thing, "doesn't exist"] ++ loc
  where
    loc = maybe [] (\sub -> ["in subroutine " ++ show sub]) mloc

errRedeclare :: Show a => String -> a -> Maybe SubroutineName -> Message
errRedeclare variety thing mloc = Error $ unwords $
  [variety, show thing, "redeclared"] ++ loc
  where
    loc = maybe [] (\sub -> ["in subroutine " ++ show sub]) mloc
