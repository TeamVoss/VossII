module TypecheckStructuredProgram (typecheckStructuredProgram) where

import Control.Monad (foldM, liftM, mapM, when)
import Control.Monad.Writer (Writer, runWriter, tell)
import Data.Either
import Data.Maybe (isNothing)
import qualified Data.Map as M
import Types
import Message
import Compile
import Expression
import Environment
import PrettyPrint
import Typecheck

typecheckStructuredProgram :: StructuredProgram -> Compile StructuredProgram
typecheckStructuredProgram prog = mkCompile $ typecheckStructuredProgram' prog

typecheckStructuredProgram'
  :: StructuredProgram
  -> Environment
  -> ([Message], Maybe (Environment, StructuredProgram))
typecheckStructuredProgram' prog env =
  case errors of
    [] -> (msgs, Just (env, prog'))
    _ -> (msgs, Nothing)
  where
    errors = filter isError msgs
    (prog', msgs) = runWriter (checkStructuredProgram' env prog)

type W = Writer [Message]

checkStructuredProgram'
  :: Environment
  -> StructuredProgram
  -> W StructuredProgram
checkStructuredProgram' env prog = do
  subs' <- liftM M.fromList $ mapM foo $ M.toList $ strp_subs prog
  return $ prog { strp_subs = subs' }
  where
    foo (sub, stm) = do
      stm' <- checkStm env sub stm
      return (sub, stm')

checkStm :: Environment -> SubroutineName -> Stm -> W Stm
checkStm env sub stm =
  case stm of
    Do astm -> do
      stm' <- checkAtomicStm env sub astm
      return stm'
    Nop -> fine
    Seq stm1 stm2 -> do
      stm1' <- checkStm env sub stm1
      stm2' <- checkStm env sub stm2
      return $ Seq stm1' stm2'
    Label _ -> fine
    Goto _ -> fine
    Return -> fine
    Branch exp stm1 stm2 -> do
      mexp' <- doTC env sub $ check exp bit
      let exp' = maybe exp id mexp'
      stm1' <- checkStm env sub stm1
      stm2' <- checkStm env sub stm2
      return $ Branch exp' stm1' stm2'
    While exp stm1 -> do
      mexp' <- doTC env sub $ check exp bit
      let exp' = maybe exp id mexp'
      stm1' <- checkStm env sub stm1
      return $ While exp' stm1'
    For stm1 exp stm2 stm3 -> do
      mexp' <- doTC env sub $ check exp bit
      let exp' = maybe exp id mexp'
      stm1' <- checkStm env sub stm1
      stm2' <- checkStm env sub stm2
      stm3' <- checkStm env sub stm3
      return $ For stm1' exp' stm2' stm3'
    ForEach v exp1 exp2 stm1 -> do
      mt <- doTC env sub $ lookupVarType v
      let t = maybe bit id mt
      mexp1' <- doTC env sub $ check exp1 t
      let exp1' = maybe exp1 id mexp1'
      mexp2' <- doTC env sub $ check exp2 t
      let exp2' = maybe exp2 id mexp2'
      stm1' <- checkStm env sub stm1
      return $ ForEach v exp1' exp2' stm1'
    Sandwich stm1 exp stm2 -> do
      mexp' <- doTC env sub $ check exp bit
      let exp' = maybe exp id mexp'
      stm1' <- checkStm env sub stm1
      stm2' <- checkStm env sub stm2
      return $ Sandwich stm1' exp' stm2'
  where
    fine = return stm

checkAtomicStm :: Environment -> SubroutineName -> AtomicStm -> W Stm
checkAtomicStm env sub astm =
  case astm of
    Assign lhs rhs -> do
      mrhs' <- doTC env sub $ checkAssign lhs rhs
      case mrhs' of
        Just rhs' -> return $ Do $ Assign lhs rhs'
        Nothing -> return $ Do astm  -- error logged, return original
    Scissors -> return $ Do astm
    HintPower action status -> do
      let (Just ai) = findActionInfo env action
      let proto = ai_protocol ai
      if proto_power proto == AlwaysOn
        then do
          logMsg sub $ Warning $
            "Action " ++ show action ++ " does not have power management, " ++
            "power '" ++ show status ++ "' hint ignored."
          return Nop
        else
          return $ Do astm

-- Run a TC computation. If it produces an error, log it and return Nothing.
-- Otherwise return the result.
doTC :: Environment -> SubroutineName -> TC a -> W (Maybe a)
doTC env sub tc =
  case runTC tc env of
    Left err -> do
      logMsg sub err
      return Nothing
    Right value -> return $ Just value

-- Log an error.
logMsg :: SubroutineName -> Message -> W ()
logMsg sub err = tell [mapMessage lbl lbl err]
  where
    lbl = ((sub ++ ": ") ++)

