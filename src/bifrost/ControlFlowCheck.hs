module ControlFlowCheck (controlFlowCheck) where

-- Check that the control flow of a StructuredProgram is valid; specifically
-- that it contains no subroutine call cycles, and that each subroutine's code
-- is guaranteed not to "fall off" the end of the subroutine (i.e., reach the
-- end without reaching a return statement). Assumes all subroutine references
-- are valid (see RefCheck.hs).

import qualified Data.Map as M
import qualified Data.Set as S

import Types
import Message
import Expression
import StructuredProgram
import Compile
import PrettyPrint
import Graph

controlFlowCheck :: StructuredProgram -> Compile StructuredProgram
controlFlowCheck prog = mkCompile $ \env ->
  let callGraph = makeCallGraph prog
      cycles = findSomeCycles callGraph
      noRetSubs = findNoRetSubs prog
      errors = map errCycle cycles ++ map errNoRet noRetSubs
  in (errors, case errors of
    [] -> Just (env, prog)
    _ -> Nothing)

-- Create a graph of calls between subroutines (including self-calls).
makeCallGraph :: StructuredProgram -> Graph SubroutineName
makeCallGraph prog = M.map findCalls $ strp_subs prog

-- All subroutines called from a statement.
findCalls :: Stm -> S.Set SubroutineName
findCalls stm =
  case stm of
    Do (Assign _ rhs) -> foo [rhs] []
    Do Scissors -> none
    Do (HintPower _ _) -> none
    Nop -> none
    Seq s1 s2 -> foo [] [s1,s2]
    Label _ -> none
    Goto _ -> none
    Return -> none
    Branch e s1 s2 -> foo [e] [s1,s2]
    While e s -> foo [e] [s]
    For s1 e s2 s3 -> foo [e] [s1,s2,s3]
    ForEach _ e1 e2 s -> foo [e1, e2] [s]
  where
    none = S.empty
    foo exps stms =
      S.unions $ (map expCalls exps) ++ (map findCalls stms)

-- All subroutines called from an expression.
expCalls :: Exp -> S.Set SubroutineName
expCalls exp =
  case getInner exp of
    ESubCall name _ -> S.singleton name
    _ -> S.unions $ map expCalls $ children exp

-- Subroutines where control flow might pass through without a return statement.
findNoRetSubs :: StructuredProgram -> [SubroutineName]
findNoRetSubs prog =
  let allSubs = M.toList $ strp_subs prog
  in map fst $ filter (canPassThrough . snd) allSubs

-- Can control flow pass through to what comes after a statement? (Errs on the
-- side of "yes", but ending with a Return guarantees "no".)
canPassThrough :: Stm -> Bool
canPassThrough stm =
  case stm of
    Seq s1 s2 -> all canPassThrough [s1,s2]
    Return -> False
    Branch _ s1 s2 -> any canPassThrough [s1,s2]
    _ -> True



errCycle :: [SubroutineName] -> Message
errCycle members = Error $ "call cycle detected: " ++
  case members of
    (sub:subs) -> joinWith " -> " (map show $ members ++ [sub])
    [] -> error "internal error: empty cycle"

errNoRet :: SubroutineName -> Message
errNoRet sub = Error $
  "subroutine " ++ show sub ++ " contains code-paths that don't end in return"
