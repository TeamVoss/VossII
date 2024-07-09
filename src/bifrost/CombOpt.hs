module CombOpt where

{-

Some not-completely-braindead manipulation of expressions. Assumes that they are
both COMBINATIONAL and WELL-TYPED. Also assumes any types supplied as arguments
are appropriate given the types of other arguments.

-}

import Data.List (nub)
import qualified Data.Map as M

import Types
import Expression
import ExpressionExtra
import PrettyPrint

-- "Bake" any remaining don't-cares into zero.
bake :: Exp -> Exp
bake exp =
  case getInner exp of
    EDontCare -> zero (typeOf exp)
    _ -> uponChildren bake exp

-- Apply a few rules to recursively simplify an expression.
simplify :: Exp -> Exp
simplify exp =
  let exp' = uponChildren simplify exp
      typ = typeOf exp'
  in case getInner exp' of
    EIfThenElse e1 e2 e3 -> optIfThenElse typ e1 e2 e3
    EOpApp Not [e] -> optNot e
    EOpApp Or [e1,e2] -> optOr typ [e1,e2]
    EOpApp And [e1,e2] -> optAnd typ [e1,e2]
    EOpApp Xor [e1,e2] -> optXor typ [e1,e2]
    EProj i e -> optProj typ i e
    _ -> exp'

-- Given mutually-exclusive (condition, value) pairs, produce an expression that
-- satisfies those and is zero if no condition holds. As usual all conditions
-- should be of type bit.
mutexConds :: Type -> [(Exp, Exp)] -> Exp
mutexConds typ pairs = foldr (\(c,v) other -> optIfThenElse typ c v other) (zero typ) pairs
{-
mutexConds typ pairs =
  let pairsNoDC = filter (not . isDontCare . snd) pairs  -- w/o EDontCare values
      pairsNoZero = filter (not . isZero . snd) pairsNoDC  -- w/o obvious 0 vals
      valueToConds = foldr ins M.empty pairsNoZero     -- value => [condition]
      valueToCond = M.map (optOr bit) valueToConds     -- value => condition
      valueToCondNoZero = M.filter (not . isZero) valueToCond  -- no obv 0 conds
      pairs' = map (\(v,c) -> (c,v)) $ M.toList valueToCondNoZero
      products = map (uncurry $ predicate typ) pairs'
  in optOr typ products
  where
    ins (cond, value) m =
      flip (M.insert value) m $
        case M.lookup value m of
          Nothing -> [cond]
          Just conds -> cond:conds
 -} 
-- Predicate an expression on a condition (note: argument order opposite).
-- The resulting expression takes on that value if the condition is true, or
-- zero otherwise.
predicate :: Type -> Exp -> Exp -> Exp
predicate typ cond exp =
  if isZero cond
    then zero typ
    else if isOnes cond
      then exp
      else optAnd typ [signExtend typ cond, exp]

-- Make an if-then-else, checking for trivial conditions and identical branches.
optIfThenElse :: Type -> Exp -> Exp -> Exp -> Exp
optIfThenElse typ e1 e2 e3 =
  if isZero e1
    then e3
    else if isOnes e1
      then e2
      else if e2 == e3
        then e2
        else if isDontCare e2
          then e3
          else if isDontCare e3
            then e2
            else EIfThenElse e1 e2 e3 `ofType` typ

-- Make a projection from an expression, checking for tuples.
optProj :: Type -> Int -> Exp -> Exp
optProj typ i e =
  case getInner e of
    ETuple es ->
      if 0 <= i && i < length es
        then es !! i
        else error $ "internal error: tuple mismatch in optProj: index=" ++
          show i ++ " into " ++ show e
    _ -> EProj i e `ofType` typ


-- Make a negation of an expression, checking for double negation and opeators
-- we know how to negate.
optNot :: Exp -> Exp
optNot exp =
  if isDontCare exp
    then exp
    else case getInner exp of
           EOpApp Not [e] -> e
           EOpApp Eq  [e1,e2] -> replace $ EOpApp Neq [e1,e2]
           EOpApp Neq [e1,e2] -> replace $ EOpApp Eq  [e1,e2]
           EOpApp Lt  [e1,e2] -> replace $ EOpApp Gte [e1,e2]
           EOpApp Gt  [e1,e2] -> replace $ EOpApp Lte [e1,e2]
           EOpApp Lte [e1,e2] -> replace $ EOpApp Gt  [e1,e2]
           EOpApp Gte [e1,e2] -> replace $ EOpApp Lt  [e1,e2]
           _ -> mkNot exp
  where
    replace inner' = Exp inner' (getExpLabel exp)

-- Make a disjunction of zero or more expressions, checking for neutral
-- elements, duplication, and forcing.
optOr :: Type -> [Exp] -> Exp
optOr typ exps =
  if any isOnes exps
    then ones typ
    else let exps' = nub $ filter (not . isZero) exps
         in case exps' of
           [] -> zero typ
           _ -> foldr1 mkOr exps'

-- Make a conjunction of zero or more expressions, checking for neutral
-- elements, duplication, and forcing.
optAnd :: Type -> [Exp] -> Exp
optAnd typ exps =
  if any isZero exps
    then zero typ
    else let exps' = nub $ filter (not . isOnes) exps
         in case exps' of
           [] -> ones typ
           _ -> foldr1 mkAnd exps'

-- Make an xor of zero or more expressions, checking for neutral elements and
-- duplication.
optXor :: Type -> [Exp] -> Exp
optXor typ exps =
  let exps' = nub $ filter (not . isZero) exps
  in case exps' of
    [] -> zero typ
    _ -> foldr1 mkXor exps'

-- Make a bitwise implication a=>b, checking for neutral elements, forcing, and
-- duplication.
optImpl :: Type -> Exp -> Exp -> Exp
optImpl typ e1 e2 =
  if isZero e1 || isOnes e2
    then ones typ
    else if isOnes e1
      then e2
      else mkOr (optNot e1) e2

-- Can we be sure this expression is zero?
isZero :: Exp -> Bool
isZero exp = exp == zero (typeOf exp)

-- Can we be sure this expression is all ones? (Currently only recognizes the
-- bit value 1 and sign extensions thereof.)
isOnes :: Exp -> Bool
isOnes exp =
  case getInner exp of
    EInt 1 -> typeOf exp == bit
    ESignExtend exp' -> isOnes exp'
    _ -> False

-- Is this expression a don't-care?
isDontCare :: Exp -> Bool
isDontCare exp = exp == (dontCare $ typeOf exp)

-- Zero expression of some type. (Neutral element of Or, Xor.)
zero :: Type -> Exp
zero typ = EInt 0 `ofType` typ

-- All-ones expression of some type. (Neutral element of And.)
ones :: Type -> Exp
ones typ =
  if typ == bit
    then EInt 1 `ofType` bit
    else mkNot $ zero typ

-- Expression whose value does not affect anything in operation. Use carefully.
dontCare :: Type -> Exp
dontCare typ = EDontCare `ofType` typ

-- Sign-extension.
signExtend :: Type -> Exp -> Exp
signExtend typ exp = ESignExtend exp `ofType` typ
