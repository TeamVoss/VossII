module ExpressionExtra where

import Types
import Expression
import PrettyPrint

-- For errors in the below...
calamity :: String -> [String] -> a
calamity s xs =
  let suffix = case xs of
                 [] -> ""
                 _ -> ":\n" ++ joinWith "\n" xs
  in error $ "internal error: " ++ s ++ suffix

-- Like mkTuple but with an exception for n=1.
smartTuple :: [Exp] -> Exp
smartTuple es =
  case es of
    [e] -> e
    _ -> mkTuple es

-- Like mkTupleType but with an exception for n=1.
smartTupleType :: [Type] -> Type
smartTupleType ts =
  case ts of
    [t] -> t
    _ -> mkTupleType ts

-- Like mkLhsTuple but with an exception for n=1.
smartLhsTuple :: [Lhs] -> Lhs
smartLhsTuple lhss =
  case lhss of
    [lhs] -> lhs
    _ -> mkLhsTuple lhss

mkTuple :: [Exp] -> Exp
mkTuple exps =
  let typ = mkTupleType $ map typeOf exps
  in ETuple exps `ofType` typ

mkTupleType :: [Type] -> Type
mkTupleType = Product

mkLhsTuple :: [Lhs] -> Lhs
mkLhsTuple = LhsTuple

mkIfThenElse :: Exp -> Exp -> Exp -> Exp
mkIfThenElse cond e1 e2 =
  let tc = typeOf cond
      t1 = typeOf e1
      t2 = typeOf e2
  in if tc == bit && t1 == t2
    then EIfThenElse cond e1 e2 `ofType` t1
    else calamity "type mismatch when making if-then-else expression" $
         map show [cond,e1,e2] ++ map show [tc,t1,t2]

mkNot :: Exp -> Exp
mkNot exp = EOpApp Not [exp] `ofType` (typeOf exp)

mkOr :: Exp -> Exp -> Exp
mkOr e1 e2 =
  let t1 = typeOf e1
      t2 = typeOf e2
  in if t1 == t2
    then EOpApp Or [e1,e2] `ofType` t1
    else calamity "type mismatch when making or-expression" $
         map show [e1,e2] ++ map show [t1,t2]

mkAnd :: Exp -> Exp -> Exp
mkAnd e1 e2 =
  let t1 = typeOf e1
      t2 = typeOf e2
  in if t1 == t2
    then EOpApp And [e1,e2] `ofType` t1
    else calamity "type mismatch when making and-expression" $
         map show [e1,e2] ++ map show [t1,t2]

mkXor :: Exp -> Exp -> Exp
mkXor e1 e2 =
  let t1 = typeOf e1
      t2 = typeOf e2
  in if t1 == t2
    then EOpApp Xor [e1,e2] `ofType` t1
    else calamity "type mismatch when making xor-expression" $
         map show [e1,e2] ++ map show [t1,t2]

mkProj :: Int -> Exp -> Exp
mkProj i e =
  case typeOf e of
    Product ts -> if 0 <= i && i < length ts
      then EProj i e `ofType` (ts !! i)
      else error $ "internal error: invalid index when making projection"
    _ -> calamity "type mismatch when making projection" $
         [show i, show e, show $ typeOf e]

-- Match a left and rhs as granularly as possible. Assumes that this is all
-- well-typed so everything should match. Skips ()=() matches.
matchSides :: Lhs -> Exp -> [(Lhs, Exp)]
matchSides lhs rhs = 
  case (lhs, rhs) of
    (LhsTuple lhss, Exp (ETuple rhss) _) ->
      if length lhss == length rhss
        then zip lhss rhss >>= uncurry matchSides
        else calamity "matchSides: tuple size mismatch" [show lhs, show rhs]
    _ -> [(lhs,rhs)]

typesOfFunPat :: FunPat -> ([Type],Type)
typesOfFunPat fp =
  (map typeOfPattern $ fp_arguments fp, typeOfPattern $ fp_result fp)

typeOfPattern :: Pattern -> Type
typeOfPattern pat =
  case pat of
    PSingle (_,t) -> t
    PTuple pats -> mkTupleType $ map typeOfPattern pats

-- Where a named parameter occurs in a pattern, and what type it has in each
-- occurrence. With sensible patterns a parameter should occur at most once, but
-- this function makes no such assumption.
paramPaths :: VarName -> Pattern -> [([Int], Type)]
paramPaths name = foo
  where
    foo pat =
      case pat of
        PSingle (name',t) -> if name' == name then [([], t)] else []
        PTuple pats ->
          let results = map foo pats
              augment i (p,t) = (i:p,t)
              results' = zipWith (map . augment) [0..] results
          in concat results'

flattenPat :: Pattern -> [Param]
flattenPat pat =
  case pat of
    PSingle param -> [param]
    PTuple pats -> pats >>= flattenPat
