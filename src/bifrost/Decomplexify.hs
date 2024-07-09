module Decomplexify (decomplexify, isn'tComplex) where

{-
  Introduces intermediate variables so that:
  - All branch and loop conditions are combinational (in the sense of
    Expression.isCombinational).
  - All assignment right-hand-sides are simple (in the sense of
    Expression.isSimple).
  - While and For and ForEach have been replaced by Sandwich (with a combinational
    condition).

  For a more formal specification, see the function isn'tComplex.
-}

import qualified Data.Map as M
import Control.Monad.State
import Types
import Compile
import Expression
import ExpressionExtra
import Environment
import PrettyPrint

decomplexify :: StructuredProgram -> Compile StructuredProgram
decomplexify prog = mkCompile $ \env ->
  let (prog', env') = runState (decomplexify' prog) env
  in ([], Just (env', prog'))

-- Our friendly state monad.
type Dec a = State Environment a

-- What decomplexify strives to attain.
isn'tComplex :: StructuredProgram -> Bool
isn'tComplex = all isn'tComplexStm . M.elems . strp_subs

isn'tComplexStm :: Stm -> Bool
isn'tComplexStm stm =
  case stm of
    Do something -> isn'tComplexAtomicStm something
    Nop -> True
    Seq stm1 stm2 -> recurse [stm1, stm2]
    Label _ -> True
    Goto _ -> True
    Return -> True
    Branch exp stm1 stm2 -> isCombinational exp && recurse [stm1, stm2]
    While _ _ -> False
    For _ _ _ _ -> False
    ForEach _ _ _ _ -> False
    Sandwich stm1 exp stm2 -> isCombinational exp && recurse [stm1, stm2]
  where
    recurse = all isn'tComplexStm

isn'tComplexAtomicStm :: AtomicStm -> Bool
isn'tComplexAtomicStm astm =
  case astm of
    Assign _ exp -> isCombinational exp
    Scissors -> True
    HintPower _ _ -> True

decomplexify' :: StructuredProgram -> Dec StructuredProgram
decomplexify' prog = do
  let subs = strp_subs prog
  subs' <- mapM decomplexifyStm subs
  return $ prog { strp_subs = subs' }

decomplexifyStm :: Stm -> Dec Stm
decomplexifyStm stm =
  case stm of
    Do (Assign lhs exp) -> do
      (stms, exp') <- makeSimple exp
      return $ stms `andthen` (Do $ Assign lhs exp')
    Do Scissors -> fine
    Do (HintPower _ _) -> fine
    Nop -> fine
    Seq stm1 stm2 -> do
      stm1' <- recurse stm1
      stm2' <- recurse stm2
      return $ Seq stm1' stm2'
    Label _ -> fine
    Goto _ -> fine
    Return -> fine
    Branch exp stm1 stm2 -> do
      (stms, exp') <- makeCombinational exp
      stm1' <- recurse stm1
      stm2' <- recurse stm2
      return $ stms `andthen` (Branch exp' stm1' stm2')
    While exp stm1 -> do
      (expStms, exp') <- makeCombinational exp
      stm1' <- recurse stm1
      return $ Sandwich (expStms `andthen` Nop) exp' stm1'
    For stm1 exp stm2 stm3 -> do
      (expStms, exp') <- makeCombinational exp
      stm1' <- recurse stm1
      stm2' <- recurse stm2
      stm3' <- recurse stm3
      return $ Seq stm1' $
        Sandwich (expStms `andthen` Nop) exp' (Seq stm3' stm2')
    ForEach var exp1 exp2 stm -> do
      (exp1Stms, exp1') <- makeCombinational exp1
      (exp2Stms, exp2') <- makeCombinational exp2
      body <- recurse stm
      let initialize = Do $ Assign (LhsVarField var []) exp1'
      let t = typeOf exp1
      let varExp = EVar var `ofType` t
      let condition = EOpApp Neq [varExp, exp2'] `ofType` bit
      let increment = Do $ Assign (LhsVarField var []) $ EOpApp Plus [varExp, EInt 1 `ofType` t] `ofType` t
      return $ flip (foldr Seq) (exp1Stms ++ exp2Stms) $ Seq initialize $ Sandwich body condition increment
  where
    fine = return stm
    recurse = decomplexifyStm
    andthen :: [Stm] -> Stm -> Stm
    foos `andthen` bar = foldr Seq bar foos
    
-- Make an expression into one that is "simple" (see Expression.isSimple),
-- possibly producing additional statements along the way. The additional
-- statements are all non-complex.
makeSimple :: Exp -> Dec ([Stm], Exp)
makeSimple exp@(Exp inner label) =
  case inner of
    EActionCall name args -> do
      (stms, args') <- makeAllCombinational args
      return (stms, Exp (EActionCall name args') label)
    ESubCall name args -> do
      (stms, args') <- makeAllCombinational args
      return (stms, Exp (ESubCall name args') label)
    _ -> makeCombinational exp

-- Make an expression into one that is "combinational" (see
-- Expression.isCombinational), possibly producing additional statements along
-- the way. The additional statements are all non-complex, and only assign to
-- freshly-generated variables. Evaluation is carried out in the sensible order.
makeCombinational :: Exp -> Dec ([Stm], Exp)
makeCombinational exp@(Exp inner label) =
  case inner of
    EVar _ -> fine
    EInt _ -> fine
    EBlob _ -> fine
    EIfThenElse e1 e2 e3 -> do
      (stms1, e1') <- makeCombinational e1
      if isCombinational e2 && isCombinational e3
        then return (stms1, Exp (EIfThenElse e1' e2 e3) label)
        else do
          let typ = typeOf exp
          (lhs, rhs) <- allocStorage typ
          (stms2, e2') <- makeCombinational e2
          (stms3, e3') <- makeCombinational e3
          let trueBranch  = foldr Seq (Do $ Assign lhs e2') stms2
          let falseBranch = foldr Seq (Do $ Assign lhs e3') stms3
          let allStms = stms1 ++ [Branch e1' trueBranch falseBranch]
          return (allStms, rhs)
    EOpApp op args -> do
      things <- mapM makeCombinational args
      let stms = things >>= fst
      let args' = map snd things
      return (stms, relabel $ EOpApp op $ args')
    EApp fun arg -> do
      (stmsFun, fun') <- makeCombinational fun
      (stmsArg, arg') <- makeCombinational arg
      return (stmsFun ++ stmsArg, relabel $ EApp fun' arg')
    EField f e -> do
      (stms, e') <- makeCombinational e
      return (stms, relabel $ EField f e')
    EFieldUpdate f eStruct eNewValue -> do
      (stmsStruct, eStruct') <- makeCombinational eStruct
      (stmsNewValue, eNewValue') <- makeCombinational eNewValue
      return (stmsStruct ++ stmsNewValue,
              relabel $ EFieldUpdate f eStruct' eNewValue')
    ETuple es -> do
      (stms, es') <- makeAllCombinational es
      return (stms, relabel $ ETuple es')
    EProj i e -> do
      (stms, e') <- makeCombinational e
      return (stms, relabel $ EProj i e')
    ETypeAnn t e -> do
      (stms, e') <- makeCombinational e
      return (stms, relabel $ ETypeAnn t e')
    EActionCall name args -> do
      (stms, args') <- makeAllCombinational args
      let typ = el_type label
      (lhs, rhs) <- allocStorage typ
      let stm = Do $ Assign lhs $ relabel $ EActionCall name args'
      return (stms ++ [stm], rhs)
    ESubCall name args -> do
      (stms, args') <- makeAllCombinational args
      let typ = el_type label
      (lhs, rhs) <- allocStorage typ
      let stm = Do $ Assign lhs $ relabel $ ESubCall name args'
      return (stms ++ [stm], rhs)
    _ -> error $ "Unexpected expression to makeCombinational: " ++ printExp exp
  where
    fine = return ([], exp)
    relabel inner' = Exp inner' label

-- Make multiple expressions combinational (in order).
makeAllCombinational :: [Exp] -> Dec ([Stm], [Exp])
makeAllCombinational es = do
  things <- mapM makeCombinational es
  return (things >>= fst, map snd things)

-- Create intermediate variables to match a type we wish to store. Favors making
-- many small variables, rather than one storing everything.
allocStorage :: Type -> Dec (Lhs, Exp)
allocStorage typ =
  case typ of
    NamedType _ -> atomic
    Product ts -> do
      pairs <- mapM allocStorage ts
      let ls = map fst pairs
      let rs = map snd pairs
      return (mkLhsTuple ls, mkTuple rs)
    Fun _ _ -> error $ "Cannot make storage for function type: " ++ show typ
  where
    atomic = do
      env <- get
      let (var, env') = allocateMiscVar typ env
      put env'
      let lhs = LhsVarField var []
      let rhs = EVar var `ofType` typ
      return (lhs, rhs)
