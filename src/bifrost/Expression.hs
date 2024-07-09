module Expression where

import qualified Data.Map as M
import Types

getInner :: Exp -> InnerExp
getInner (Exp inner _) = inner

getExpLabel :: Exp -> ExpLabel
getExpLabel (Exp _ label) = label

swapInner :: InnerExp -> Exp -> Exp
swapInner inner (Exp _ label) = Exp inner label

typeOf :: Exp -> Type
typeOf (Exp e lbl) = el_type lbl

ofType :: InnerExp -> Type -> Exp
ofType e t = Exp e (ExpLabel t)

untyped :: InnerExp -> Exp
untyped inner = inner `ofType` Untyped

bit :: Type
bit = NamedType "bit"

-- Test whether an expression is purely combinational, meaning that it does not
-- contain EActionCall or ESubCall.
isCombinational :: Exp -> Bool
isCombinational exp =
  case getInner exp of
    EActionCall _ _ -> False
    ESubCall _ _ -> False
    _ -> all isCombinational $ children exp

-- Test whether an expression is "simple", meaning that it is either:
-- 1) Combinational in the isCombinational sense
-- 2) A action call, with a combinational argument
-- 3) A subroutine call, with a combinational argument
-- These are the allowed right-hand-sides of assignment statements in the
-- compiler (see Decomplexify for reducing arbitrary assignments into these).
isSimple :: Exp -> Bool
isSimple exp@(Exp inner _) =
  if isCombinational exp
    then True
    else
      case inner of
        EActionCall _ args -> all isCombinational args
        ESubCall _ args -> all isCombinational args
        _ -> False

-- Get the application spine of an expression. Returns the expression itself if
-- it's not an application (the idea being that it's a degenerate application to
-- zero arguments).
expSpine :: Exp -> [Exp]
expSpine exp@(Exp inner _) =
  case inner of
    EApp f arg -> expSpine f ++ [arg]
    _ -> [exp]

opArity :: Op -> Int
opArity op =
  case op of
    Not -> 1
    _ -> 2

opPosition :: Op -> OpPosition
opPosition op =
  case op of
    Not -> Prefix
    _ -> Infix

-- The children of an expression.
children :: Exp -> [Exp]
children (Exp inner _) =
  case inner of
    EIfThenElse e1 e2 e3 -> [e1,e2,e3]
    EOpApp _ es -> es
    EApp e1 e2 -> [e1,e2]
    EField _ e -> [e]
    EFieldUpdate _ e1 e2 -> [e1,e2]
    ETuple es -> es
    EProj _ e -> [e]
    ETypeAnn _ e -> [e]
    EActionCall _ es -> es
    ESubCall _ es -> es
    ESignExtend e -> [e]
    EDict m -> M.elems m
    _ -> []

-- Apply a function to each child of an expression, and reassemble.
uponChildren :: (Exp -> Exp) -> Exp -> Exp
uponChildren g exp@(Exp inner label) =
  case inner of
    EIfThenElse e1 e2 e3 ->
      let [e1',e2',e3'] = map g [e1,e2,e3]
      in subst $ EIfThenElse e1' e2' e3'
    EOpApp op es -> nary (EOpApp op) es
    EApp e1 e2 -> binary EApp e1 e2
    EField f e -> unary (EField f) e
    EFieldUpdate f e1 e2 -> binary (EFieldUpdate f) e1 e2
    ETuple es -> nary ETuple es
    EProj i e -> unary (EProj i) e
    ETypeAnn t e -> unary (ETypeAnn t) e
    EActionCall name es -> nary (EActionCall name) es
    ESubCall name es -> nary (ESubCall name) es
    ESignExtend e -> unary ESignExtend e
    EDict m -> subst $ EDict $ M.map g m
    _ -> same
  where
    unary f e =
      let e' = g e
      in subst $ f e'
    binary f e1 e2 =
      let e1' = g e1
          e2' = g e2
      in subst $ f e1' e2'
    nary f es =
      let es' = map g es
      in subst $ f es'
    subst inner' = Exp inner' label
    same = exp

-- Apply a monadic function to each child of an expression, and reassemble.
untoChildren :: Monad m => (Exp -> m Exp) -> Exp -> m Exp
untoChildren g exp@(Exp inner label) =
  case inner of
    EIfThenElse e1 e2 e3 -> do
      res <- mapM g [e1,e2,e3]
      case res of
        [e1',e2',e3'] -> subst $ EIfThenElse e1' e2' e3'
    EOpApp op es -> nary (EOpApp op) es
    EApp e1 e2 -> binary EApp e1 e2
    EField f e -> unary (EField f) e
    EFieldUpdate f e1 e2 -> binary (EFieldUpdate f) e1 e2
    ETuple es -> nary ETuple es
    EProj i e -> unary (EProj i) e
    ETypeAnn t e -> unary (ETypeAnn t) e
    EActionCall name es -> nary (EActionCall name) es
    ESubCall name es -> nary (ESubCall name) es
    ESignExtend e -> unary ESignExtend e
    EDict m -> do
      m' <- mapM g m
      subst $ EDict m'
    _ -> same
  where
    unary f e = do
      e' <- g e
      subst $ f e'
    binary f e1 e2 = do
      e1' <- g e1
      e2' <- g e2
      subst $ f e1' e2'
    nary f es = do
      es' <- mapM g es
      subst $ f es'
    subst inner' = return $ Exp inner' label
    same = return exp


-- The children of a particular type.
typeChildren :: Type -> [Type]
typeChildren t =
  case t of
    Untyped -> []
    NamedType _ -> []
    Address -> []
    Product ts -> ts
    Fun t1 t2 -> [t1,t2]
    FLType _ -> []

-- Apply a function to each of a type's children, and reassemble.
uponTypeChildren :: (Type -> Type) -> Type -> Type
uponTypeChildren f t =
  case t of
    Untyped -> t
    NamedType _ -> t
    Address -> t
    Product ts -> Product (map f ts)
    Fun t1 t2 -> Fun (f t1) (f t2)

-- Statically evaluate an expression using "pure" semantics (arb precision) to
-- a normal form: an EInt, an EStr, or an EDict containing normal-form exprs.
pureEval :: Exp -> Maybe Exp
pureEval exp =
  case getInner exp of
    EInt i -> do
      give $ EInt i
    EOpApp op exps -> do
      args <- mapM pureEval exps
      evalOp op (map getInner args)
    EStr s -> do
      give $ EStr s
    EDict m -> do
      m' <- mapM pureEval m
      give $ EDict m'
    _ -> Nothing
  where
    give = return . untyped
    evalOp op is =
      case (op, is) of
        (Plus,  [EInt i, EInt j]) -> give $ EInt $ i + j
        (Minus, [EInt i, EInt j]) -> give $ EInt $ i - j
        (Times, [EInt i, EInt j]) -> give $ EInt $ i * j
        (Div,   [EInt i, EInt j]) -> give $ EInt $ i `div` j
        (Mod,   [EInt i, EInt j]) -> give $ EInt $ i `mod` j
        (Plus,  [EStr s1, EStr s2]) -> give $ EStr $ s1 ++ s2
        (Plus,  [EDict m1, EDict m2]) -> give $ EDict $ M.union m1 m2
        _ -> Nothing

-- Like pureEval, but fails unless the result is an EInt.
pureEvalInteger :: Exp -> Maybe Integer
pureEvalInteger exp = do
  exp' <- pureEval exp
  case getInner exp' of
    EInt i -> return i
    _ -> Nothing

-- Like pureEval, but fails unless the result is an EDict.
pureEvalDict :: Exp -> Maybe (M.Map String Exp)
pureEvalDict exp = do
  exp' <- pureEval exp
  case getInner exp' of
    EDict m -> return m
    _ -> Nothing
