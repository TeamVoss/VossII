module Typecheck where

import Control.Monad (foldM, liftM, mapM, zipWithM, zipWithM_)
import Data.Either
import Data.Maybe (catMaybes)
import qualified Data.Map as M
import Types
import Message
import Expression
import ExpressionExtra
import Environment
import PrettyPrint


----------------------------------------
-- Typechecking monad
----------------------------------------

data TC a = TC (Environment -> Either Message a)

instance Functor TC where
  fmap f (TC g) = TC $ \e -> fmap f (g e)

instance Applicative TC where
  pure x = TC (\_ -> Right x)
  (TC f) <*> (TC g) = TC $ \e ->
    case (f e, g e) of
      (Left err, _) -> Left err
      (_, Left err) -> Left err
      (Right h, Right x) -> Right (h x)

instance Monad TC where
  return x = TC (\_ -> Right x)
  (TC f) >>= g = TC $ \e ->
    case f e of
      Left err -> Left err
      Right x ->
        case g x of
          TC h -> h e

runTC :: TC a -> Environment -> Either Message a
runTC (TC f) env = f env

optional :: TC a -> TC (Maybe a)
optional (TC f) = TC $ \e ->
  case f e of
    Left _ -> return Nothing
    Right x -> return $ Just x

backup :: TC a -> TC a -> TC a
backup plan1 plan2 = do
  m <- optional plan1
  case m of
    Just x -> return x
    Nothing -> plan2

----------------------------------------
-- Typechecking of assignments
----------------------------------------

-- Returns the new RHS if everything checks.
checkAssign :: Lhs -> Exp -> TC Exp
checkAssign lhs rhs = do
  mt1 <- lookupLhsType lhs
  rhs' <- case mt1 of
    Just t1 -> check rhs t1
    Nothing -> infer rhs
  checkLhsFitsRhs lhs $ typeOf rhs'
  return rhs'

-- Assumes rhs has already been typechecked successfully.
checkLhsFitsRhs :: Lhs -> Type -> TC ()
checkLhsFitsRhs lhs rtyp =
  case (lhs, rtyp) of
    (LhsVarField v fs, _) -> do
      ltyp <- lookupVarFieldType v fs
      if ltyp == rtyp
        then does
        else doesn't
    (LhsTuple lhss, Product rtyps) -> do
      if length lhss == length rtyps
        then zipWithM_ checkLhsFitsRhs lhss rtyps
        else doesn't
    (LhsIgnore, _) -> does
    _ -> doesn't
  where
    does = return ()
    doesn't = broke $ errLhsRhsMismatch lhs rtyp 


----------------------------------------
-- Typechecking of expressions
----------------------------------------

-- Check that an expression is of a given type, and label it (and its sub-
-- expressions) accordingly.
check :: Exp -> Type -> TC Exp
check exp typ =
  case inner of
    EVar _ -> inferAndCheck
    EInt i ->
      case typ of
        NamedType _ -> original
        Address -> failboat $ errIntsAren't "return addresses"
        Product _ -> failboat $ errIntsAren't "tuples"
        Fun _ _ -> failboat $ errIntsAren't "functions"
    EBlob _ -> original
    EIfThenElse cond e1 e2 -> do
      cond' <- check cond bit
      e1' <- check e1 typ
      e2' <- check e2 typ
      replace $ EIfThenElse cond' e1' e2'
    EOpApp op args -> do
      assert (length args == opArity op) errArityMismatch
      case opTypeStyle op of
        Combine -> do
          args' <- mapM (flip check typ) args
          replace $ EOpApp op args'
        Compare -> do
          assert (typ == bit) errComparisonBit
          argsInfer <- mapM (optional . infer) args
          case catMaybes argsInfer of
            [] -> failboat errOperandTypesDunno
            (e:_) -> do
              let t = typeOf e
              args' <- mapM (flip check t) args
              replace $ EOpApp op args'
    EApp e1 e2 ->
      (do 
        e1' <- infer e1
        let t1 = typeOf e1'
        case t1 of
          Fun ta tb -> do
            assert (tb == typ) $ errFunResult t1
            e2' <- check e2 ta
            replace $ EApp e1' e2'
          _ -> failboat $ errNotFunType t1
      ) `backup`
      (do
        e2' <- infer e2
        let t2 = typeOf e2'
        e1' <- check e1 (Fun t2 typ)
        replace $ EApp e1' e2'
      )
    EField _ _ -> inferAndCheck
    EFieldUpdate f e1 e2 -> do
      tf <- lookupStructField typ f
      e1' <- check e1 typ
      e2' <- check e2 tf
      replace $ EFieldUpdate f e1' e2'
    ETuple es ->
      case typ of
        Product ts -> do
          assert (length es == length ts) errTupleSize
          es' <- zipWithM check es ts
          replace $ ETuple es'
        _ -> failboat $ errNotProductType typ
    EProj _ _ -> inferAndCheck
    ETypeAnn typ' e1 -> do
      assert (typ == typ') errAnnotation
      e1' <- check e1 typ
      replace $ ETypeAnn typ e1'
    EActionCall _ _ -> inferAndCheck
    ESubCall _ _ -> inferAndCheck
    EStr _ -> failboat errString
    EDict _ -> failboat errDict
  where
    inner = getInner exp
    original = return $ inner `ofType` typ
    replace inner' = return $ inner' `ofType` typ
    inferAndCheck = do
      exp' <- infer exp
      let t = typeOf exp'
      if t == typ
        then return exp'
        else failboat $ errActualType t
    assert cond msg =
      if cond
        then return ()
        else failboat msg
    failboat msg = broke $ errCheck exp typ msg

-- Infer the type of an expression (and all sub-expressions), and label them
-- accordingly.
infer :: Exp -> TC Exp
infer exp =
  case inner of
    EVar v -> do
      env <- getEnv
      case findVarType env v of
        Just vt -> original vt
        Nothing -> failboat $ errMissingVarType v
    EInt _ -> failboat errInferInt
    EBlob _ -> failboat errInferBlob
    EIfThenElse cond e1 e2 -> do
      cond' <- check cond bit
      e1' <- infer e1
      e2' <- infer e2
      let t1 = typeOf e1'
      let t2 = typeOf e2'
      assert (t1 == t2) $ errBranchTypes t1 t2
      replace t1 $ EIfThenElse cond' e1' e2'
    EOpApp op args -> do
      assert (length args == opArity op) errArityMismatch
      argsInfer <- mapM (optional . infer) args
      case catMaybes argsInfer of
        [] -> failboat errOperandTypesDunno
        (e:_) -> do
          let t = typeOf e
          args' <- mapM (flip check t) args
          let ts = map typeOf args'
          let new = EOpApp op args'
          case opTypeStyle op of
            Combine -> do
              assert (not $ null ts) $ errOperandTypes ts
              replace (head ts) new
            Compare -> replace bit new
    EApp e1 e2 -> do
      e1' <- infer e1
      let t1 = typeOf e1'
      case t1 of
        Fun ta tb -> do
          e2' <- check e2 ta
          replace tb $ EApp e1' e2'
        _ -> failboat $ errNotFunType t1
    EField f e1 -> do
      e1' <- infer e1
      let t1 = typeOf e1'
      tf <- lookupStructField t1 f
      replace tf $ EField f e1'
    EFieldUpdate f e1 e2 -> do
      e1' <- infer e1
      let t1 = typeOf e1'
      tf <- lookupStructField t1 f
      e2' <- check e2 tf
      replace t1 $ EFieldUpdate f e1' e2'
    ETuple es -> do
      es' <- mapM infer es
      let typ = mkTupleType $ map typeOf es'
      replace typ $ ETuple es'
    EProj i e1 -> do
      e1' <- infer e1
      ti <- project (typeOf e1') i
      replace ti $ EProj i e1'
    ETypeAnn typ e1 -> do
      e1' <- check e1 typ
      replace typ $ ETypeAnn typ e1'
    EActionCall name args -> do
      (tas, tb) <- lookupActionArgResultTypes name
      assert (length args == length tas) errArityMismatch
      args' <- zipWithM check args tas
      replace tb $ EActionCall name args'
    ESubCall name args -> do
      (tas, tb) <- lookupSubArgResultTypes name
      assert (length args == length tas) errArityMismatch
      args' <- zipWithM check args tas
      replace tb $ ESubCall name args'
    EStr _ -> failboat errString
    EDict _ -> failboat errDict
  where
    inner = getInner exp
    original t = return $ inner `ofType` t
    replace t inner' = return $ inner' `ofType` t
    assert cond msg =
      if cond
        then return ()
        else failboat msg
    failboat msg = broke $ errInfer exp msg




----------------------------------------
-- Handy helpers
----------------------------------------

-- Returns Nothing if the lhs contains LhsIgnore, as we can't possibly know the
-- type for that (and this does not indicate error).
lookupLhsType :: Lhs -> TC (Maybe Type)
lookupLhsType lhs =
  case lhs of
    LhsVarField v fs -> liftM Just $ lookupVarFieldType v fs
    LhsTuple lhss -> do
      mts <- mapM lookupLhsType lhss
      return $ do
        ts <- sequence mts
        return $ Product ts
    LhsIgnore -> return Nothing

lookupVarFieldType :: Var -> [Field] -> TC Type
lookupVarFieldType v fs = do
  vt <- lookupVarType v
  foldM lookupStructField vt fs

lookupVarType :: Var -> TC Type
lookupVarType v = do
  env <- getEnv
  case findVarType env v of
    Just t -> return t
    Nothing -> broke $ errMissingVarType v

lookupStructField :: Type -> Field -> TC Type
lookupStructField t f = do
  env <- getEnv
  case t of
    NamedType tn ->
      case findStruct env tn of
        Just struct ->
          case M.lookup f $ struct_fields struct of
            Just tf -> return tf
            Nothing -> broke $ errNoField t f
        Nothing -> broke $ errNoStructInfo t
    _ -> broke $ errNotStruct t

lookupActionArgResultTypes :: ActionName -> TC ([Type],Type)
lookupActionArgResultTypes name = do
  env <- getEnv
  case findActionInfo env name of
    Just ai -> return $ typesOfFunPat $ at_io $ ai_type ai
    Nothing -> broke $ errMissingAction name

lookupSubArgResultTypes :: SubroutineName -> TC ([Type],Type)
lookupSubArgResultTypes name = do
  env <- getEnv
  case findSubroutineType env name of
    Just st -> return $ typesOfFunPat $ st_io st
    Nothing -> broke $ errMissingSub name

project :: Type -> Int -> TC Type
project t i =
  case t of
    Product ts ->
      if 0 <= i && i < length ts
        then return $ ts !! i
        else couldn't
    _ -> couldn't
  where
    couldn't = broke $ errProjection t i

broke :: String -> TC a
broke = TC . const . Left . Error

getEnv :: TC Environment
getEnv = TC (\env -> Right env)

-- Didn't want to deal with metavariables so we describe an operator's type
-- using this fixed set of possible polymorphic types.
data OpTypeStyle
  = Combine        -- op : T^n -> T
  | Compare        -- op : T^n -> bit
  
opTypeStyle :: Op -> OpTypeStyle
opTypeStyle op =
  case op of
    Not -> Combine
    Or -> Combine
    And -> Combine
    Xor -> Combine
    Eq -> Compare
    Neq -> Compare
    Lt -> Compare
    Gt -> Compare
    Lte -> Compare
    Gte -> Compare
    Plus -> Combine
    Minus -> Combine
    Times -> Combine
    Div -> Combine
    Mod -> Combine
    ShiftL -> Combine
    ShiftR -> Combine
    ArithShiftR -> Combine

allEqual :: Eq a => [a] -> Bool
allEqual xs = and $ zipWith (==) xs $ tail xs


----------------------------------------
-- Error messages
----------------------------------------

errActualType :: Type -> String
errActualType t = "actual type is " ++ show t

errAnnotation :: String
errAnnotation = "annotation doesn't match expected type"

errArityMismatch :: String
errArityMismatch = "arity mismatch"

errBranchTypes :: Type -> Type -> String
errBranchTypes t1 t2 = "if-then-else branch types not equal: " ++
  show t1 ++ ", " ++ show t2

errCheck :: Exp -> Type -> String -> String
errCheck exp typ msg =
  "Could not typecheck: " ++ show exp ++
  " as type " ++ show typ ++
  " because: " ++ msg

errComparisonBit :: String
errComparisonBit = "comparison produces type bit"

errDict :: String
errDict = "it's a dictionary!?"

errFunResult :: Type -> String
errFunResult t =
  "function type gives result that doesn't match expected type: " ++ show t

errInfer :: Exp -> String -> String
errInfer exp msg =
  "Could not infer the type of: " ++ show exp ++
  " because: " ++ msg

errInferInt :: String
errInferInt = "it's a bloody integer"

errInferBlob :: String
errInferBlob = "it's a blob"

errIntsAren't :: String -> String
errIntsAren't s = "integers are not " ++ s

errLhsRhsMismatch :: Lhs -> Type -> String
errLhsRhsMismatch lhs rtyp =
  "left-hand side " ++ show lhs ++
  " does not match rhs type " ++ show rtyp

errMissingAction :: ActionName -> String
errMissingAction name = "cannot find action " ++ show name

errMissingVarType :: Var -> String
errMissingVarType v = "cannot find var " ++ show v

errMissingSub :: SubroutineName -> String
errMissingSub name = "cannot find nu" ++ "clear sub " ++ show name ++ "."

errNoField :: Type -> Field -> String
errNoField t f = "structure type " ++ show t ++
  " does not seem to contain a field " ++ show f

errNotFunType :: Type -> String
errNotFunType t = "not a function type: " ++ show t

errNotProductType :: Type -> String
errNotProductType t = "not a product (i.e. tuple) type: " ++ show t

errNoStructInfo :: Type -> String
errNoStructInfo t = "could not find struct info for type: " ++ show t

errNotStruct :: Type -> String
errNotStruct t = "not a struct type: " ++ show t

errNotUnitType :: Type -> String
errNotUnitType t = "not the unit type: " ++ show t

errOperandTypes :: [Type] -> String
errOperandTypes ts = "operand types no good: " ++ show ts

errOperandTypesDunno :: String
errOperandTypesDunno = "could not deduce operand types"

errProjection :: Type -> Int -> String
errProjection t i =
  "invalid projection: index " ++ show i ++ " of type " ++ show t

errString :: String
errString = "it's a string. Perhaps you meant it to be a blob of FL code instead?"

errTupleSize :: String
errTupleSize = "tuple size doesn't match type"
