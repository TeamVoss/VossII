module ReadAST (Message, readAST) where

import Control.Monad
import Control.Monad.Fail
import Data.Either
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Set as S
import qualified Lang.AbsLang as Ast
import Lang.PrintLang ()
import Types
import Message
import Compile
import Environment
import Expression
import ExpressionExtra
import Protocol
import KitchenSink

-- Locations within the original source code for error reporting
data Location = NoClue | Global | InSub SubroutineName

-- The monad we use for everything
type Arr = KitchenSink Message ArrState

-- For named action types
type ActionTypeName = String

-- The state we keep
data ArrState = ArrState
  { arrs_env :: Environment    -- environment
  , arrs_defs :: M.Map VarName ([VarName], Ast.Exp)  -- macro name => params+exp
  , arrs_aliases :: M.Map TypeName Type  -- type aliases
  , arrs_loc :: Location       -- present location for error reporting
  , arrs_has_name :: Bool      -- has the name been declared?
  , arrs_has_protocol :: Bool  -- has the protocol been declared?
  , arrs_action_types :: M.Map ActionTypeName ActionType  -- named action types
  }

-- Non-environmental data from top-level declarations
data Top
  = TSubroutine SubroutineName Stm
  | TOther

readAST :: Ast.Program -> Compile StructuredProgram
readAST astprog = mkCompile $ \env ->
  let todo = preamble >> crawlProg astprog
      initial = initArrState env
      (mres, msgs, st) = runKitchenSink todo initial
      (errors, warnings) = splitMessages msgs
      env' = arrs_env st
  in case (errors, mres) of
    ([], Just prog) -> (msgs, Just (env', prog))
    _ -> (msgs, Nothing)
  where
    splitMessages = partitionEithers . map messageToEither

mainSubroutineName :: SubroutineName
mainSubroutineName = "main"

initArrState :: Environment -> ArrState
initArrState env = ArrState env M.empty M.empty NoClue False False M.empty


preamble :: Arr ()
preamble = do
  declareType "bit" (Just "bit")

crawlProg :: Ast.Program -> Arr StructuredProgram
crawlProg (Ast.Prog asttops) = do
  setLoc Global
  tops <- crawlTops asttops
  let subs  = tops >>= extractSubroutine
  st <- get
  when (not $ arrs_has_name st) $ yell errNoName
  when (not $ arrs_has_protocol st) $ yell errNoProtocol
  when (mainSubroutineName `notElem` (map fst subs)) $ yell errNoMain
  return $ StructuredProgram mainSubroutineName (M.fromList subs)
  where
    extractSubroutine t =
      case t of
        TSubroutine name stm -> [(name, stm)]
        TOther -> []

crawlTops :: [Ast.TopLevel] -> Arr [Top]
crawlTops tops = do
  (_, tops') <- seqWithExtraState crawlTop True tops
  return tops'

crawlTop :: Bool -> Ast.TopLevel -> Arr (Bool, Top)
crawlTop declsAllowed t =
  case t of
    Ast.TopDecl decl -> do
      when (not declsAllowed) $ yell $ errForbiddenDecl decl
      crawlDecl decl
      return (True, TOther)  -- `True` so error only yelled once
    Ast.TopSub subdef -> do
      (name, stm) <- crawlSub subdef
      return (False, TSubroutine name stm)

crawlDecl :: Ast.Decl -> Arr ()
crawlDecl decl =
  case decl of
    Ast.NameDecl ident -> declareName $ identToProgramName ident
    Ast.ProtocolDecl proto -> do
      proto' <- crawlProtocol proto
      declareProtocol $ proto'
    Ast.FlavorDecl flavor -> do
      flavor' <- crawlFlavor flavor
      declareFlavor flavor'
    Ast.StateDecl aspects ->
      mapM_ (declareStateAspect . convertStateAspect) aspects
    Ast.ActionTypeDecl name atyp ->
      let name' = convertActionTypeName name
      in do
        atyp' <- crawlActionType atyp
        declareActionType name' atyp'
    Ast.ActionDecl name typ arrayInfo provider protocol ->
      let name' = convertActionName name
      in do
        protocol' <- crawlProtocol protocol
        provider' <- crawlProvider provider
        typ' <- crawlActionType typ
        array <- crawlActionArrayInfo arrayInfo
        declareAction name' typ' array provider' protocol'
    Ast.GlobalVarDecl varDecl -> crawlVarDecl varDecl Nothing
    Ast.TypeDecl name compdef ->
      let name' = convertTypeName name
          compdef' = case compdef of
            Ast.TypeCompileString s -> Just s
            Ast.TypeCompileInfer -> Nothing
      in declareType name' compdef'
    Ast.TypeAliasDecl name typ -> do
      let name' = convertTypeName name
      typ' <- crawlType typ
      declareTypeAlias name' typ'
    Ast.FieldsDecl name defs -> do
      let name' = convertTypeName name
      defs' <- mapM crawlFieldDef defs
      declareFields name' defs'
    Ast.NumericDecl name imod -> do
      let name' = convertTypeName name
      declareNumeric name' imod
    Ast.DefineDecl defname defparams exp -> do
      let defname' = convertDefineName defname
      let defparams' = map convertDefineParam defparams
      declareDefine defname' defparams' exp

crawlActionType :: Ast.ActionType -> Arr ActionType
crawlActionType at =
  case at of
    Ast.ActionTypeNamed name ->
      let name' = convertActionTypeName name
      in do
        st <- get
        case M.lookup name' $ arrs_action_types st of
          Just atyp -> return atyp
          Nothing -> scream $ errNo "action type" name'
    Ast.ActionTypeRaw
      funpat
      (Ast.StateAspectList aspectsR)
      (Ast.StateAspectList aspectsW) -> do
        funpat' <- crawlFunPat funpat
        aspectsR' <- mapM crawlStateAspect aspectsR
        aspectsW' <- mapM crawlStateAspect aspectsW
        let atyp = ActionType funpat' $ ActionStateInteraction aspectsR' aspectsW'
        return atyp


crawlActionArrayInfo :: Ast.ActionArrayInfo -> Arr Integer
crawlActionArrayInfo arrayInfo =
  case arrayInfo of
    Ast.ActionSingle -> return 0
    Ast.ActionArray arrayExp -> do
      arrayExp1 <- crawlExpNoResolve arrayExp
      case pureEvalInteger arrayExp1 of
        Just array -> return array
        Nothing -> do
          yell $ errArraySize arrayExp1
          return 0

crawlProtocol :: Ast.Protocol -> Arr Protocol
crawlProtocol proto =
  case proto of
    Ast.ProtocolAuto -> return ProtocolAuto
    Ast.ProtocolGiven a -> do
      exp <- crawlExpNoResolve a
      case pureEval exp of
        Nothing -> scream $ errNotStatic exp
        Just exp' -> case protocolFromExp exp' of
          Nothing -> scream $ errBadProtocol exp'
          Just p -> return p

crawlFlavor :: String -> Arr Flavor
crawlFlavor s =
    case s of
        "vanilla" -> return Vanilla
        "chocolate" -> return Chocolate
        "mint" -> return Mint
        "strawberry" -> return Strawberry
        _ -> scream $ errBadFlavor s

crawlProvider :: Ast.Provider -> Arr ActionProvider
crawlProvider prov =
  case prov of
    Ast.ProviderExternal -> return External
    Ast.ProviderModule s -> return $ Instantiate s []
    Ast.ProviderModuleShare s as -> do
      actions <- mapM crawlActionName as
      return $ Instantiate s actions

crawlStateAspect :: Ast.StateAspect -> Arr GlobalStateAspect
crawlStateAspect aspect = do
  let aspect' = convertStateAspect aspect
  env <- getEnv
  when (not $ existsStateAspect env aspect') $ yell $ errNoAspect aspect'
  return aspect'

crawlFieldDef :: Ast.FieldDef -> Arr (Field, Type)
crawlFieldDef (Ast.MkFieldDef name typ) = do
  let name' = convertFieldName name
  typ' <- crawlType typ
  return (name', typ')

crawlSub :: Ast.SubDef -> Arr (SubroutineName, Stm)
crawlSub (Ast.MkSub flags name funpat sublines) = do
  locSaved <- getLoc
  let name' = convertSubroutineName name
  let flags' = map convertSubroutineFlag flags
  setLoc (InSub name')
  funpat' <- crawlFunPat funpat
  mapM (\(vn, typ) -> declareVar (LocalVar name' vn) typ []) $ paramsOf funpat'
  declareSubroutineType name' funpat' flags'
  stm <- crawlSubLines name' sublines
  setLoc locSaved
  return (name', stm)
  where
    paramsOf (FunPat is o) = (is >>= paramsOfPat) ++ paramsOfPat o
    paramsOfPat pat =
      case pat of
        PSingle param -> [param]
        PTuple pats -> pats >>= paramsOfPat

crawlSubLines :: SubroutineName -> [Ast.SubLine] -> Arr Stm
crawlSubLines sub sublines = do
  (_, stmss) <- seqWithExtraState (crawlSubLine sub) True sublines
  let stms = concat stmss
  -- when (not $ endsWithReturn stms) $ warn $ warnNoReturn sub
  return $ smartSeq stms
  {-
  where
    endsWithReturn stms = (not $ null stms) && (isReturn $ head $ reverse stms)
    isReturn stm =
      case stm of
        Return -> True
        _ -> False
  -}

crawlSubLine :: SubroutineName -> Bool -> Ast.SubLine -> Arr (Bool, [Stm])
crawlSubLine sub declsAllowed subline =
  case subline of
    Ast.SubVarDecl vdecl -> do
      when (not declsAllowed) $ yell $ errForbiddenDecl vdecl
      crawlVarDecl vdecl (Just sub)
      return (True, [])  -- True so error only raised once
    Ast.SubStm stm -> do
      stm' <- crawlStm sub stm
      return (False, [stm'])

crawlStm :: SubroutineName -> Ast.Stm -> Arr Stm
crawlStm sub stm =
  case stm of
    Ast.Normal (Ast.Assign ass) -> crawlAssignish sub ass
    Ast.Normal Ast.Return -> return Return
    Ast.Normal (Ast.Goto lbl) -> return $ Goto $ convertGotoLabel lbl
    Ast.Normal (Ast.HintPower power action) -> do
      action' <- crawlActionName action
      let power' = convertPower power
      return $ Do $ HintPower action' power'
    Ast.Abnormal (Ast.Label lbl) -> return $ Label $ convertGotoLabel lbl
    Ast.Abnormal Ast.Scissors -> return $ Do Scissors
    Ast.Abnormal (Ast.IfLike (Ast.IfElse exp blk1 postElse)) -> do
      exp' <- crawlExp sub exp
      blk1' <- crawlStmBlock sub blk1
      alt <- case postElse of
        Ast.ElseIf ifLike -> crawlStm sub (Ast.Abnormal (Ast.IfLike ifLike))
        Ast.ElseBlock blk2 -> crawlStmBlock sub blk2
      return $ Branch exp' blk1' alt
    Ast.Abnormal (Ast.IfLike (Ast.IfOnly exp blk)) -> do
      exp' <- crawlExp sub exp
      blk' <- crawlStmBlock sub blk
      return $ Branch exp' blk' Nop
    Ast.Abnormal (Ast.While exp blk) -> do
      exp' <- crawlExp sub exp
      blk' <- crawlStmBlock sub blk
      return $ While exp' blk'
    Ast.Abnormal (Ast.For ass1 exp ass2 blk) -> do
      ass1' <- crawlAssignish sub ass1
      exp' <- crawlExp sub exp
      ass2' <- crawlAssignish sub ass2
      blk' <- crawlStmBlock sub blk
      return $ For ass1' exp' ass2' blk'
    Ast.Abnormal (Ast.ForEach vn exp1 exp2 blk) -> do
      var <- resolveVar sub (convertVarName vn)
      exp1' <- crawlExp sub exp1
      exp2' <- crawlExp sub exp2
      blk' <- crawlStmBlock sub blk
      return $ ForEach var exp1' exp2' blk'
    Ast.Abnormal (Ast.Block blk) -> crawlStmBlock sub blk

crawlStmBlock :: SubroutineName -> Ast.StmBlock -> Arr Stm
crawlStmBlock sub (Ast.MkBlock stms) = do
  stms' <- mapM (crawlStm sub) stms
  return $ smartSeq stms'

smartSeq :: [Stm] -> Stm
smartSeq stms =
  case stms of
    [] -> Nop
    _ -> foldr1 Seq stms

crawlAssignish :: SubroutineName -> Ast.Assignish -> Arr Stm
crawlAssignish sub ass = do
  (lhs, rhs) <- getSides
  return $ Do $ Assign lhs rhs
  where
    getSides =
      case ass of
        Ast.WithLhs exp1 exp2 -> do
          lhs <- crawlLhs sub exp1
          rhs <- crawlExp sub exp2
          return (lhs, rhs)
        Ast.WithoutLhs exp -> do
          rhs <- crawlExp sub exp
          return (LhsIgnore, rhs)
        Ast.PlusEquals exp1 exp2 -> do
          lhs <- crawlLhs sub exp1
          lhs' <- crawlExp sub exp1
          rhs <- crawlExp sub exp2
          return (lhs, (untyped $ EOpApp Plus [lhs', rhs]))
        Ast.MinusEquals exp1 exp2 -> do
          lhs <- crawlLhs sub exp1
          lhs' <- crawlExp sub exp1
          rhs <- crawlExp sub exp2
          return (lhs, (untyped $ EOpApp Minus [lhs', rhs]))
        Ast.Increment exp -> do
          lhs <- crawlLhs sub exp
          lhs' <- crawlExp sub exp
          let rhs = untyped $ EInt 1
          return (lhs, (untyped $ EOpApp Plus [lhs', rhs]))
        Ast.Decrement exp -> do
          lhs <- crawlLhs sub exp
          lhs' <- crawlExp sub exp
          let rhs = untyped $ EInt 1
          return (lhs, (untyped $ EOpApp Minus [lhs', rhs]))

crawlLhs :: SubroutineName -> Ast.Exp -> Arr Lhs
crawlLhs sub exp =
  case exp of
    Ast.EIgnore -> return LhsIgnore
    Ast.EVar vn -> do
      var <- resolveVar sub (convertVarName vn)
      return $ LhsVarField var []
    Ast.EField e f -> do
      let f' = convertFieldName f
      lhs <- crawlLhs sub e
      case lhs of
        LhsVarField v fs -> return $ LhsVarField v (fs ++ [f'])
        _ -> do
          yell $ errFieldOfBad f' exp
          return LhsIgnore
    Ast.ETupleZero -> return $ LhsTuple []
    Ast.ETupleTwoPlus e es -> do
      lhss <- mapM (crawlLhs sub) (e:es)
      return $ smartLhsTuple lhss
    _ -> do
      yell $ errLhsBad exp
      return LhsIgnore

crawlVarDecl :: Ast.VarDecl -> Maybe SubroutineName -> Arr ()
crawlVarDecl (Ast.MkVarDecl typ names) msub = do
  typ' <- crawlType typ
  let names' = map convertVarName names
  let vars = map foo names'
  mapM_ (\v -> declareVar v typ' []) vars
  where
    foo = maybe GlobalVar LocalVar msub

crawlFunPat :: Ast.FunPat -> Arr FunPat
crawlFunPat fp =
  case fp of
    Ast.FunPatPat p -> do
      p' <- crawlPattern p
      return $ FunPat [] p'
    Ast.FunPatFun p fp1 -> do
      p' <- crawlPattern p
      (FunPat ins out) <- crawlFunPat fp1
      return $ FunPat (p':ins) out

crawlPattern :: Ast.Pattern -> Arr Pattern
crawlPattern pat =
  case pat of
    Ast.PatternParam parm -> do
      parm' <- crawlParam parm
      return $ PSingle parm'
    Ast.PatternTupleEmpty -> return $ PTuple []
    Ast.PatternTupleTwoPlus pat0 pats -> do
      pats' <- mapM crawlPattern (pat0:pats)
      return $ PTuple pats'

crawlParam :: Ast.Param -> Arr Param
crawlParam (Ast.MkParam name typ) = do
  let name' = identToVarName name
  typ' <- crawlType typ
  return (name', typ')

crawlType :: Ast.Type -> Arr Type
crawlType t =
  case t of
    Ast.FunType t1 t2 -> do
      t1' <- crawlType t1
      t2' <- crawlType t2
      return $ Fun t1' t2'
    Ast.NamedType ident -> do
      let tn = identToTypeName ident
      aliases <- getAliases
      case M.lookup tn aliases of
        Just typ -> return typ
        Nothing -> do
          env <- getEnv
          when (not $ existsType env tn) $ yell $ errNo "type" tn
          return $ NamedType tn
    Ast.TupleZeroType -> return $ smartTupleType []
    Ast.TupleTwoPlusType t0 ts -> do
      typs <- mapM crawlType (t0:ts)
      return $ smartTupleType typs


----------------------------------------
-- Expressions, define expansion, variable resolution
----------------------------------------

-- Extract an expression, expand any macros in it, and then resolve its
-- variables.
crawlExp :: SubroutineName -> Ast.Exp -> Arr Exp
crawlExp sub astexp = do
  exp0 <- crawlExpNoExpandNoResolve astexp
  exp1 <- expand exp0
  exp2 <- resolve sub exp1
  return exp2

-- Extract an expression. Expands define-macros but doesn't attempt variable
-- resolution.
crawlExpNoResolve :: Ast.Exp -> Arr Exp
crawlExpNoResolve astexp = do
  exp0 <- crawlExpNoExpandNoResolve astexp
  exp1 <- expand exp0
  return exp1

-- Extract an expression without expanding define-macros. Does not check that
-- variables are declared, but does check that types are.
crawlExpNoExpandNoResolve :: Ast.Exp -> Arr Exp
crawlExpNoExpandNoResolve astexp =
  case astexp of
    Ast.EIfThenElse a1 a2 a3 -> do
      res <- recurses [a1,a2,a3]
      case res of
        [e1,e2,e3] -> give $ EIfThenElse e1 e2 e3
    Ast.EOr    a1 a2 -> oper [a1,a2] Or
    Ast.EAnd   a1 a2 -> oper [a1,a2] And
    Ast.EXor   a1 a2 -> oper [a1,a2] Xor
    Ast.EEq    a1 a2 -> oper [a1,a2] Eq
    Ast.ENeq   a1 a2 -> oper [a1,a2] Neq
    Ast.ELt    a1 a2 -> oper [a1,a2] Lt
    Ast.EGt    a1 a2 -> oper [a1,a2] Gt
    Ast.ELte   a1 a2 -> oper [a1,a2] Lte
    Ast.EGte   a1 a2 -> oper [a1,a2] Gte
    Ast.EPlus  a1 a2 -> oper [a1,a2] Plus
    Ast.EMinus a1 a2 -> oper [a1,a2] Minus
    Ast.ETimes a1 a2 -> oper [a1,a2] Times
    Ast.EDiv   a1 a2 -> oper [a1,a2] Div
    Ast.EMod   a1 a2 -> oper [a1,a2] Mod
    Ast.EShiftL      a1 a2 -> oper [a1,a2] ShiftL
    Ast.EShiftR      a1 a2 -> oper [a1,a2] ShiftR
    Ast.EArithShiftR a1 a2 -> oper [a1,a2] ArithShiftR
    Ast.ENot   a1 -> oper [a1] Not
    Ast.ECall call -> crawlCall call
    Ast.EApp a1 a2 -> do
      e1 <- recurse a1
      e2 <- recurse a2
      give $ EApp e1 e2
    Ast.EVar av -> give $ EVar $ UnresolvedVar (convertVarName av)
    Ast.EIntLit i -> give $ EInt i
    Ast.EBlob s -> give $ EBlob s
    Ast.EField a f -> do
      e <- recurse a
      give $ EField (convertFieldName f) e
    Ast.ETupleZero -> give $ getInner $ smartTuple []
    Ast.ETupleTwoPlus a0 as -> do
      es <- recurses (a0:as)
      give $ getInner $ smartTuple es
    Ast.EProj a i -> do
      e <- recurse a
      give $ EProj (fromIntegral i) e
    Ast.ETypeAnn a at -> do
      e <- recurse a
      t <- crawlType at
      give $ ETypeAnn t e
    Ast.EIgnore -> scream $ errIgnoreExp
    Ast.EStr s -> give $ EStr s
    Ast.EDict (Ast.MkDict assigs) -> do
      assocList <- forM assigs (\(Ast.MkDictAssig k a) -> do
        e <- recurse a
        return (identToString k, e))
      give $ EDict $ M.fromList assocList
  where
    recurse = crawlExpNoExpandNoResolve
    recurses = mapM recurse
    give = return . untyped
    oper args op = do
      operands <- recurses args
      give $ EOpApp op operands

crawlCall :: Ast.Call -> Arr Exp
crawlCall call =
  case call of
    Ast.ActionCall name args -> do
      name' <- crawlActionName name
      args' <- mapM crawlExpNoExpandNoResolve $ unargs args
      return $ untyped $ EActionCall name' args'
    Ast.SubCall name args -> do
      let name' = convertSubroutineName name
      args' <- mapM crawlExpNoExpandNoResolve $ unargs args
      return $ untyped $ ESubCall name' args'
  where
    unargs (Ast.MkArgs exps) = exps

crawlActionName :: Ast.ActionName -> Arr ActionName
crawlActionName name = do
  let name' = convertActionName name
  env <- getEnv
  when (isNothing $ findActionInfo env name') $ yell $ errNo "action" name'
  return name'

-- Expand all "define" macros in an expression. Does so in outermost-leftmost
-- (i.e., normal) order.
expand :: Exp -> Arr Exp
expand = flip expandSpine []

-- Helper for expand. Arguments: an expression, and a list of arguments it is
-- viewed as being applied to via implicit EApp nodes (typically the empty list
-- to begin with; this grows as the spine of the expression tree is traversed).
--
-- Algorithm: A handy observation when expanding an application spine is that
-- the spine only changes while its head is a define-macro and there are enough
-- arguments for an expansion to take place. Once that is no longer the case,
-- recursive expansions of its head and arguments cannot possibly bring it back
-- to a point where it can be expanded further: if the head is not a define-
-- macro it won't become one when expanded, and if it is a define-macro but
-- there are insufficient arguments on the spine, this will not change either.
-- So once a spine is "stuck", it stays so, and we don't need to revisit it
-- after expanding the subexpressions on it.
expandSpine :: Exp -> [Exp] -> Arr Exp
expandSpine exp spine =
  case getInner exp of
    EVar (UnresolvedVar vn) -> do
      defs <- getDefines
      case M.lookup vn defs of
        Just (params, replacement) ->
          let arity = length params
          in if arity <= length spine
            then do
              let used = take arity spine
              let remaining = drop arity spine
              let mapping = M.fromList $ zip params used
              replacement' <- crawlExpNoExpandNoResolve replacement
              let replacement'' = substitute mapping replacement'
              expandSpine replacement'' remaining
            else
              spineDone
        Nothing -> spineDone
    EApp e1 e2 -> expandSpine e1 (e2:spine)
    _ -> spineDone
  where
    spineDone = do
      exp' <- untoChildren expand exp   -- expand subexpressions within head
      spine' <- mapM expand spine       -- expand each argument on the spine
      return $ foldl app exp' spine'
    app e1 e2 = untyped $ EApp e1 e2

-- Traverse an expression and replace several variables with other expressions.
-- (Substitution is not carried out on these replacements.)
substitute :: M.Map VarName Exp -> Exp -> Exp
substitute m exp =
  case getInner exp of
    EVar (UnresolvedVar vn) ->
      case M.lookup vn m of
        Just exp' -> exp'
        Nothing -> exp
    _ -> uponChildren (substitute m) exp

-- Resolve the variable names within an expression.
resolve :: SubroutineName -> Exp -> Arr Exp
resolve sub exp =
  case getInner exp of
    EVar (UnresolvedVar vn) -> do
      var <- resolveVar sub vn
      replace $ EVar var
    _ -> untoChildren (resolve sub) exp
  where
    replace inner' = return $ Exp inner' (getExpLabel exp)

-- Resolve a single variable name.
resolveVar :: SubroutineName -> VarName -> Arr Var
resolveVar sub vn = do
  env <- getEnv
  let global = GlobalVar vn
  let local = LocalVar sub vn
  if existsVar env local
    then return local
    else if existsVar env global
      then return global
      else do
        yell $ errNo "variable" vn
        return $ UnresolvedVar vn

----------------------------------------
-- Declarations
----------------------------------------

declareAction
  :: ActionName
  -> ActionType
  -> Integer
  -> ActionProvider
  -> Protocol
  -> Arr ()
declareAction name typ array prov proto = do
  env <- getEnv
  let actionInfos = env_actionInfos env
  let actions = env_actions env
  let info = ActionInfo typ proto prov array
  let actionInfos' = M.insert name info actionInfos
  let actions' = actions ++ [name]
  putEnv $ env { env_actions = actions', env_actionInfos = actionInfos' }

declareActionType :: ActionTypeName -> ActionType -> Arr ()
declareActionType name typ = do
  st <- get
  let types = arrs_action_types st
  when (M.member name types) $ yell $ errRedeclare "action type" name
  let types' = M.insert name typ types
  put $ st { arrs_action_types = types' }

declareDefine :: VarName -> [VarName] -> Ast.Exp -> Arr ()
declareDefine name params replacement = do
  st <- get
  let defs = arrs_defs st
  when (M.member name defs) $ yell $ errRedeclare "define-macro" name
  let defs' = M.insert name (params, replacement) defs
  put $ st { arrs_defs = defs' }

declareFields :: TypeName -> [(Field, Type)] -> Arr ()
declareFields name defs = do
  env <- getEnv
  when (isJust $ findStruct env name) $ yell $
    errRedeclare "fields definitions for" name
  when (not $ existsType env name) $ yell $ errFieldsWithoutType name
  let dups = duplicates $ map fst defs
  mapM_ (yell . (errRedeclare $ "field " ++ name ++ "-->")) dups
  let structs = env_structs env
  let structs' = M.insert name (Struct $ M.fromList defs) structs
  putEnv $ env { env_structs = structs' }

declareFlavor :: Flavor -> Arr ()
declareFlavor flavor = do
    env <- getEnv
    putEnv $ env { env_flavor = Just flavor }

declareName :: ProgramName -> Arr ()
declareName name = do
  st <- get
  let env = arrs_env st
  let old = env_name env
  when (arrs_has_name st) $ yell $ errRename old name
  let env' = env { env_name = name }
  put $ st { arrs_env = env', arrs_has_name = True }

declareNumeric :: TypeName -> Integer -> Arr ()
declareNumeric name imod = do
  env <- getEnv
  when (isJust $ M.lookup name $ env_numeric env) $ yell $
    errRedeclare "numeric information about" name
  when (not $ existsType env name) $ yell $ errNumericWithoutType name
  let numeric' = M.insert name (UnsignedNumeric imod) $ env_numeric env
  putEnv $ env { env_numeric = numeric' }

declareProtocol :: Protocol -> Arr ()
declareProtocol proto = do
  st <- get
  let env = arrs_env st
  let old = env_protocol env
  when (arrs_has_protocol st) $ yell $ errReProtocol old proto
  let env' = env { env_protocol = proto }
  put $ st { arrs_env = env', arrs_has_protocol = True }

declareStateAspect :: GlobalStateAspect -> Arr ()
declareStateAspect aspect = do
  env <- getEnv
  let aspects = env_stateAspects env
  when (aspect `elem` aspects) $ yell $ errRedeclare "state aspect" aspect
  let aspects' = S.insert aspect aspects
  let env' = env { env_stateAspects = aspects' }
  putEnv env'

declareSubroutineType :: SubroutineName -> FunPat -> [SubroutineFlag] -> Arr ()
declareSubroutineType name funpat flags = do
  when (flags `notElem` [[], [Inline]]) $ yell $
    errInvalidSubroutineFlags name flags
  env <- getEnv
  let ts = env_subTypes env
  when (M.member name ts) $ yell $ errRedeclare "subroutine" name
  let ts' = M.insert name (SubroutineType funpat flags) ts
  let env' = env { env_subTypes = ts' }
  putEnv env'

declareType :: TypeName -> Maybe String -> Arr ()
declareType name compdef = do
  env <- getEnv
  let ts = env_types env
  when (M.member name ts) $ yell $ errRedeclare "type" name
  let ts' = M.insert name compdef ts
  let env' = env { env_types = ts' }
  putEnv env'

declareTypeAlias :: TypeName -> Type -> Arr ()
declareTypeAlias name typ = do
  st <- get
  let aliases = arrs_aliases st
  when (M.member name aliases) $ yell $ errRedeclare "type alias" name
  let aliases' = M.insert name typ aliases
  put $ st { arrs_aliases = aliases' }

declareVar :: Var -> Type -> [VarFlag] -> Arr ()
declareVar var typ flags = do
  env <- getEnv
  let vars = env_vars env
  when (M.member var vars) $ yell $ errRedeclare "var" var
  let vars' = M.insert var (VarInfo typ flags) vars
  let env' = env { env_vars = vars' }
  putEnv env'

      
----------------------------------------
-- Simple conversions
----------------------------------------

convertActionName :: Ast.ActionName -> ActionName
convertActionName (Ast.MkActionName ident) = identToString ident

convertActionTypeName :: Ast.ActionTypeName -> ActionTypeName
convertActionTypeName (Ast.MkActionTypeName ident) = identToString ident

convertDefineName :: Ast.DefineName -> VarName
convertDefineName (Ast.MkDefineName ident) = identToString ident

convertDefineParam :: Ast.DefineParam -> VarName
convertDefineParam (Ast.MkDefineParam ident) = identToString ident

convertFieldName :: Ast.FieldName -> Field
convertFieldName fn =
  case fn of
    Ast.FieldNameIdent ident -> identToString ident
    Ast.FieldNameString s -> s

convertGotoLabel :: Ast.GotoLabel -> GotoLabel
convertGotoLabel (Ast.MkGotoLabel ident) = identToString ident

convertPower :: Ast.Power -> Power
convertPower pow =
  case pow of
    Ast.PowerOn -> PowerOn
    Ast.PowerOff -> PowerOff

--convertPowerProtocol :: Ast.PowerProtocol -> PowerProtocol
--convertPowerProtocol pc =
--  case pc of
--    Ast.PowerProtocolAlwaysOn -> AlwaysOn
--    Ast.PowerProtocolPowerShake -> PowerShake


convertStateAspect :: Ast.StateAspect -> GlobalStateAspect
convertStateAspect (Ast.MkStateAspect ident) = identToString ident

convertSubroutineFlag :: Ast.SubFlag -> SubroutineFlag
convertSubroutineFlag f =
  case f of
    Ast.Inline -> Inline

convertSubroutineName :: Ast.SubroutineName -> SubroutineName
convertSubroutineName (Ast.MkSubroutineName ident) = identToString ident

convertTypeName :: Ast.TypeName -> TypeName
convertTypeName (Ast.MkTypeName ident) = identToTypeName ident

convertVarName :: Ast.VarName -> VarName
convertVarName (Ast.MkVarName ident) = identToVarName ident

identToProgramName :: Ast.Ident -> ProgramName
identToProgramName = identToString

identToString :: Ast.Ident -> String
identToString (Ast.Ident s) = s

identToTypeName :: Ast.Ident -> TypeName
identToTypeName = identToString

identToVarName :: Ast.Ident -> VarName
identToVarName = identToString


----------------------------------------
-- State manipulation
----------------------------------------

getAliases :: Arr (M.Map TypeName Type)
getAliases = liftM arrs_aliases get

getDefines :: Arr (M.Map VarName ([VarName], Ast.Exp))
getDefines = liftM arrs_defs get

getEnv :: Arr Environment
getEnv = liftM arrs_env get

getLoc :: Arr Location
getLoc = liftM arrs_loc get

putEnv :: Environment -> Arr ()
putEnv env = modify $ \st -> st { arrs_env = env }

setLoc :: Location -> Arr ()
setLoc loc = modify $ \st -> st { arrs_loc = loc }


----------------------------------------
-- Misc
----------------------------------------

duplicates :: Eq a => [a] -> [a]
duplicates list =
  case list of
    [] -> []
    (x:xs) ->
      let dups = duplicates $ filter (/= x) xs
      in if x `elem` xs
        then x:dups
        else dups

seqWithExtraState :: Monad m => (a -> b -> m (a, c)) -> a -> [b] -> m (a, [c])
seqWithExtraState f s list =
  case list of
    [] -> return (s, [])
    (x:xs) -> do
      (s',y) <- f s x
      (s'',ys) <- seqWithExtraState f s' xs
      return (s'', y:ys)

    
----------------------------------------
-- Error and warning handling
----------------------------------------

instance Show Location where
  show loc =
    case loc of
      NoClue -> "somewhere"
      Global -> "global scope"
      InSub name -> "somewhere in " ++ name

locate :: Location -> String -> String
locate loc s = show loc ++ ": " ++ s

-- Add an error message for the current location, but don't fail.
yell :: String -> Arr ()
yell err = do
  loc <- getLoc
  logMessages [Error $ locate loc err]
  return ()

-- Add an error message for the current location and do indeed fail.
scream :: String -> Arr a
scream err = do
  yell err
  failure

-- Add a warning message for the current location, don't fail.
warn :: String -> Arr ()
warn wrn = do
  loc <- getLoc
  logMessages [Warning $ locate loc wrn]
  return ()


----------------------------------------
-- Error and warning messages
----------------------------------------

errArraySize :: Exp -> String
errArraySize exp = "couldn't figure out array size: " ++ show exp

errBadFlavor :: String -> String
errBadFlavor s = "not a valid flavor: " ++ s

errBadProtocol :: Exp -> String
errBadProtocol exp = "not a valid protocol description: " ++ show exp

errDefineArgs :: VarName -> String
errDefineArgs name = "not enough args given to " ++ show name

errFieldOfBad :: Show a => Field -> a -> String
errFieldOfBad thing field =
  "left hand side of an assignment tries to refer to field " ++ show field ++
  " of something other than a variable (or a variable's field): " ++ show thing

errFieldsWithoutType :: TypeName -> String
errFieldsWithoutType name =
  "fields for type " ++ show name ++ " declared before the type"

errForbiddenDecl :: Show a => a -> String
errForbiddenDecl thing =
  "declaration interspersed with non-declaration code: " ++ show thing

errIgnoreExp :: String
errIgnoreExp = "the placeholder _ may not be used inside of expressions"

errInvalidSubroutineFlags :: Show a => SubroutineName -> a -> String
errInvalidSubroutineFlags name flags =
  "invalid flags for subroutine " ++ show name ++ ": " ++ show flags

errLhsBad :: Show a => a -> String
errLhsBad thing = "Bad left-hand-side: " ++ show thing

errNo :: Show a => String -> a -> String
errNo variety thing =
  "reference to undeclared " ++ variety ++ " " ++ show thing

errNoAspect :: GlobalStateAspect -> String
errNoAspect aspect = "State aspect not declared: " ++ show aspect

errNoName :: String
errNoName = "program is missing a 'name' declaration"

errNoMain :: String
errNoMain = "program is missing a '" ++ mainSubroutineName ++ "' subroutine"

errNoProtocol :: String
errNoProtocol = "program is missing a 'protocol' declaration"

errNotStatic :: Exp -> String
errNotStatic exp = "cannot be statically evaluated: " ++ show exp

errNumericWithoutType :: TypeName -> String
errNumericWithoutType name =
  "numeric information for type " ++ show name ++ " declared before the type"

errRedeclare :: Show a => String -> a -> String
errRedeclare variety thing = variety ++ " " ++ show thing ++ " re-declared"

errRename :: ProgramName -> ProgramName -> String
errRename n1 n2 = "program given two names: " ++ show n1 ++ ", " ++ show n2

errReProtocol :: Protocol -> Protocol -> String
errReProtocol old new = "protocol declared twice: " ++
  show old ++ ", " ++ show new

{-
warnNoReturn :: SubroutineName -> String
warnNoReturn name = "subroutine " ++ show name ++
  " doesn't end with a return statement"
-}
