module Plan (plan) where

import Data.List (findIndex)
import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad
import Control.Monad.State

import Types
import Properties
import Compile
import Environment
import Expression
import ExpressionExtra
import PrettyPrint
import BlockProgram


----------------------------------------
-- Public
----------------------------------------

plan :: BlockProgram -> Compile SyncProgram
plan blop = mkCompile $ \env ->
  let (env', bitp) = plan' env blop
  in ([], Just (env', bitp))


----------------------------------------
-- Types
----------------------------------------

-- The state of planning. Kept the typo, as pr sounds like a trendy website.
data PlanrState = PlanrState
  { ps_env :: Environment    -- environment, remains unchanged
  , ps_blop :: BlockProgram  -- input program
  , ps_bitp :: SyncProgram   -- output program
  , ps_cache :: M.Map (Block, RetStack) ActivityLabel  -- compiled/compiling
  , ps_available :: M.Map String Integer  -- for labels; see reserveActivityLabel
  , ps_expansions :: M.Map ActionName [ActionName]  -- array action -> individ.
  }

type Plan = State PlanrState

-- Information about the code path being compiled. Does a few things:
-- (1) Keeps track of action calls and updated variable values, which eventually
--     make up a compiled activity for this path (pedantic note: which is in turn
--     merged with activities from sibling paths if we're in a branch statement).
-- (2) Carries the information needed to know whether a subsequent statement can
--     be fit into the same activity, or if we need to begin a new one.
-- (3) Remembers the current block label so that we can give subsequent activities
--     names based thereupon, making things easier to debug.
data PathInfo = PathInfo
  { path_hint :: BlockLabel  -- approximate block label => nice activity names
  , path_tainted :: Bool  -- is this path predicated upon a action result?
  , path_stateRead :: S.Set GlobalStateAspect   -- global state read
  , path_stateWrite :: S.Set GlobalStateAspect  -- global state written
  , path_actionCalls :: M.Map ActionName [Exp]  -- action name => args
  , path_vars :: M.Map Var Exp                  -- variable => new value
  , path_power :: M.Map ActionName Power        -- action power hintts
  }

-- When compiling inlined subroutines we need to remember what to return to, so
-- we keep a stack of continuations.
type RetStack = [Ret]

-- Each continuation consists of a block and its approximate label (used as a
-- hint when new activity names are created).
data Ret = Ret
  { ret_block :: Block
  , ret_hint :: BlockLabel
  }
  deriving (Eq, Ord)


----------------------------------------
-- Top-level stuff
----------------------------------------

plan' :: Environment -> BlockProgram -> (Environment, SyncProgram)
plan' env blop = (env', bitp')
  where
    (env', expansions) = expandActionArrays $ createReturnAddresses env
    bitp' = (ps_bitp final) { bitp_entry = mainActivityLabel }
    (mainActivityLabel, final) = runState (planFrom mainBlockLabel) initial
    mainBlockLabel = mustFindSubroutineBlockLabel blop main
    initial = PlanrState env' blop bitp M.empty M.empty expansions
    bitp = SyncProgram
      mainFunPat
      inputVars
      outputVars
      (ReturnAddr main)
      standin
      M.empty
      M.empty
    inputVars  = map paramToVar $ flattenPat =<< fp_arguments mainFunPat
    outputVars = map paramToVar $ flattenPat $   fp_result mainFunPat
    paramToVar (v,_) = LocalVar main v
    main = blop_main blop
    mainFunPat = st_io $ mustFindSubroutineType env (blop_main blop)
    standin = error $ "internal error: stand-in label for entry point"


----------------------------------------
-- Compilation
----------------------------------------

-- Create return address variables
createReturnAddresses :: Environment -> Environment
createReturnAddresses env = foldr foo env subs
  where
    subs = M.keys $ env_subTypes env
    foo sub env =
      let var = ReturnAddr sub
          vi = VarInfo Address []
      in addVar var vi env

-- Launch!
planFrom :: BlockLabel -> Plan ActivityLabel
planFrom label = do
  bprog <- getBlockProg
  let block = mustFindBlock bprog label
  beginActivity label block []

-- Compiles a block, and everything that it reaches. Allocates a label for the
-- first activity of the compiled result, launches compilation (which will no doubt
-- call this same function as needed, creating more activities), and adds the
-- returned (compiled) first activity to the activity program.
--
-- Key feature: if this same block contents (and inline function return stack)
-- has been compiled before, or is in the process of being compiled, the label
-- already allocated for the compiled activity is returned, rather than launching a
-- duplicate compilation. This facilitates sharing, and ties otherwise infinite
-- loops.
--
-- Everything being untyped makes me nervous about this caching, though, since
-- expressions can reference polymorphic functions! The question then is whether
-- a /statement/ can be polymorphic, since they are the basis of blocks and
-- thus the caching. Technically, they can be due to LhsIgnore (a.k.a "_")
-- providing a polymorphic sink, but it isn't clear that polymorphism /without/
-- type ambiguity is possible, so for now I will hesitatnly trust this aggro
-- caching approach.
beginActivity :: BlockLabel -> Block -> RetStack -> Plan ActivityLabel
beginActivity labelhint block stack = do
  search <- searchCache (block, stack)
  case search of
    Just label -> return label
    Nothing -> do
      label <- reserveActivityLabel labelhint
      insertCache (block, stack) label
      let path =
            PathInfo labelhint False S.empty S.empty M.empty M.empty M.empty
      activity <- accumulate path block stack
      addActivity label activity
      return label

-- Compiles a block of code and everything it reaches; packs as much as possible
-- into the activity under construction (the developments thus far are tracked via
-- the PathInfo argument). When necessary, use beginActivity to launch compilation
-- into new activities (for code we can't fit in the present one), and finally return
-- the constructed first activity. Original flavor.
accumulate :: PathInfo -> Block -> RetStack -> Plan Activity
accumulate path (Block contents next) stack =
  case contents of
    [] -> accumulateNext path next stack
    (stm:stms) -> accumulateAtomicStm path stm (Block stms next) stack

-- Like accumulate, but looks up the block for you from its label. Sets the
-- label hint in the path as well, that way the generated activity names include the
-- name of the block and are more debugging-friendly.
accumulateFrom :: PathInfo -> BlockLabel -> RetStack -> Plan Activity
accumulateFrom path label stack = do
  bprog <- getBlockProg
  let block = mustFindBlock bprog label
  accumulate (hint label path) block stack

-- Compile one statement followed by a block (`cont`, for "continuation").
accumulateAtomicStm :: PathInfo -> AtomicStm -> Block -> RetStack -> Plan Activity
accumulateAtomicStm path astm cont stack =
  case astm of
    Assign lhs rhs ->
      case getInner rhs of
        EActionCall name args -> do
          accumulateActionCall path astm cont stack
        ESubCall name args -> do
          accumulateSubCall path lhs name args cont stack
        _ -> do
          let rhs' = rewrite path rhs
          accumulateCombAssign path lhs rhs' cont stack
    Scissors ->
      continueInNewActivity path cont stack
    HintPower action status -> do
      accumulateHintPower path action status cont stack
    RunSub name inline ->
      accumulateRunSub path name inline cont stack

-- Compile starting from the end of a block.
accumulateNext :: PathInfo -> NextBlock -> RetStack -> Plan Activity
accumulateNext path next stack =
  case next of
    NBGoto label ->
      accumulateFrom path label stack
    NBBranch cond label1 label2 -> do
      let condRw = rewrite path cond
      condShare <- share condRw
      avail <- available condShare
      let path' = if avail then path else taintPath path
      activity1 <- accumulateFrom path' label1 stack
      activity2 <- accumulateFrom path' label2 stack
      env <- getEnv
      return $ mergeActivitys env condShare activity1 activity2
    NBReturn currentSubName ->
      case stack of
        [] -> return $ makeActivity path $ (EVar $ ReturnAddr currentSubName) `ofType` Address
        (ret:rets) -> do
          let path' = hint (ret_hint ret) path
          accumulate path' (ret_block ret) rets

-- Make a activity from the current path, and link it to a new activity compiled from
-- a block. This is used, for example, when the next statement to compile can't
-- fit into the same activity the stuff covered in the path so far.
continueInNewActivity :: PathInfo -> Block -> RetStack -> Plan Activity
continueInNewActivity path block stack = do
  blabel <- beginActivity (path_hint path) block stack
  return $ makeActivity path (EActivityLabel blabel `ofType` Address)


----------------------------------------
-- Pure assignment
----------------------------------------

-- Assign to a LHS a combinational RHS (assumed to already have been rewritten
-- to take into account variable updates), and then continue compilation. Will
-- always fit the assignment into the current block--this property is required
-- by accumulateActionCall, which needs to capture the results in the same activity.
-- Handles sharing of the rhs.
accumulateCombAssign :: PathInfo -> Lhs -> Exp -> Block -> RetStack -> Plan Activity
accumulateCombAssign path lhs rhs cont stack =
  if isCombinational rhs
    then do
      let matches = matchSides lhs rhs
      path' <- foldM assign path matches
      accumulate path' cont stack
    else
      error $ "Attempt to assign non-combinational RHS: " ++
        show lhs ++ " = " ++ show rhs
  where
    -- Carry out an assignment, producing a modified PathInfo.
    assign :: PathInfo -> (Lhs, Exp) -> Plan PathInfo
    assign p (lhs, rhs) =
      case lhs of
        LhsVarField v [] -> do
          rhsShared <- share rhs
          return $ updateVar v rhsShared p
        LhsVarField v fs -> do
          -- Struct time
          env <- getEnv
          let typ = mustFindVarType env v
          let original = maybe (EVar v `ofType` typ) id $ M.lookup v (path_vars p)
          rhsShared <- share rhs
          vNew <- updatedStruct original fs rhsShared
          vNewShared <- share vNew
          return $ updateVar v vNewShared p
        LhsTuple lhss -> do
          rhsShared <- share rhs
          let (Product ts) = typeOf rhs
          let assigs = zip lhss $ [EProj i rhs `ofType` (ts !! i) | i <- [0..]]
          foldM assign p assigs
        LhsIgnore -> return p
    -- Get the new value of a struct with something inside set to a new value.
    updatedStruct orig fieldPath value =
      case fieldPath of
        [] -> return value
        (f:fs) -> do
          env <- getEnv
          let typ = typeOf orig
          let (NamedType tn) = typ
          let struct = mustFindStruct env tn
          let (Just ftype) = M.lookup f $ struct_fields struct
          origShared <- share orig
          let orig' = EField f origShared `ofType` ftype
          newFieldValue <- updatedStruct orig' fs value
          return $ EFieldUpdate f origShared newFieldValue `ofType` typ


----------------------------------------
-- Action calls
----------------------------------------

-- Attempt to compile a action call into the current activity; if impossible then
-- defer it to a new activity. As usual, takes a continuation block to compile
-- afterward.
accumulateActionCall :: PathInfo -> AtomicStm -> Block -> RetStack -> Plan Activity
accumulateActionCall path stm@(Assign lhs rhs) cont stack = do
  let (EActionCall origName args) = getInner rhs
  env <- getEnv
  name <- chooseExpandedAction path origName
  let ai = mustFindActionInfo env name
  let at = ai_type ai
  let argsRw = map (rewrite path) args
  avails <- mapM available argsRw
  let avail = and avails                      -- req 1: args ready to go
  let tainted = path_tainted path             -- req 2: not branched on unavailable signal
  let allowed = actionAllowed path name at    -- req 3: action doesn't interfere
  case (avail, tainted, allowed) of
    (True, False, Just pathWithState) -> do
      -- create bindings for the arguments because we like sharing extra
      argsShared <- mapM share argsRw
      -- add the action call (note: state stuff already added by actionAllowed)
      let calls = M.insert name argsShared $ path_actionCalls pathWithState
      let pathWithCall = pathWithState { path_actionCalls = calls }
      -- nab the results and continue
      let results = actionResultExp name at
      accumulateCombAssign pathWithCall lhs results cont stack
    _ -> do
      -- can't do it in this block, reconstitute stm and start a new one
      continueInNewActivity path (stm `before` cont) stack

-- For a non-array action, return the original action name. For an array
-- action, choose one of the individual actions that it was expanded into.
-- Priority is given to actions that are not in use.
chooseExpandedAction :: PathInfo -> ActionName -> Plan ActionName
chooseExpandedAction path name = do
  expansions <- getExpansions
  case M.lookup name expansions of
    Nothing -> return name
    Just [] -> error $ "wtf? empty expansion for array action " ++ show name
    Just names ->
      case filter (\n -> not $ M.member n $ path_actionCalls path) names of
        [] -> return $ names !! 0
        (foo:_) -> return foo

-- Determine whether a particular action can be run in the same activity as the
-- preceding path. If so, returns the path updated to reflect the state aspects
-- of the action call, but does not add the action call itself (primarily
-- because we may wish to run `share` on the argument, but we don't want to do
-- that until we know the action is indeed possible in this activity, ergo we don't
-- yet know the action's argument here).
actionAllowed :: PathInfo -> ActionName -> ActionType -> Maybe PathInfo
actionAllowed path name typ =
  let aSt = at_state typ
      aRead = S.fromList $ asi_stateRead aSt
      aWrite = S.fromList $ asi_stateWrite aSt
      pRead = path_stateRead path
      pWrite = path_stateWrite path
      stateOk = all S.null
        [ S.intersection pRead aWrite
        , S.intersection pWrite aRead
        , S.intersection pWrite aWrite
        ]
      alreadyRunning = name `elem` (M.keys $ path_actionCalls path)
      allowed = stateOk && not alreadyRunning
      pRead' = S.union pRead aRead
      pWrite' = S.union pWrite aWrite
      path' = path { path_stateRead = pRead', path_stateWrite = pWrite' }
  in if allowed then Just path' else Nothing


----------------------------------------
-- Subroutines
----------------------------------------

-- Compile a subroutine call assignment, e.g. `lhs = call foo arg0 arg1 ...;`.
-- Expands it into a prologue which assigns the arguments into the subroutine's
-- parameters, an actual run of the subroutine, and an epilogue that saves the
-- results. Takes a continuation block to be compiled after.
accumulateSubCall
  :: PathInfo
  -> Lhs
  -> SubroutineName
  -> [Exp]
  -> Block
  -> RetStack
  -> Plan Activity
accumulateSubCall path lhs name args cont stack = do
  env <- getEnv
  let typ = mustFindSubroutineType env name
  let inline = Inline `elem` st_flags typ
  let prologue = Assign (subParamLhs name typ) (mkTuple args)
  let run = RunSub name inline
  let epilogue = Assign lhs (subResultExp name typ)
  let everything = prologue `before` (run `before` (epilogue `before` cont))
  accumulate path everything stack

-- Compile an actual run of a subroutine, inlined or not inlined. As usual,
-- takes a continuation block to compile after.
accumulateRunSub :: PathInfo -> SubroutineName -> Bool -> Block -> RetStack -> Plan Activity
accumulateRunSub path name inline cont stack = do
  let here = path_hint path
  prog <- getBlockProg
  let subLabel = mustFindSubroutineBlockLabel prog name
  case inline of
    True -> do
      -- push continuation on stack, jump to the sub
      let stack' = (Ret cont here):stack
      accumulateFrom path subLabel stack'
    False -> do
      -- compile the stuff after the return
      ret <- beginActivity here cont stack
      -- store that into return address and goto the sub with empty stack
      let lhs = LhsVarField (ReturnAddr name) []
      let rhs = EActivityLabel ret `ofType` Address
      let jump = Block [Scissors] (NBGoto subLabel)
      accumulateCombAssign path lhs rhs jump []


----------------------------------------
-- Power
----------------------------------------

accumulateHintPower :: PathInfo -> ActionName -> Power -> Block -> RetStack -> Plan Activity
accumulateHintPower path action status cont stack =
  let path' = path { path_power = M.insert action status $ path_power path }
  in accumulate path' cont stack


----------------------------------------
-- Pure helpers
----------------------------------------

-- Make a Activity from the actions taken along a path and an expression indicating
-- which Activity should follow.
makeActivity :: PathInfo -> Exp -> Activity
makeActivity path next =
  let updates = path_vars path
      actions = M.map ACall $ path_actionCalls path
      powers = M.map PowSet $ path_power path
  in simplifyActivity $ Activity updates actions powers next

-- Merge two activities, with differences combined via a condition (true => first
-- activity, false => second activity).
mergeActivitys :: Environment -> Exp -> Activity -> Activity -> Activity
mergeActivitys env cond b1 b2 = simplifyActivity b3
  where
    b3 = Activity updates actions powers next
    updates = mergeMaps
      (\v -> EVar v `ofType` (mustFindVarType env v))
      (mux mkIfThenElse)
      (actv_updates b1)
      (actv_updates b2)
    actions = mergeMaps
      (\_ -> ADontCall)
      (mux ADepends)
      (actv_actions b1)
      (actv_actions b2)
    powers = mergeMaps
      (\_ -> PowDontSet)
      (mux PowDepends)
      (actv_powers b1)
      (actv_powers b2)
    next = mux mkIfThenElse (actv_next b1) (actv_next b2)
    mux f a b = if a == b then a else f cond a b

-- Filter out non-operative updates and power events.
simplifyActivity :: Activity -> Activity
simplifyActivity b = b
  { actv_updates = M.filterWithKey isNonTrivialUpdate $ actv_updates b
  , actv_powers = M.filter isNonTrivialPower $ actv_powers b
  }
  where
    isNonTrivialUpdate var exp =
      case getInner exp of
        EVar var' -> var /= var'
        _ -> True
    isNonTrivialPower tree = tree /= PowDontSet

-- Combine two maps.
mergeMaps
  :: Ord a
  => (a -> b)
  -> (b -> b -> c)
  -> M.Map a b
  -> M.Map a b
  -> M.Map a c
mergeMaps def merge map1 map2 = map3
  where
    map3 = M.fromList $ map (\k -> (k, merge (get k map1) (get k map2))) domain
    get k m = maybe (def k) id $ M.lookup k m
    domain = S.toList $ S.union (M.keysSet map1) (M.keysSet map2)
    unionLists xs ys = S.toList $ S.union (S.fromList xs) (S.fromList ys)

-- Set a variable's new value.
updateVar :: Var -> Exp -> PathInfo -> PathInfo
updateVar v exp p = p { path_vars = M.insert v exp $ path_vars p }

-- Rewrite an expression to take into account updated variables. It is assumed
-- that the expression comes from the supplied BlockProgram, and therefore only
-- contains certain constructors (others will produce an error).
rewrite :: PathInfo -> Exp -> Exp
rewrite path exp =
  case getInner exp of
    EVar v ->
      case M.lookup v (path_vars path) of
        Just exp' ->
          if getExpLabel exp == getExpLabel exp'
            then exp'
            else error $ "internal error: rewrite variable substitution type mismatch: " ++ show [exp,exp']
        Nothing -> exp
    _ -> uponChildren (rewrite path) exp

-- Prefix a block with an atomic statement.
before :: AtomicStm -> Block -> Block
before stm (Block stms next) = Block (stm:stms) next

-- Mark a path as tainted.
taintPath :: PathInfo -> PathInfo
taintPath p = p { path_tainted = True }

-- Set the label hint of a path (influences names of created activities, making the
-- compiled program more friendly to debugging).
hint :: BlockLabel -> PathInfo -> PathInfo
hint label path = path { path_hint = label }

-- An expression representing all of the results of a action.
actionResultExp :: ActionName -> ActionType -> Exp
actionResultExp name = foo . fp_result . at_io
  where
    foo pat =
      case pat of
        PSingle (paramName, typ) -> EActionResult name paramName `ofType` typ
        PTuple pats -> mkTuple $ map foo pats

-- A left-hand-side representing the inputs to a subroutine.
subParamLhs :: SubroutineName -> SubroutineType -> Lhs
subParamLhs name = mkLhsTuple . map foo . fp_arguments . st_io
  where
    foo pat =
      case pat of
        PSingle (paramName, _) -> LhsVarField (LocalVar name paramName) []
        PTuple pats -> mkLhsTuple $ map foo pats

-- An expression representing all of the results of a action.
subResultExp :: SubroutineName -> SubroutineType -> Exp
subResultExp name = foo . fp_result . st_io
  where
    foo pat =
      case pat of
        PSingle (paramName, typ) -> EVar (LocalVar name paramName) `ofType` typ
        PTuple pats -> mkTuple $ map foo pats


----------------------------------------
-- Stateful helpers
----------------------------------------

-- Determine whether an expression depends only on signals that are available
-- immediately, i.e. does not depends on results from non-instantaneous actions.
-- Needs the Compile monad in order to look up shared expressions. Input must be
-- a combinational expression, in the Expression.isCombinational sense.
available :: Exp -> Plan Bool
available exp =
  case getInner exp of
    EActionResult action _ -> do
      env <- getEnv
      let ai = mustFindActionInfo env action
      return $ isInstant $ ai_protocol ai
    EShared i -> do
      exp' <- getShared i
      available exp'
    _ -> do
      deps <- mapM available $ children exp
      return $ and deps

-- Make an expression shared by creating a dedicated binding for it (or find one
-- if one already exists). Returns the expression to substitute. Does not create
-- bindings for trivial expressions such as variables. For tuples (including
-- nested ones) bindings are created for each element. Only applicable to
-- combinational expressions.
share :: Exp -> Plan Exp
share exp =
  case getInner exp of
    EVar _ -> same
    EInt _ -> same
    EBlob _ -> shareit
    EActivityLabel _ -> same
    EShared _ -> same
    EActionResult _ _ -> same
    ETuple es -> untoChildren share exp
    _ -> shareit
  where
    same = return exp
    shareit = do
      i <- makeShared exp
      return $ swapInner (EShared i) exp

makeShared :: Exp -> Plan SharedRef
makeShared exp = do
  prog <- getActivityProg
  let shared = bitp_shared prog
  let freeIndex = head $ filter (flip M.notMember shared) [0..]
  case filter ((== exp) . snd) $ M.toList shared of
    ((i,_):_) -> return i
    [] -> do
      let shared' = M.insert freeIndex exp shared
      putActivityProg $ prog { bitp_shared = shared' }
      return freeIndex

getEnv :: Plan Environment
getEnv = liftM ps_env get

getBlockProg :: Plan BlockProgram
getBlockProg = liftM ps_blop get

getShared :: SharedRef -> Plan Exp
getShared i = do
  prog <- getActivityProg
  case M.lookup i $ bitp_shared prog of
    Just exp -> return exp
    Nothing -> error $
      "internal error: invalid shared expression reference: " ++ show i

getExpansions :: Plan (M.Map ActionName [ActionName])
getExpansions = liftM ps_expansions get

getActivityProg :: Plan SyncProgram
getActivityProg = liftM ps_bitp get

putActivityProg :: SyncProgram -> Plan ()
putActivityProg p = do
  ps <- get
  put $ ps { ps_bitp = p }

searchCache :: (Block, RetStack) -> Plan (Maybe ActivityLabel)
searchCache query = liftM (M.lookup query . ps_cache) get

insertCache :: (Block, RetStack) -> ActivityLabel -> Plan ()
insertCache query result = modify $
  \ps -> ps { ps_cache = M.insert query result (ps_cache ps) }

addActivity :: ActivityLabel -> Activity -> Plan ()
addActivity label activity = do
  p <- getActivityProg
  let activities = bitp_activities p
  case M.lookup label activities of
    Just _ -> error $ "Attempt to overwrite activity " ++ show label
    Nothing -> putActivityProg $ p { bitp_activities = M.insert label activity activities }

-- Find an available ActivityLabel similar to a BlockLabel. Uses the name from the
-- input BlockLabel, and finds the lowest integer that makes it unique.
reserveActivityLabel :: BlockLabel -> Plan ActivityLabel
reserveActivityLabel (name, _) = do
  ps <- get
  let available = ps_available ps
  let i = maybe 0 id $ M.lookup name available
  let available' = M.insert name (i + 1) available
  put $ ps { ps_available = available' }
  return (name, i)
