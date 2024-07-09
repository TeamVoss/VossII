module Liveness (liveness) where

-- Prune assignments, variables, and shared expressions that aren't live. Also
-- gives warnings about variables that are live at the entry to the program,
-- excluding inputs and the program's return address (this may produce false
-- positives under some circumstances).
--
-- Note: warnings are not given for shared combinational expressions that are
-- live at the entry to the program; I *think* these are invariably false
-- positives.

import Control.Monad
import Control.Monad.State
import qualified Data.Map as M
import qualified Data.Set as S

import Types
import Message
import Expression
import SyncProgram
import Compile
import Finite
import Graph
import PrettyPrint

type Liveable = (Thing, Version)

data Version = Incoming ActivityLabel | Outgoing ActivityLabel
  deriving (Eq, Ord)

data Thing = AVar Var | AShared SharedRef
  deriving (Eq, Ord)

liveness :: SyncProgram -> Compile SyncProgram
liveness prog = mkCompile $ \env ->
  let initial = initialState prog env
      final = execState findRootsAndFlowGraph initial
      livestuff = reachable (live_graph final) $ live_roots final
      (env', prog') = prune livestuff env prog
      messages = checkUninitialized prog livestuff
  in (messages, Just (env', prog'))

checkUninitialized :: SyncProgram -> S.Set Liveable -> [Message]
checkUninitialized prog live = S.toList trouble >>= warning
  where
    trouble = liveAtEntry `S.difference` exclude
    liveAtEntry = S.fromList (check =<< S.toList live)
    check (t, vers) = if vers == entry then [t] else []
    entry = Incoming $ bitp_entry prog
    exclude = S.fromList $ map AVar $ (bitp_ret prog):(bitp_inputs prog)
    warning t =
      case t of
        AVar v -> [Warning $ show v ++ " may not be initialized prior to use"]
        AShared i -> []  -- these don't matter... I think?

prune :: S.Set Liveable -> Environment -> SyncProgram ->
         (Environment, SyncProgram)
prune live env prog =
  let -- General stuff
      liveThings = (S.map fst live) `S.union` preserves
      isVarLive v = (AVar v) `elem` liveThings
      isSharedLive i = (AShared i) `elem` liveThings
      -- New program
      shared' = filterKey isSharedLive $ bitp_shared prog
      activities' = M.mapWithKey (pruneActivity live) $ bitp_activities prog
      prog' = prog { bitp_activities = activities', bitp_shared = shared' }
      -- New environment
      vars' = filterKey isVarLive $ env_vars env
      env' = env { env_vars = vars' }
  in (env', prog')
  where
    filterKey f = M.filterWithKey ((f .) . const)
    preserves = S.fromList $ map AVar $ bitp_inputs prog ++ bitp_outputs prog

pruneActivity :: S.Set Liveable -> ActivityLabel -> Activity -> Activity
pruneActivity live label activity =
  let updates' = M.filterWithKey keepUpdate $ actv_updates activity
      actions' = actv_actions activity
      powers'  = actv_powers activity
      next'    = actv_next activity
  in Activity updates' actions' powers' next'
  where
    keepUpdate v _ = (AVar v, Outgoing label) `elem` live


-- Liveness figuring-out monad
type Live = State LiveState

data LiveState = LiveState
  -- Unchanging state
  { live_prog :: SyncProgram
  , live_env :: Environment
  , live_domain :: S.Set Thing      -- all things (variables and shareds)
  , live_vars :: S.Set Var          -- all variables specifically
  , live_returns :: M.Map SubroutineName (S.Set ActivityLabel) -- return paths
  -- Changing state
  , live_roots :: S.Set Liveable
  , live_flows :: S.Set (Liveable, Liveable)  -- (x,y) = x depends on y
  , live_graph :: Graph Liveable  -- made from live_flows at the end
  }


initialState :: SyncProgram -> Environment -> LiveState
initialState prog env =
  LiveState prog env domain vars returns S.empty S.empty M.empty
  where
    domain  = S.map AVar vars `S.union` S.map AShared shared
    vars    = S.fromList $ M.keys $ env_vars env
    shared  = S.fromList $ M.keys $ bitp_shared prog
    returns = M.fromList $ map (\name -> (name, findReturns name)) retSubs
    -- helpers
    retSubs = S.toList vars >>=
      (\v -> case v of
               ReturnAddr sub -> [sub]
               _ -> [])
    findReturns sub =
      case M.lookup (ReturnAddr sub) possibleVarValues of
        Just set -> S.map (extractActivityLabel sub) set
        Nothing -> error $
          "internal error: could not find return destinations for " ++ show sub
    extractActivityLabel sub exp =
      case getInner exp of
        EActivityLabel label -> label
        _ -> error $ "internal error: bad return destination for " ++
          show sub ++ ": " ++ show exp
    possibleVarValues = analyzeFinite prog env

findRootsAndFlowGraph :: Live ()
findRootsAndFlowGraph = do
  prog <- getProg
  mapM_ searchShared $ M.toList $ bitp_shared prog
  mapM_ searchActivity $ M.toList $ bitp_activities prog
  createGraph

createGraph :: Live ()
createGraph = do
  domain <- liftM S.toList getDomain
  versions <- allVersions
  let nodes = [(t,v) | t <- domain, v <- versions]
  let graph = M.fromList $ map (\node -> (node, S.empty)) nodes
  flows <- getFlows
  let graph' = foldr addEdge graph flows
  putGraph graph'
  

----------------------------------------
-- Flows from shared expressions
----------------------------------------

-- Find flows from shared combinational expressions. Because they are not stored
-- in registers, for each possible version liveness simply flows to their
-- dependencies.
searchShared :: (SharedRef, Exp) -> Live ()
searchShared (index, exp) = do
  versions <- allVersions
  mapM_ (searchSharedAtVersion index exp) versions

searchSharedAtVersion :: SharedRef -> Exp -> Version -> Live ()
searchSharedAtVersion index exp vers =
  mapM_ idepend $ S.toList $ dependencies exp
  where
    idepend thing = addFlow this (thing, vers)
    this = (AShared index, vers)


----------------------------------------
-- Flows from variables, and roots
----------------------------------------

-- For every activity:
-- 1. Updated variables create a flow from the outgoing version of the variable
--    to the incoming versions of its dependencies.
-- 2. Non-updated variables create a flow from the outgoing version of the
--    variable to the incoming version.
-- 3. For every variable there is a flow from the incoming version in each
--    successor activity to the outgoing version from the current activity.
-- 4. The outgoing versions of output variables are roots if the activity might
--    finish the run of the program.
-- 5. For action arguments and conditions, the incoming versions of their
--    dependencies are roots.
-- 6. For action power settings, the incoming versions of their conditions'
--    dependencies are roots.
-- 7. The incoming versions of dependencies of the next activity are roots.
searchActivity :: (ActivityLabel, Activity) -> Live ()
searchActivity (label, activity) = do
  searchUpdates label activity
  searchSuccessors label activity
  searchOutputVars label activity
  searchActions label activity
  searchNextLogic label activity

-- 1 and 2 from the list above.
searchUpdates :: ActivityLabel -> Activity -> Live ()
searchUpdates label activity = getVars >>= mapM_ foo
  where
    foo v = mapM_ (flow v) (updatedDeps v)
    flow v t = addFlow (AVar v, Outgoing label) (t, Incoming label)
    updatedDeps v =
      case M.lookup v $ actv_updates activity of
        Just exp -> dependencies exp
        Nothing -> S.singleton (AVar v)

-- 3 from the list.
searchSuccessors :: ActivityLabel -> Activity -> Live ()
searchSuccessors label activity = do
  sucs <- liftM S.toList $ successors activity
  vars <- liftM S.toList getVars
  sequence_ [varToSuc v l | v <- vars, l <- sucs]
  where
    varToSuc var label' =
      let thing = AVar var
      in addFlow (thing, Incoming label') (thing, Outgoing label)

-- 4 from the list.
searchOutputVars :: ActivityLabel -> Activity -> Live ()
searchOutputVars label activity = do
  mf <- activityMightFinish activity
  if mf
    then do
      prog <- getProg
      let ovars = bitp_outputs prog
      let olives = map (\v -> (AVar v, Outgoing label)) ovars
      mapM_ addRoot olives
    else return ()

-- 5 from the list.
searchActions :: ActivityLabel -> Activity -> Live ()
searchActions label = mapM_ foo . M.elems . actv_actions
  where
    foo tree =
      case tree of
        ADepends exp t1 t2 -> do
          mapM_ root $ dependencies exp
          foo t1
          foo t2
        ACall args -> mapM_ (mapM_ root . dependencies) args
        ADontCall -> return ()
    root t = addRoot (t, Incoming label)

-- 6 from the list.
searchPowers :: ActivityLabel -> Activity -> Live ()
searchPowers label = mapM_ foo . M.elems . actv_powers
  where
    foo tree =
      case tree of
        PowDepends exp t1 t2 -> do
          mapM_ root $ dependencies exp
          foo t1
          foo t2
        PowSet _ -> return ()
        PowDontSet -> return ()
    root t = addRoot (t, Incoming label)

-- 7 from the list.
searchNextLogic :: ActivityLabel -> Activity -> Live ()
searchNextLogic label = mapM_ root . dependencies . actv_next
  where
    root t = addRoot (t, Incoming label)




----------------------------------------
-- Helpers
----------------------------------------

successors :: Activity -> Live (S.Set ActivityLabel)
successors = foo . actv_next
  where
    foo :: Exp -> Live (S.Set ActivityLabel)
    foo exp =
      case getInner exp of
        EVar (ReturnAddr sub) -> getReturnDestinations sub
        EActivityLabel label -> return $ S.singleton label
        EIfThenElse _ e1 e2 -> do
          ls1 <- foo e1
          ls2 <- foo e2
          return $ ls1 `S.union` ls2
        _ -> error $ "internal error: bad next activity: " ++ show exp

-- Might the entire program finish after this activity?
activityMightFinish :: Activity -> Live Bool
activityMightFinish activity = do
  prog <- getProg
  let ret = bitp_ret prog
  return $ nextMightBeVar ret $ actv_next activity
  where
    nextMightBeVar v exp =
      case getInner exp of
        EVar v' -> v == v'
        EActivityLabel _ -> False
        EIfThenElse _ e1 e2 -> nextMightBeVar v e1 || nextMightBeVar v e2
        _ -> error $
          "internal error: unexpected form of 'actv_next': " ++ show exp

-- Every single possible Version.
allVersions :: Live [Version]
allVersions = do
  prog <- getProg
  return $ (M.keys $ bitp_activities prog) >>=
    \label -> [Incoming label, Outgoing label]

-- The Things that can be found in an expression tree.
dependencies :: Exp -> S.Set Thing
dependencies exp =
  case getInner exp of
    EVar v -> S.singleton $ AVar v
    EShared index -> S.singleton $ AShared index
    _ -> S.unions $ map dependencies $ children exp


----------------------------------------
-- State stuff
----------------------------------------

-- addFlow x y records that x depends on y (so liveness of x flows to y).
addFlow :: Liveable -> Liveable -> Live ()
addFlow from to = modify $
  \st -> st { live_flows = S.insert (from, to) $ live_flows st }

addRoot :: Liveable -> Live ()
addRoot root = modify $
  \st -> st { live_roots = S.insert root $ live_roots st }

getDomain :: Live (S.Set Thing)
getDomain = liftM live_domain get

getEnv :: Live Environment
getEnv = liftM live_env get

getFlows :: Live (S.Set (Liveable, Liveable))
getFlows = liftM live_flows get

getProg :: Live SyncProgram
getProg = liftM live_prog get

getVars :: Live (S.Set Var)
getVars = liftM live_vars get

getReturnDestinations :: SubroutineName -> Live (S.Set ActivityLabel)
getReturnDestinations sub = do
  st <- get
  case M.lookup sub $ live_returns st of
    Just set -> return set
    Nothing -> error $
      "internal error: missing return destinations for: " ++ show sub

putGraph :: Graph Liveable -> Live ()
putGraph g = modify $ \st -> st { live_graph = g }
