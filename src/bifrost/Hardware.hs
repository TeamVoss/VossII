module Hardware (hardware) where

{-

ACTION MANAGER MODULES

Note: the "release" wire has since been replaced by its complement "hold".

For each handshaked action we make a "adapter module". The adapter module takes
responsibility for the action's req, ack, arguments, and results signals, and
exposes a more abstract interface to our main module: run, done, release,
h-arguments, and h-results signals. The "run" signal tells the adapter module
that we wish to run the action, the "done" signal indicates that the current run
of the action has completed, and the "release" signal tells the adapter that as
of the next cycle we are done with the entire transaction (and a new one might
begin). The h-argument and h-result signals are for communicating the arguments
and results respectively with the adapter, as opposed to directly with the
action (same contents, different timing).

Semantic information (ignoring the case of a reset signal):
- A transaction begins when the "run" signal is high, and on the previous cycle
  either: (a) the "run" signal was low, or (b) the "release" signal was high.
- A transaction corresponds to exactly one run of the associated action, using
  the arguments supplied via the h-argument signals.
- The "done" signal indicates that the relevant run of the action has completed,
  and that the h-result signals carry the action's results until the transaction
  is finished.
- A transaction finishes on the cycle where the "release" signal is high.
- The release signal may also be raised outside of a transaction, in which case
  it is ignored.

Precise timing information (ignoring the case of a reset signal):
- Once raised, the "run" signal must remain high through the cycle where the
  "release" signal is raised.
- If the "run" signal is high, the "done" signal will eventually become high
  (possibly immediately).
- Once high, the "done" signal will remain high through the cycle where the
  "release" signal is raised (possibly immediate).
- The "release" signal may only be high when the "done" signal is high, or when
  neither "run" nor "done" are high (n.b. outside of the transaction, no
  effect).
- When the "run" signal is high, the h-argument signals must be valid and
  constant through the cycle where the "release" signal is raised.
- When the "done" signal is high, the h-result signals are valid and
  remain constant through the cycle where the "release" signal is raised.

It is noted that "done" may depend without delay upon "run", and "release" may
depend without delay upon "done", allowing an entire transaction to take place
in a single cycle, and be followed by a new transaction on the next cycle.

-}

import Control.Monad
import Data.List (delete, findIndex)
import Data.Maybe (catMaybes, isJust, fromJust)
import qualified Data.Map as M
import qualified Data.Set as S

import Types
import Expression
import ExpressionExtra
import Environment
import CombOpt
import Message
import KitchenSink
import PrettyPrint
import Compile
import WireNames

type HW = KitchenSink Message HWState

data HWState = HWState
  { hws_prog :: SyncProgram
  , hws_env :: Environment
  , hws_flavor :: Flavor
  , hws_mods :: [HardwareModule]
  , hws_work :: HardwareModule  -- work in progress
  }

-- The root of all evil.
hardware :: SyncProgram -> Compile Hardware
hardware prog = mkCompile $ \env ->
  let initial = HWState prog env undefined [] (emptyModule "")
      (mname, messages, final) = runKitchenSink makeHardware initial
  in case (mname, filter isError messages) of
    (Just name, []) ->
      let hw = Hardware name (hws_mods final)
          env' = hws_env final
      in (messages, Just (env', hw))
    _ -> (messages, Nothing)

makeHardware :: HW ModuleName
makeHardware = do
  proto <- getProtocol
  env <- getEnv
  let flavor = case proto_data proto of
        Immediate _ -> Strawberry
        _           ->
            let def = Mint  -- living in the fast lane
            in case (env_flavor env) of
                Just f -> if f == Strawberry then def else f
                Nothing -> def
  modify $ \st -> st { hws_flavor = flavor }
  makeMainModule

-- Create the main module, and the adapter modules in the process.
makeMainModule :: HW ModuleName
makeMainModule = do
  env <- getEnv
  flavor <- getFlavor
  newModule $ mainModuleName env
  makeClockResetPower
  case flavor of
    Strawberry -> makeStrawberry
    _ -> makeNonStrawberry
  -- yay
  moduleDone

makeClockResetPower :: HW ()
makeClockResetPower = do
  proto <- getProtocol
  flavor <- getFlavor
  let hasClock = proto_clock proto
  let hasReset = proto_reset proto
  let rawClock = bitwire rawClockWire
  let rawReset = Just $ bitwire rawResetWire
  when (not hasClock && flavor /= Strawberry) $ problem $
    show flavor ++ " flavor needs a clock of some sort"
  when (not hasReset && flavor /= Strawberry) $ problem $
    show flavor ++ " flavor needs a reset of some sort"
  when hasClock $ void $ do
    addWire InputWire rawClockWire bit clkreset
    addWire InternalWire sleepClockWire bit clkreset
  when hasReset $ void $ do
    addWire InputWire rawResetWire bit clkreset
    addWire InternalWire sleepResetWire bit clkreset
  -- since we're only clock-gating there's no need to do fancy reset stuff yet
  when hasReset $ void $ equateWire sleepResetWire $ bitwire rawResetWire
  -- do power protocol stuff
  addWire InternalWire needsClockWire bit control   -- really only needed for power-managed, but easier this way
  case proto_power proto of
    AlwaysOn -> do
      when hasClock $ void $ equateWire sleepClockWire $ bitwire rawClockWire
    ClockGating -> void $ do
      when (not hasClock) $ problem $
        show (proto_power proto) ++ " power protocol needs a clock"
      addWire InputWire clockReqWire bit handshake
      --addWire OutputWire powerAckWire bit handshake
      --addFlop powerAckWire
      --  (bitwire powerReqWire) rawClock Nothing rawReset False
      -- equateWire resetWire $ mkOr
      --  (bitwire resetInWire)
      --  (mkNot $ bitwire powerAckWire)
      addEquateWire InternalWire clockGateWire control $ (bitwire clockReqWire `mkOr` bitwire needsClockWire) `mkOr` bitwire rawResetWire
      instantiate clockGateModuleName clkreset [rawClockWire, clockGateWire, sleepClockWire]
      promiseDefined sleepClockWire

makeStrawberry :: HW ()
makeStrawberry = do
  prog <- getProg
  env <- getEnv
  -- Optional request wire, does nothing
  proto <- getProtocol
  let dp = proto_data proto
  let (reqs, _) = getDataShakes reqWires ackWires dp
  forM_ reqs $ \(w,t) -> addWire InputWire w t handshake
  -- Other logic
  let activities = M.elems $ bitp_activities prog
  when (length activities /= 1) $ nonono $
    "program cannot be made into a combinational circuit " ++
    "as it has multiple activities"
  let [activity] = activities
  forM_ (bitp_inputs prog) $ \var -> do
    let (Just typ) = findVarType env var
    typ' <- wirifyType typ
    addWire InputWire (inputWire var) typ' datapath
    addEquateWire InternalWire (varWire var) datapath $ EWire (inputWire var) `ofType` typ'
  forM_ (bitp_outputs prog) $ \var -> do
    let (Just exp) = M.lookup var $ actv_updates activity
    varWireExp <- wirify exp
    addEquateWire OutputWire (outputWire var) datapath varWireExp
  forM_ (M.toList $ bitp_shared prog) $
    \(ref, exp) -> do
      exp' <- wirify exp
      addEquateWire InternalWire (sharedWire ref) datapath exp'
  equateWire needsClockWire (zero bit)
  return ()

makeNonStrawberry :: HW ()
makeNonStrawberry = do
  prog <- getProg
  env <- getEnv
  flavor <- getFlavor
  -- make the handshake stuff and FSM
  makeControl
  -- create input var wires
  forM_ (bitp_inputs prog) $ \var -> do
    let (Just typ) = findVarType env var
    typ' <- wirifyType typ
    addWire InputWire (inputWire var) typ' datapath
  -- create ouput var wires and connect them to corresponding variable wires
  forM_ (bitp_outputs prog) $ \var -> do
    let (Just typ) = findVarType env var
    typ' <- wirifyType typ
    case flavor of
        Mint -> do
            let exp = mkIfThenElse (bitwire mintFinishingWire `mkAnd` bitwire (varEnableWire var)) (EWire (varNextWire var) `ofType` typ') (EVar var `ofType` typ)
            exp' <- wirify exp
            addEquateWire OutputWire (outputWire var) datapath exp'
        _ -> do
            exp' <- wirify $ EVar var `ofType` typ
            addEquateWire OutputWire (outputWire var) datapath exp'
  -- create the wires, additional modules, and logic for all of the actions
  mapM_ makeAction $ env_actions env
  -- create the wires, flops, and logic for the variables
  mapM_ makeVar $ M.keys $ env_vars env
  -- create and define the shared expression wires
  forM_ (M.toList $ bitp_shared prog) $
    \(ref, exp) -> do
      exp' <- wirify exp
      addEquateWire InternalWire (sharedWire ref) datapath exp'

makeControl :: HW ()
makeControl = do
  flavor <- getFlavor
  proto <- getProtocol
  addWire InternalWire doneWire bit control
  addWire InternalWire mayContinueWire bit control
  let dp = proto_data proto
  let (reqs, acks) = getDataShakes reqWires ackWires dp
  forM_ reqs $ \(w,t) -> addWire InputWire w t handshake
  forM_ acks $ \(w,t) -> addWire OutputWire w t handshake
  makeAdvance
  addEquateWire InternalWire holdWire control $ mkNot $ bitwire advanceWire
  addWire InternalWire runningWire bit control
  addWire InternalWire updateWire bit control
  case flavor of
    Vanilla -> makeControlVanilla
    Chocolate -> makeControlChocolate
    Mint -> makeControlMint
  let modname = case dp of
        OtherDataProtocol name _ _ -> rxModuleName name
        _ -> error $ "Cannot make a module that speaks: " ++ show dp
  instantiate modname helper $ [rawClockWire, rawResetWire, doneWire] ++ (map fst reqs) ++ (map fst acks) ++ [startWire, mayContinueWire]
  mapM_ promiseDefined $ (map fst acks) ++ [startWire, mayContinueWire]

makeControlVanilla :: HW ()
makeControlVanilla = do
  doneSt <- isDoneState
  equateWire doneWire doneSt
  equateWire updateWire $ mkOr (bitwire startWire) (bitwire advanceWire)
  addWire InternalWire startWire bit control
  makeFSMVanilla
  -- When do we need clock?
  idle <- isIdle
  equateWire needsClockWire $ bitwire startWire `mkOr` (mkNot idle)
  -- When are we running?
  equateWire runningWire $ mkNot $ doneSt `mkOr` idle
  return ()

makeControlChocolate :: HW ()
makeControlChocolate = do
  -- Common
  doneSt <- isDoneState
  equateWire doneWire doneSt
  equateWire updateWire $ bitwire advanceWire
  addEquateWire InternalWire notDoneWire control $ mkNot $ bitwire doneWire
  addWire InternalWire notInitialWire bit control
  let notInitial' =
        (bitwire notInitialWire `mkAnd` bitwire notDoneWire)  -- down after done
        `mkOr` bitwire updateWire                             -- up after update
  addFlop notInitialWire notInitial' sleepClock Nothing sleepReset False
  addEquateWire InternalWire initialWire control $ mkNot $ bitwire notInitialWire
  -- Determining whether the machine is running.
  addWire InternalWire startWire bit control
  addWire InternalWire continueRunningWire bit control
  addFlop continueRunningWire (bitwire runningWire) sleepClock Nothing sleepReset False
  equateWire runningWire $ (bitwire continueRunningWire `mkOr` bitwire startWire) `mkAnd` (bitwire notDoneWire)
  -- Finite state machine
  makeFSMChocolate
  -- When do we need clock?
  equateWire needsClockWire $ bitwire runningWire `mkOr` bitwire doneWire
  return ()

makeControlMint :: HW ()
makeControlMint = do
  -- Common
  equateWire updateWire $ bitwire advanceWire
  addWire InternalWire notInitialWire bit control
  let notInitialNext = (mkNot $ bitwire mintFinishingWire) `mkAnd` (bitwire updateWire `mkOr` bitwire notInitialWire)
  addFlop notInitialWire notInitialNext sleepClock Nothing sleepReset False
  addEquateWire InternalWire initialWire control $ mkNot $ bitwire notInitialWire
  -- Determining whether the machine is running.
  addWire InternalWire startWire bit control
  addWire InternalWire continueRunningWire bit control
  addFlop continueRunningWire (bitwire runningWire `mkAnd` (mkNot $ bitwire mintFinishingWire)) sleepClock Nothing sleepReset False
  inDoneState <- isDoneState
  equateWire runningWire $ (bitwire continueRunningWire `mkOr` bitwire startWire)
  -- Finite state machine
  makeFSMMint
  -- When do we need clock?
  equateWire needsClockWire $ bitwire runningWire `mkOr` inDoneState
  return ()

-- Create the state wire and main FSM.
makeFSMVanilla :: HW ()
makeFSMVanilla = do
  prog <- getProg
  env <- getEnv
  proto <- getProtocol
  typ <- stateType
  stw <- getStateWire
  let activityStates = map activityState $ M.keys $ bitp_activities prog
  let allStates = idleState:doneState:activityStates
  createEnumType (fsmStateTypeName env) allStates EnumMutex
  makeInStateWires allStates
  addWire InternalWire stw typ statemachine
  let idleNext = EEnum (activityState $ bitp_entry prog) `ofType` typ
  let doneNext = EEnum idleState `ofType` typ
  doneSt <- isDoneState
  let enable = bitwire updateWire `mkOr` (doneSt `mkAnd` bitwire mayContinueWire)
  next <- liftM simplify $ muxAcrossStatesVanilla typ idleNext doneNext $
    \(_, activity) -> wirify $ actv_next activity
  -- emulate a reset to idle state (note: synchronous)
  let (next', enable') = case sleepReset of
        Just e -> (mkIfThenElse e (EEnum idleState `ofType` typ) next, mkOr enable e)
        Nothing -> (next, enable)
  addFlop stw next' sleepClock (Just enable') Nothing False

-- Create the state wire and main FSM.
makeFSMChocolate :: HW ()
makeFSMChocolate = do
  prog <- getProg
  env <- getEnv
  proto <- getProtocol
  typ <- stateType
  stw <- getStateWire
  let activityStates = map activityState $ M.keys $ bitp_activities prog
  let allStates = activityStates ++ [doneState]
  let entry = activityState $ bitp_entry prog
  let allStates' = entry : delete entry allStates  -- entry needs to be 0
  createEnumType (fsmStateTypeName env) allStates' EnumMutex
  makeInStateWires allStates
  addWire InternalWire stw typ statemachine
  doneSt <- isDoneState
  let enable = bitwire updateWire `mkOr` (doneSt `mkAnd` bitwire mayContinueWire)
  let entryExp = EEnum entry `ofType` typ
  next <- liftM simplify $ muxAcrossStatesChocolate typ entryExp $
    \(_, activity) -> wirify $ actv_next activity
  -- emulate a reset to entry state (note: synchronous)
  let (next', enable') = case sleepReset of
        Just e -> (mkIfThenElse e entryExp next, mkOr enable e)
        Nothing -> (next, enable)
  addFlop stw next' sleepClock (Just enable') Nothing False

-- Create the state wire and main FSM, and equate the done wire.
makeFSMMint :: HW ()
makeFSMMint = do
  prog <- getProg
  env <- getEnv
  proto <- getProtocol
  typ <- stateType
  stw <- getStateWire
  let activityStates = map activityState $ M.keys $ bitp_activities prog
  let allStates = activityStates ++ [doneState]
  let entry = activityState $ bitp_entry prog
  let allStates' = entry : delete entry allStates  -- entry needs to be 0
  createEnumType (fsmStateTypeName env) allStates' EnumMutex
  makeInStateWires allStates
  addWire InternalWire stw typ statemachine
  doneSt <- isDoneState
  let enable = bitwire updateWire `mkOr` (doneSt `mkAnd` bitwire mayContinueWire)
  let entryExp = EEnum entry `ofType` typ
  next <- liftM simplify $ muxAcrossStatesChocolate typ entryExp $
    \(_, activity) -> wirify $ actv_next activity
  -- emulate a reset to entry state (note: synchronous)
  let (next', enable') = case sleepReset of
        Just e -> (mkIfThenElse e entryExp next, mkOr enable e)
        Nothing -> (next, enable)
  addEquateWire InternalWire mintTmpStateWire control next'
  addEquateWire InternalWire mintTmpStateIsDoneWire control $ EOpApp Eq [EWire mintTmpStateWire `ofType` typ, EEnum doneState `ofType` typ] `ofType` bit
  addEquateWire InternalWire mintFinishingWire control $ enable' `mkAnd` bitwire mintTmpStateIsDoneWire
  doneSt <- isDoneState
  equateWire doneWire $ doneSt `mkOr` bitwire mintFinishingWire
  let next'' = mkIfThenElse (bitwire mintTmpStateIsDoneWire `mkAnd` bitwire mayContinueWire) entryExp next'
  addFlop stw next'' sleepClock (Just $ enable') Nothing False

-- Create the advance wire and its logic.
makeAdvance :: HW ()
makeAdvance = do
  env <- getEnv
  handled <- filterM needsAdapter $ env_actions env
  let readies = map
        (\action -> optImpl bit
          (bitwire $ actionWantWire action)
          (bitwire $ actionDoneWire action))
        handled
  let ready = optAnd bit readies
  let running = bitwire runningWire
  void $ addEquateWire InternalWire advanceWire control $ optAnd bit [running, ready]

-- Create the circuitry for a variable and its updates.
makeVar :: Var -> HW ()
makeVar var = do
  flavor <- getFlavor
  case flavor of
    Vanilla -> makeVarVanilla var
    Chocolate -> makeVarChocolate var
    Mint -> makeVarChocolate var

makeVarVanilla :: Var -> HW ()
makeVarVanilla var = do
  prog <- getProg
  env <- getEnv
  styp <- stateType
  let (Just typ) = findVarType env var
  typ' <- wirifyType typ
  -- Create wires and flop
  let cur = varWire var
  let next = varNextWire var
  let enable = varEnableWire var
  addWire InternalWire cur typ' datapath
  addWire InternalWire next typ' datapath
  addWire InternalWire enable bit datapath
  addFlop cur (EWire next `ofType` typ') sleepClock (Just $ bitwire enable) Nothing False
  -- Define wires
  maybeInitialValue <- getInitialValue var
  let initialValue = maybe (dontCare typ') id maybeInitialValue
  keep <- wirify $ EVar var `ofType` typ
  next <- muxAcrossStatesVanilla typ' initialValue keep
    (\(_, activity) -> maybe
      (return keep)
      wirify $
      M.lookup var $ actv_updates activity)
  let next' = simplify $ bake $ simplify next
  equateWire (varNextWire var) next'
  equateWire (varEnableWire var) $ bitwire updateWire
  return ()

-- Makes the wiring for a variable in the Chocolate architecture style. Unlike
-- with the Vanilla architecture we want to be able to begin immediately, which
-- means not spending a cycle initializing input variables. To accomplish this
-- a mux is added after each input variable's flop, allowing it to be bypassed
-- in favor of the raw input. Care is also taken to initialize the variable if
-- it is not set when leaving the entry activityful.
makeVarChocolate :: Var -> HW ()
makeVarChocolate var = do
  prog <- getProg
  env <- getEnv
  styp <- stateType
  let (Just typ) = findVarType env var
  typ' <- wirifyType typ

  -- Determine next value cases and enable disjunction, ignoring initialization.
  let activityNexts = catMaybes $ flip map (M.toList $ bitp_activities prog) $
        \(label, activity) -> do
          next <- M.lookup var $ actv_updates activity
          return (label, next)
  nexts <- forM activityNexts $ \(label, exp) -> do
    cond <- isInActivity label
    exp' <- wirify exp
    return (cond, exp')
  enables <- forM activityNexts $ isInActivity . fst

  -- Adjust nexts and enables to do initialization if needed.
  maybeInitialValue <- getInitialValue var
  let setByEntryActivity = M.member var $ actv_updates $ fromJust $ M.lookup
        (bitp_entry prog) (bitp_activities prog)
  let needsInit = isJust maybeInitialValue && not setByEntryActivity
  let nexts' = if needsInit
        then (bitwire initialWire, fromJust maybeInitialValue):nexts
        else nexts
  let enables' = if needsInit
        then (bitwire initialWire) : enables
        else enables

  -- Turn into single expressions.
  let nextExp = simplify $ bake $ simplify $ mutexConds typ' nexts'
  let enableExp = simplify $ optOr bit enables' `mkAnd` bitwire updateWire

  -- Build the circuit.
  let cur = varWire var
  let next = varNextWire var
  let enable = varEnableWire var
  addWire InternalWire cur typ' datapath
  addWire InternalWire next typ' datapath
  addWire InternalWire enable bit datapath

  equateWire next nextExp
  equateWire enable enableExp

  let needsBypass = isJust maybeInitialValue
  if needsBypass
    then do
      let tmpWire = varTmpWire var
      addWire InternalWire tmpWire typ' datapath
      addFlop tmpWire (EWire next `ofType` typ') sleepClock (Just $ bitwire enable) Nothing False
      equateWire cur $ mkIfThenElse
        (bitwire initialWire)
        (fromJust maybeInitialValue)
        (EWire tmpWire `ofType` typ')
      return ()
    else do
      addFlop cur (EWire next `ofType` typ') sleepClock (Just $ bitwire enable) Nothing False

-- The initial (note: already wirified) value for a variable, if one exists.
getInitialValue :: Var -> HW (Maybe Exp)
getInitialValue var = do
  prog <- getProg
  env <- getEnv
  styp <- stateType
  flavor <- getFlavor
  let (Just typ) = findVarType env var
  typ' <- wirifyType typ
  return $ if var == bitp_ret prog
    then Just $ EEnum doneState `ofType` styp
        --case flavor of
        --Vanilla -> Just $ EEnum doneState `ofType` styp
        --Chocolate -> Just $ EEnum doneState `ofType` styp
        --Mint -> Just $ EEnum (activityState $ bitp_entry prog) `ofType` styp
    else if var `elem` bitp_inputs prog
      then Just $ EWire (inputWire var) `ofType` typ'
      else Nothing

-- Create the circuitry for an action: its implementation, its adapter (if
-- needed), and its invocation.
makeAction :: ActionName -> HW ()
makeAction action = do
  prog <- getProg
  env <- getEnv
  let (Just info) = findActionInfo env action
  let atyp = ai_type info
  let protocol = ai_protocol info
  let pdata = proto_data protocol
  let power = proto_power protocol
  let provider = ai_provider info
  let fp = at_io atyp
  let inputParams  = flattenPat =<< fp_arguments fp
  let outputParams = flattenPat $ fp_result fp
  -- possible wires we may create/use (note: types here not wirified)
  let inputs   = map (\(inp, typ) -> (actionInputWire action inp, typ)) inputParams
  let outputs  = map (\(out, typ) -> (actionOutputWire action out, typ)) outputParams
  let hinputs  = map (\(inp, typ) -> (adapterInputWire action inp, typ)) inputParams
  let houtputs = map (\(out, typ) -> (adapterOutputWire action out, typ)) outputParams
  let want = actionWantWire action
  let done = actionDoneWire action
  let clockReq = actionClockReqWire action
  --let pwrReq = actionPowerReqWire action
  --let pwrAck = actionPowerAckWire action
  let run = actionRunWire action
  -- think a bit
  let (toAction, fromAction) = case provider of
        External -> (OutputWire, InputWire)
        Instantiate _ _ -> (InternalWire, InternalWire)
  let clocks = if proto_clock protocol then [sleepClockWire] else []
  let resets = if proto_reset protocol then [sleepResetWire] else []
  let (powerReqs, powerAcks) = case power of
        AlwaysOn -> ([],[])
        ClockGating -> ([clockReq],[])
        --PowerShake -> ([pwrReq],[pwrAck])
  let (shakeReqs, shakeAcks) = getDataShakes (actionReqWires action) (actionAckWires action) pdata
  let maybeMakeAdapter = case pdata of
        Immediate _ -> if power == AlwaysOn
                       then Nothing
                       else Just makeImmediateAdapter
        _ -> Just $ makeLatchingAdapter pdata
  -- think slightly about the arbiter
  sharees <- getSharees action
  let ntaps = 1 + (fromIntegral $ length sharees)
  let taps = [0 .. ntaps - 1]
  -- create the wires to and from the action implementation and arbiter
  forM_ powerReqs $ \w -> do
    addWire toAction w bit handshake
    forM taps $ \i -> addWire InternalWire (arbify i w) bit handshake
  forM_ powerAcks $ \w -> do
    addWire fromAction w bit handshake
    forM taps $ \i -> addWire InternalWire (arbify i w) bit handshake
  forM_ shakeReqs $ \(w,t) -> do
    addWire toAction w t handshake
    forM taps $ \i -> addWire InternalWire (arbify i w) t handshake
  forM_ shakeAcks $ \(w,t) -> do
    addWire fromAction w t handshake
    forM taps $ \i -> addWire InternalWire (arbify i w) t handshake
  forM_ inputs    $ \(w,t) -> do
    t' <- wirifyType t
    addWire toAction w t' datapath
    forM taps $ \i -> addWire InternalWire (arbify i w) t' datapath
  forM_ outputs   $ \(w,t) -> do
    t' <- wirifyType t
    addWire fromAction w t' datapath
    forM taps $ \i -> addWire InternalWire (arbify i w) t' datapath
  -- the wires used to talk with the action, other than clk/reset
  let bundle = powerReqs ++ powerAcks ++ map fst shakeReqs ++ map fst shakeAcks ++ map fst (inputs ++ outputs)
  let bundleToAction = powerReqs ++ map fst shakeReqs ++ map fst inputs
  let bundleFromAction = powerAcks ++ map fst shakeAcks ++ map fst outputs
  -- create arbiter and its taps
  let arbBundles = taps >>= \i -> map (arbify i) bundle
  arbiterName <- diversion $ makeArbiter ntaps action
  instantiate arbiterName helper $ [sleepClockWire, rawResetWire] ++ bundle ++ arbBundles
  let arbWiresDefd = bundleToAction ++ (taps >>= \i -> map (arbify i) $ bundleFromAction)
  mapM_ promiseDefined arbWiresDefd
  -- create the action implementation if needed
  case provider of
    Instantiate implName sharedActions -> do
      sharedWires <- liftM concat $ forM sharedActions $ \action' -> do
        tap <- getArbTap action' action
        bundle' <- liftM (map $ \(w,d) -> (arbify tap w,d)) $ actionBundle action'
        mapM_ promiseDefined $ map fst $ filter snd bundle'
        return $ map fst bundle'
      instantiate implName child $ clocks ++ resets ++ bundle ++ sharedWires
      mapM_ promiseDefined $ map fst shakeAcks ++ powerAcks ++ map fst outputs
    _ -> return ()
  -- create power logic
  makeActionPower action power
  -- create the adapter and associated wires
  case maybeMakeAdapter of
    Nothing -> case pdata of
      Immediate True -> do
        let [(w,_)] = shakeReqs
        wantExp <- computeActionWant action
        void $ equateWire (arbify 0 w) wantExp
      _ -> return ()
    Just makeAdapter -> do
      addWire InternalWire want bit control
      addWire InternalWire done bit control
      forM_ hinputs  $ \(w,t) -> do
        t' <- wirifyType t
        addWire InternalWire w t' datapath
      forM_ houtputs $ \(w,t) -> do
        t' <- wirifyType t
        addWire InternalWire w t' datapath
      wantExp <- computeActionWant action
      void $ equateWire want wantExp
      addEquateWire InternalWire run control $
        case power of
          AlwaysOn -> bitwire want
          ClockGating -> bitwire want
          -- PowerShake -> bitwire want `mkAnd` bitwire pwrAck
      adapterName <- diversion $ makeAdapter action inputParams outputParams
      instantiate adapterName helper $
        [sleepClockWire, sleepResetWire, run, done, holdWire] ++
        map fst (hinputs ++ houtputs) ++
        (map our $ map fst shakeReqs ++ map fst shakeAcks ++ map fst (inputs ++ outputs))
      mapM_ promiseDefined $
        [done] ++ (map our $ map fst shakeReqs ++ (map fst inputs)) ++ (map fst houtputs)
  -- hook up the action's inputs
  forM_ inputParams $ \(name,_) -> do
    wire <- actionEffectiveInputWire action name
    exp <- computeActionInput action name
    equateWire wire exp

makeArbiter :: Integer -> ActionName -> HW ModuleName
makeArbiter ntaps action = do
  env <- getEnv
  let (Just info) = findActionInfo env action
  let atyp = ai_type info
  let protocol = ai_protocol info
  let pdata = proto_data protocol
  let power = proto_power protocol
  let provider = ai_provider info
  let fp = at_io atyp
  let inputParams  = flattenPat =<< fp_arguments fp
  let outputParams = flattenPat $ fp_result fp
  -- possible wires we may create/use (note: types here not wirified)
  let clk = arbiterInsideClockWire
  let reset = arbiterInsideResetWire
  let clockReq = arbiterInsideClockReqWire
  --let pwrReq = arbiterInsidePowerReqWire
  --let pwrAck = arbiterInsidePowerAckWire
  let inputs   = map (\(inp, typ) -> (arbiterInsideInputWire action inp, typ)) inputParams
  let outputs  = map (\(out, typ) -> (arbiterInsideOutputWire action out, typ)) outputParams
  -- think a bit
  let (powerReqs, powerAcks) = case power of
        AlwaysOn -> ([],[])
        ClockGating -> ([clockReq],[])
        --PowerShake -> ([pwrReq],[pwrAck])
  let (shakeReqs, shakeAcks) = getDataShakes arbiterInsideReqWires arbiterInsideAckWires pdata
  let taps = [0 .. ntaps - 1]
  -- go!
  newModule $ arbiterModuleName env action
  -- i/o wires: clk/reset
  addWire InputWire clk bit clkreset
  addWire InputWire reset bit clkreset
  -- i/o wires: with action 
  forM_ powerReqs $ \w -> addWire OutputWire w bit handshake
  forM_ powerAcks $ \w -> addWire InputWire w bit handshake
  forM_ shakeReqs $ \(w,t) -> addWire OutputWire w t handshake
  forM_ shakeAcks $ \(w,t) -> addWire InputWire w t handshake
  forM_ inputs    $ \(w,t) -> do
    t' <- wirifyType t
    addWire OutputWire w t' datapath
  forM_ outputs   $ \(w,t) -> do
    t' <- wirifyType t
    addWire InputWire w t' datapath
  -- i/o wires: with taps
  forM_ taps $ \i -> do
    forM_ powerReqs $ \w -> addWire InputWire (arbify i w) bit handshake
    forM_ powerAcks $ \w -> addWire OutputWire (arbify i w) bit handshake
    forM_ shakeReqs $ \(w,t) -> addWire InputWire (arbify i w) t handshake
    forM_ shakeAcks $ \(w,t) -> addWire OutputWire (arbify i w) t handshake
    forM_ inputs    $ \(w,t) -> do
      t' <- wirifyType t
      addWire InputWire (arbify i w) t' datapath
    forM_ outputs   $ \(w,t) -> do
      t' <- wirifyType t
      addWire OutputWire (arbify i w) t' datapath
  if ntaps == 1
    then do
      -- pass-through
      forM_ powerReqs $ \w -> equateWire w $ bitwire $ arbify 0 w
      forM_ powerAcks $ \w -> equateWire (arbify 0 w) $ bitwire w
      forM_ shakeReqs $ \(w,t) -> equateWire w $ typewire t $ arbify 0 w
      forM_ shakeAcks $ \(w,t) -> equateWire (arbify 0 w) $ typewire t w
      forM_ inputs    $ \(w,t) -> do
        t' <- wirifyType t
        equateWire w $ EWire (arbify 0 w) `ofType` t'
      forM_ outputs   $ \(w,t) -> do
        t' <- wirifyType t
        equateWire (arbify 0 w) $ EWire w `ofType` t'
    else do
      case power of
        AlwaysOn -> return ()
        ClockGating -> do
          forM_ powerReqs $ \w -> equateWire w $ optOr bit (map (\i -> bitwire $ arbify i w) taps)
      case pdata of
        Immediate _ -> nonono $ "cannot arbitrate immediate-protocol action " ++ show action
        -- FourPhase -> do
        --   let req = head arbiterInsideReqWires
        --   let ack = head arbiterInsideAckWires
        --   when (power /= AlwaysOn) $ problem $ "arbiter can't power manage for action " ++ show action
        --   -- slow round-robin first draft
        --   let shift = arbiterInsideShiftWire
        --   let selected i = arbify i arbiterInsideSelectedWire
        --   -- create wires
        --   addWire InternalWire shift bit control
        --   forM_ taps $ \i -> do
        --     addWire InternalWire (selected i) bit control
        --   -- set up the circular shift register
        --   equateWire shift $ mkNot $ bitwire req `mkOr` bitwire ack
        --   forM_ taps $ \i -> do
        --     let initialSelected = if i == 0 then ones bit else zero bit
        --     let i' = (i - 1) `mod` ntaps
        --     let exp = mkIfThenElse (bitwire reset) initialSelected $
        --           mkIfThenElse (bitwire shift) (bitwire $ selected i') (bitwire $ selected i)
        --     addFlop (selected i) exp (bitwire clk) Nothing Nothing False
        --   -- muxing of signals to action
        --   equateWire req $ mutexConds bit $ map (\i -> (bitwire $ selected i, bitwire $ arbify i req)) taps
        --   forM_ inputs $ \(w,t) -> do
        --     t' <- wirifyType t
        --     equateWire w $ mutexConds t' $ map (\i -> (bitwire $ selected i, EWire (arbify i w) `ofType` t')) taps
        --   -- demuxing of signals from action
        --   forM_ taps $ \i -> do
        --     equateWire (arbify i ack) (bitwire ack `mkAnd` bitwire (selected i))
        --     forM_ outputs $ \(w,t) -> do
        --       t' <- wirifyType t
        --       equateWire (arbify i w) $ mkIfThenElse
        --         (bitwire $ selected i)
        --         (EWire w `ofType` t')
        --         (zero t') -- dontcare? TODO check why dontcare is only allowed temporarily
        _ -> problem $ "don't yet know how to arbitrate protocol " ++ show pdata ++ " for action " ++ show action
  -- yay
  moduleDone

-- What actions are the given action shared with?
getSharees :: ActionName -> HW [ActionName]
getSharees shared = do
  env <- getEnv
  let sharees = flip filter (env_actions env) $ \action ->
        let (Just ai) = findActionInfo env action
        in case ai_provider ai of
          External -> False
          Instantiate _ shareds -> shared `elem` shareds
  return sharees

-- Which tap from the arbiter for action `shared` should be routed to action
-- `sharee`?
getArbTap :: ActionName -> ActionName -> HW Integer
getArbTap shared sharee = do
  sharees <- getSharees shared
  case findIndex (== sharee) sharees of
    Just i -> return $ 1 + (fromIntegral i)
    Nothing -> error $ "getArbTap can't find " ++ sharee ++ " among those shared with " ++ shared

-- The bundle of wires used to communicate with an action, each paired with a
-- direction indicator (True = to action, False = from action).
actionBundle :: ActionName -> HW [(Wire, Bool)]
actionBundle action = do
  env <- getEnv
  let (Just info) = findActionInfo env action
  let atyp = ai_type info
  let protocol = ai_protocol info
  let pdata = proto_data protocol
  let power = proto_power protocol
  let provider = ai_provider info
  let fp = at_io atyp
  let inputParams  = flattenPat =<< fp_arguments fp
  let outputParams = flattenPat $ fp_result fp
  let clockReq = actionClockReqWire action
  --let pwrReq = actionPowerReqWire action
  --let pwrAck = actionPowerAckWire action
  let inputs   = map (\(inp, _) -> actionInputWire action inp) inputParams
  let outputs  = map (\(out, _) -> actionOutputWire action out) outputParams
  let (powerReqs, powerAcks) = case power of
        AlwaysOn -> ([],[])
        ClockGating -> ([clockReq],[])
        --PowerShake -> ([pwrReq],[pwrAck])
  let (shakeReqs', shakeAcks') = getDataShakes (actionReqWires action) (actionAckWires action) pdata
  let (shakeReqs, shakeAcks) = (map fst shakeReqs', map fst shakeAcks')
  return $
    i_ powerReqs ++
    o_ powerAcks ++
    i_ shakeReqs ++
    o_ shakeAcks ++
    i_ inputs ++
    o_ outputs
  where
    i_ = map $ \w -> (w, True)
    o_ = map $ \w -> (w, False)

makeActionPower :: ActionName -> PowerProtocol -> HW ()
makeActionPower action power =
  case power of
    AlwaysOn -> return ()
    ClockGating ->
      let pwr = our $ actionClockReqWire action
      in void $ equateWire pwr (bitwire $ actionWantWire action)

-- Determine when an action should be run (its "want" signal, if it has a
-- adapter).
computeActionWant :: ActionName -> HW Exp
computeActionWant action = do
  prog <- getProg
  flavor <- getFlavor
  disjuncts <- liftM catMaybes $ mapM doActivity $ M.toList $ bitp_activities prog
  let wantVanilla = simplify $ optOr bit disjuncts
  let wantChocolate = mkAnd (bitwire runningWire) wantVanilla
  let want = case flavor of
        Vanilla -> wantVanilla
        Chocolate -> wantChocolate
        Mint -> wantChocolate
  return want
  where
    doActivity (label, activity) =
      case M.lookup action $ actv_actions activity of
        Nothing -> return Nothing
        Just tree -> do
          inActivity <- isInActivity label
          want <- treeWant tree
          return $ Just $ optAnd bit [inActivity, want]
    treeWant tree =
      case tree of
        ADepends exp tree1 tree2 -> do
          want1 <- treeWant tree1
          want2 <- treeWant tree2
          exp' <- wirify exp
          return $ if want1 == want2
            then want1
            else (exp' `mkAnd` want1) `mkOr` ((mkNot exp') `mkAnd` want2)
        ACall _ -> return $ ones bit
        ADontCall -> return $ zero bit

-- Determine the value for a given action input wire.
computeActionInput :: ActionName -> VarName -> HW Exp
computeActionInput action inp = do
  prog <- getProg
  env <- getEnv
  let (Just info) = findActionInfo env action
  let atyp = ai_type info
  let fp = at_io atyp
  let argPatterns = fp_arguments fp
  let argPattern = PTuple argPatterns  -- pretend there's one big argument tuple
  let [(path, typ)] = paramPaths inp argPattern  -- where our input var occurs
  typ' <- wirifyType typ
  let extract = flip (foldl $ flip mkProj) path
  muxed <- muxAcrossActivitys typ' (doActivity typ' extract)
  return $ simplify $ bake muxed
  where
    -- Return a (wirified!) expression for the action input when in a particular
    -- activity.
    doActivity typ' extract (label, activity) =
      case M.lookup action $ actv_actions activity of
        Nothing -> return $ dontCare typ'
        Just tree -> doTree typ' extract tree
    -- Given a call tree, compute an *un-wirified* expression for the action
    -- input. `extract` is used to pick out the input's value from the action's
    -- arguments, and `typ` is the type of the input.
    doTree typ' extract tree = do
      res <- case tree of
        ADepends exp tree1 tree2 -> do
          exp' <- wirify exp
          exp1 <- doTree typ' extract tree1
          exp2 <- doTree typ' extract tree2
          return $ optIfThenElse typ' (simplify exp') exp1 exp2
        ACall args -> do
          let arg = mkTuple args
          wirify $ simplify $ extract arg
        ADontCall -> return $ dontCare typ'
      return $ simplify res

makeImmediateAdapter :: ActionName -> [Param] -> [Param] -> HW ModuleName
makeImmediateAdapter action inputParams outputParams = do
  env <- getEnv
  newModule $ adapterModuleName env action
  -- create wires
  run <- addWire InputWire adapterInsideRunWire bit control
  done <- addWire OutputWire adapterInsideDoneWire bit control
  hold <- addWire InputWire adapterInsideHoldWire bit control
  minputs <- forM inputParams $ \(name, typ) -> do
    typ' <- wirifyType typ
    addWire InputWire (adapterInsideManagedInputWire name) typ' datapath
  moutputs <- forM outputParams $ \(name, typ) -> do
    typ' <- wirifyType typ
    addWire OutputWire (adapterInsideManagedOutputWire name) typ' datapath
  inputs <- forM inputParams $ \(name, typ) -> do
    typ' <- wirifyType typ
    addWire OutputWire (adapterInsideInputWire name) typ' datapath
  outputs <- forM outputParams $ \(name, typ) -> do
    typ' <- wirifyType typ
    addWire InputWire (adapterInsideOutputWire name) typ' datapath
  -- done = run
  equateWire done $ bitwire run
  -- pass inputs through
  forM_ inputParams $ \(name, typ) -> do
    typ' <- wirifyType typ
    let incoming = adapterInsideManagedInputWire name
    let outgoing = adapterInsideInputWire name
    equateWire outgoing $ EWire incoming `ofType` typ'
  -- pass outputs through
  forM_ outputParams $ \(name, typ) -> do
    typ' <- wirifyType typ
    let incoming = adapterInsideOutputWire name
    let outgoing = adapterInsideManagedOutputWire name
    equateWire outgoing $ EWire incoming `ofType` typ'
  moduleDone

makeLatchingAdapter :: DataProtocol -> ActionName -> [Param] -> [Param] -> HW ModuleName
makeLatchingAdapter pdata action inputParams outputParams = do
  prog <- getProg
  env <- getEnv
  newModule $ adapterModuleName env action
  -- figure out control module
  let controlModuleName = case pdata of
        OtherDataProtocol name _ _ -> txModuleName name
        _ -> error $ "Can't make latching adapter for protocol " ++ show pdata
  -- create wires
  clk <- addWire InputWire adapterInsideClockWire bit clkreset
  reset <- addWire InputWire adapterInsideResetWire bit clkreset
  run <- addWire InputWire adapterInsideRunWire bit control
  done <- addWire OutputWire adapterInsideDoneWire bit control
  hold <- addWire InputWire adapterInsideHoldWire bit control
  minputs <- forM inputParams $ \(name, typ) -> do
    typ' <- wirifyType typ
    addWire InputWire (adapterInsideManagedInputWire name) typ' datapath
  moutputs <- forM outputParams $ \(name, typ) -> do
    typ' <- wirifyType typ
    addWire OutputWire (adapterInsideManagedOutputWire name) typ' datapath
  let (reqs, acks) = getDataShakes adapterInsideReqWires adapterInsideAckWires pdata
  forM_ reqs $ \(w,t) -> addWire OutputWire w t control
  forM_ acks $ \(w,t) -> addWire InputWire w t control
  inputs <- forM inputParams $ \(name, typ) -> do
    typ' <- wirifyType typ
    addWire OutputWire (adapterInsideInputWire name) typ' datapath
  outputs <- forM outputParams $ \(name, typ) -> do
    typ' <- wirifyType typ
    addWire InputWire (adapterInsideOutputWire name) typ' datapath
  latch <- addWire InternalWire adapterInsideLatchWire bit control
  -- pass inputs through
  forM_ inputParams $ \(name, typ) -> do
    typ' <- wirifyType typ
    let incoming = adapterInsideManagedInputWire name
    let outgoing = adapterInsideInputWire name
    equateWire outgoing $ EWire incoming `ofType` typ'
  -- latch outputs
  forM_ outputParams $ \(name, typ) -> do
    typ' <- wirifyType typ
    let incoming = adapterInsideOutputWire name
    let outgoing = adapterInsideManagedOutputWire name
    let enable = EWire latch `ofType` bit
    addFlop outgoing (EWire incoming `ofType` typ') (bitwire clk) (Just enable) Nothing True
  -- instantiate control circuit
  instantiate controlModuleName control $ [clk, reset, run, hold] ++ map fst acks ++ map fst reqs ++ [latch, done]
  promiseDefined latch
  promiseDefined done
  mapM_ promiseDefined $ map fst reqs
  -- yay
  moduleDone

-- Make wires being in each state.
makeInStateWires :: [EnumValue] -> HW ()
makeInStateWires _ = return () -- mapM_ (void . makeInState)

-- Produce an expression for being in a given state. Currently makes a wire for
-- it, if no such wire already exists.
--makeInState :: EnumValue -> HW Exp
--makeInState st = do
--  stw <- getStateWire
--  let wire = inStateWire st
--  exists <- wireExists wire
--  when (not exists) $ do
--    t <- stateType
--    void $ addEquateWire InternalWire wire control $
--      EOpApp Eq [EWire stw `ofType` t, EEnum st `ofType` t] `ofType` bit
--  return $ EWire wire `ofType` bit

----------------------------------------
-- Helpers
----------------------------------------

-- Given a supply of names for req and ack wires (infinite ideally), get the names and types of req and ack wires for a protocol.
getDataShakes :: [Wire] -> [Wire] -> DataProtocol -> ([(Wire, Type)], [(Wire, Type)])
getDataShakes reqNames ackNames dp =
  let (reqTypes, ackTypes) = getDataShakeTypes dp
  in (zip reqNames reqTypes, zip ackNames ackTypes)

-- Get the types of the req and ack wire(s) for a data protocol.
getDataShakeTypes :: DataProtocol -> ([Type], [Type])
getDataShakeTypes dp =
  case dp of
    Immediate b -> (if b then [bit] else [], [])
    OtherDataProtocol _ treq tack -> ([treq], [tack])

-- Note: assumes all of the involved expressions are already wirified.
muxAcrossStatesVanilla :: Type -> Exp -> Exp -> ((ActivityLabel, Activity) -> HW Exp) -> HW Exp
muxAcrossStatesVanilla typ ifIdle ifDone ifActivity = do
  prog <- getProg
  let labelsAndActivitys = M.toList $ bitp_activities prog
  activityExps <- mapM ifActivity labelsAndActivitys
  activityConds <- mapM (isInActivity . fst) labelsAndActivitys
  idleCond <- isIdle
  doneCond <- isDoneState
  let pairs = (idleCond, ifIdle) : (doneCond, ifDone) : zip activityConds activityExps
  return $ mutexConds typ pairs

-- Note: assumes all of the involved expressions are already wirified.
muxAcrossStatesChocolate :: Type -> Exp -> ((ActivityLabel, Activity) -> HW Exp) -> HW Exp
muxAcrossStatesChocolate typ ifDone ifActivity = do
  prog <- getProg
  let labelsAndActivitys = M.toList $ bitp_activities prog
  activityExps <- mapM ifActivity labelsAndActivitys
  activityConds <- mapM (isInActivity . fst) labelsAndActivitys
  doneCond <- isDoneState
  let pairs = (doneCond, ifDone) : zip activityConds activityExps
  return $ mutexConds typ pairs

-- Note: assumes all of the involved expressions are already wirified.
muxAcrossActivitys :: Type -> ((ActivityLabel, Activity) -> HW Exp) -> HW Exp
muxAcrossActivitys typ ifActivity = do
  prog <- getProg
  let labelsAndActivitys = M.toList $ bitp_activities prog
  activityExps <- mapM ifActivity labelsAndActivitys
  activityConds <- mapM (isInActivity . fst) labelsAndActivitys
  let pairs = zip activityConds activityExps
  return $ mutexConds typ pairs

-- Turn variables, activitylabels, shared expression references, and action results
-- into their hardware-equivalent wires.
wirify :: Exp -> HW Exp
wirify exp = do
  inner' <- case getInner exp of
    EVar v -> do
      return $ EWire $ varWire v
    ETypeAnn t exp1 -> do
      t' <- wirifyType t
      exp1' <- wirify exp1
      return $ ETypeAnn t' exp1'
    EActivityLabel label -> do
      let stname = activityState label
      return $ EEnum stname
    EShared ref -> do
      return $ EWire $ sharedWire ref
    EActionResult action res -> do
      wire <- actionEffectiveOutputWire action res
      return $ EWire wire
    _ -> do
      tmp <- untoChildren wirify exp
      return $ getInner tmp
  typ' <- wirifyType $ typeOf exp
  return $ inner' `ofType` typ'

-- Change the type Address to the actual address type we're using.
wirifyType :: Type -> HW Type
wirifyType typ = do
  styp <- stateType
  return $ foo styp typ
  where
    foo styp t =
      case t of
        Address -> styp
        _ -> uponTypeChildren (foo styp) t

isDoneState :: HW Exp
isDoneState = isInState (doneState)

isIdle :: HW Exp
isIdle = isInState (idleState)

isInActivity :: ActivityLabel -> HW Exp
isInActivity = isInState . activityState

isInState :: EnumValue -> HW Exp
--isInState = return . bitwire . inStateWire
isInState st = do
  stw <- getStateWire
  t <- stateType
  return $ EOpApp Eq [EWire stw `ofType` t, EEnum st `ofType` t] `ofType` bit

-- Get the argument wire we should be putting arguments on. If the action has a
-- adapter then we use the one to the adapter, otherwise we use the raw one.
actionEffectiveInputWire :: ActionName -> VarName -> HW Wire
actionEffectiveInputWire action arg = do
  nh <- needsAdapter action
  return $ if nh
    then adapterInputWire action arg
    else our $ actionInputWire action arg

-- Get the result wire we should be taking results from. If the action has a
-- adapter then we use the one from the adapter, otherwise we use the raw one.
actionEffectiveOutputWire :: ActionName -> VarName -> HW Wire
actionEffectiveOutputWire action res = do
  nh <- needsAdapter action
  return $ if nh
    then adapterOutputWire action res
    else our $ actionOutputWire action res

-- Does an action need an adapter?
needsAdapter :: ActionName -> HW Bool
needsAdapter action = do
  proto <- actionProtocol action
  let pdata = proto_data proto
  let power = proto_power proto
  return $ case (pdata, power) of
    (Immediate _, AlwaysOn) -> False
    _ -> True

-- Used by most flops in the main module.
sleepClock :: Exp
sleepClock = bitwire sleepClockWire

-- Used by most flops in the main module.
sleepReset :: Maybe Exp
sleepReset = Just $ bitwire sleepResetWire

getStateWire :: HW Wire
getStateWire = do
  env <- getEnv
  return $ stateWire $ mainModuleName env

----------------------------------------
-- Lookups of program stuff
----------------------------------------

actionInfo :: ActionName -> HW ActionInfo
actionInfo action = do
  env <- getEnv
  return $ mustFindActionInfo env action

actionProvider :: ActionName -> HW ActionProvider
actionProvider = liftM ai_provider . actionInfo

actionProtocol :: ActionName -> HW Protocol
actionProtocol = liftM ai_protocol . actionInfo

-- Type of states for the main module's fsm.
stateType :: HW Type
stateType = do
  env <- getEnv
  return $ NamedType $ fsmStateTypeName env


----------------------------------------
-- Monad mutation misery & co
----------------------------------------

-- Clear the module under construction and begin a fresh one.
newModule :: ModuleName -> HW ()
newModule name = putModule $ emptyModule name

-- The current module under construction is done; add it to the modules list.
moduleDone :: HW ModuleName
moduleDone = do
  st <- get
  let mod = hws_work st
  let undef = mod_dbg_undefined mod
  when (not $ S.null undef) $ problem $
    "wires were left without well-defined signals: " ++
    (joinWith ", " $ map show $ S.toList undef)
  let name = mod_name mod
  when (name `elem` (map mod_name $ hws_mods st)) $ problem $
    "multiple modules created with same name: " ++ show name
  put $ st { hws_mods = (hws_mods st) ++ [mod] }
  return $ name

-- Set aside the module we're currently working on, do something else (like
-- building a totally different module), and then restore the original module
-- so that we can continue working on it. The result of the "something else"
-- task (for example, a module name) is returned.
diversion :: HW a -> HW a
diversion task = do
  work <- getModule
  res <- task
  putModule work
  return res

getFlavor :: HW Flavor
getFlavor = liftM hws_flavor get

getProtocol :: HW Protocol
getProtocol = liftM env_protocol getEnv


-- Get the module we're building at the moment.
getModule :: HW HardwareModule
getModule = liftM hws_work get

-- Set the module we're building at the moment.
putModule :: HardwareModule -> HW ()
putModule mod = modify $ \st -> st { hws_work = mod }

-- Modify the module we're building at the moment.
modifyModule :: (HardwareModule -> HardwareModule) -> HW ()
modifyModule f = modify $ \st -> st { hws_work = f (hws_work st) }

getEnv :: HW Environment
getEnv = liftM hws_env get

putEnv :: Environment -> HW ()
putEnv env = modify $ \st -> st { hws_env = env }

getProg :: HW SyncProgram
getProg = liftM hws_prog get

-- Does a wire with a certain name exist in the module?
wireExists :: Wire -> HW Bool
wireExists wire = do
  mod <- getModule
  return $ M.member wire $ mod_wires mod

-- Add a wire to the module we're building at the moment.
addWire :: WireRole -> Wire -> Type -> Maybe VisLabel -> HW Wire
addWire role wire typ mlabel = do
  debug $ "adding wire: " ++ wire
  mod <- getModule
  checkTypeWirified typ
  exists <- wireExists wire
  when exists $ problem $
    "wire " ++ show wire ++ " created/added multiple times"
  putModule $
    let wires = mod_wires mod
        wires' = M.insert wire (WireInfo role typ $ mlabel) wires
        io = mod_ordered_io mod
        int = mod_ordered_internal mod
        undef = mod_dbg_undefined mod
        io' = if role == InternalWire then io else io ++ [wire]
        int' = if role == InternalWire then int ++ [wire] else int
        undef' = if role == InputWire then undef else S.insert wire undef
        mod' = mod
          { mod_wires = wires'
          , mod_ordered_io = io'
          , mod_ordered_internal = int'
          , mod_dbg_undefined = undef'
          }
    in mod'
  return wire

-- Equate a wire to an expression. Don't forget to wirify the expression first.
equateWire :: Wire -> Exp -> HW Wire
equateWire wire exp = do
  debug $ "equating wire: " ++ wire
  checkAwaitingDef wire
  checkWirified exp
  mod <- getModule
  when (M.notMember wire $ mod_wires mod) $ problem $
    "attempted to define signal of nonexistent wire " ++ show wire
  let (Just info) = M.lookup wire $ mod_wires mod
  when (wi_role info == InputWire) $ problem $
    "attempted to define signal of input wire " ++ show wire
  let wireType = wi_type info
  let expType = typeOf exp
  when (wireType /= expType) $ problem $
    "type mismatch between wire " ++ show wire ++ " of type " ++
    show wireType ++ " and expression " ++ show exp ++ " of type " ++
    show expType
  let equations' = M.insert wire exp $ mod_equations mod
  let mod' = mod { mod_equations = equations' }
  putModule mod'
  promiseDefined wire
  return wire

addEquateWire :: WireRole -> Wire -> Maybe VisLabel -> Exp -> HW Wire
addEquateWire role wire mlabel exp = do
  addWire role wire (typeOf exp) mlabel
  equateWire wire exp

addFlop :: Wire -> Exp -> Exp -> Maybe Exp -> Maybe Exp -> Bool -> HW ()
addFlop wire exp clk enable reset transparent = do
  debug $ "adding flop with output wire: " ++ wire
  checkAwaitingDef wire
  checkWirified exp
  let flop = Floppish wire exp clk enable reset transparent
  mod <- getModule
  when (not $ wire `elem` mod_dbg_undefined mod) $ problem $
    "attmpted to connect flop to nonexistent or already well-defined wire " ++
    show wire
  let (Just info) = M.lookup wire $ mod_wires mod
  let wtyp = wi_type info
  let etyp = typeOf exp
  when (wtyp /= etyp) $ problem $
    "flip-flop output and input have different types " ++
    show wire ++ " of type " ++ show wtyp ++ ", " ++
    show exp ++ " of type " ++ show etyp
  let mod' = mod { mod_flops = (mod_flops mod) ++ [flop] }
  putModule mod'
  promiseDefined wire

{-
addFSM :: FSMName -> FSM -> HW ()
addFSM name fsm = do
  checkAwaitingDef $ fsm_state fsm
  mod <- getModule
  when (M.member name $ mod_fsms mod) $ problem $
    "attmpted to redefine fsm " ++ show name
  putModule $ mod { mod_fsms = M.insert name fsm $ mod_fsms mod }
  promiseDefined $ fsm_state fsm
-}

-- Instantiate some module (by name) within the current one.
instantiate :: ModuleName -> Maybe VisLabel -> [Wire] -> HW ()
instantiate name mlabel wires = do
  debug $ "Instantiating module: " ++ name
  modifyModule $ \mod ->
    let inst = ModuleInstantiation name wires mlabel
    in mod { mod_others = (mod_others mod) ++ [inst] }

-- Create a new enum type. The given type name is also used for the compiled FL.
-- The first value will correspond to 0 (useful for flops with reset).
createEnumType :: TypeName -> [EnumValue] -> EnumVariety -> HW Type
createEnumType name values variety = do
  env <- getEnv
  when (M.member name $ env_types env) $ problem $
    "redefinition of type: " ++ show name
  let types' = M.insert name (Just name) $ env_types env
  let enums' = M.insert name (values, variety) $ env_enums env
  let env' = env { env_types = types', env_enums = enums' }
  putEnv env'
  return $ NamedType name

-- Make sure that a wire does not yet have a well-defined signal (anti-bug
-- measure). Note: will also produce an error if the wire doesn't yet exist.
checkAwaitingDef :: Wire -> HW ()
checkAwaitingDef wire = do
  mod <- getModule
  when (not $ wire `elem` mod_dbg_undefined mod) $ problem $
    "wire not in set awaiting definition (either it doesn't yet exist, or " ++
    "has already been defined): " ++ show wire

-- Indicate a wire has received a well-defined signal (anti-bug measure, should
-- only be called by functions that actually define the signal of the wire, i.e.
-- equateWire, addFlop, addFSM, and also when ).
promiseDefined :: Wire -> HW ()
promiseDefined wire = modifyModule $ \mod ->
  mod { mod_dbg_undefined = S.delete wire $ mod_dbg_undefined mod }

-- Cause an error if an expression isn't wirified.
checkWirified :: Exp -> HW ()
checkWirified exp =
  if isWirified exp
    then return ()
    else problem $ "expression supposed to be wirified but isn't: " ++ show exp

-- Cause an error if a type isn't wirified.
checkTypeWirified :: Type -> HW ()
checkTypeWirified typ =
  if isTypeWirified typ
    then return ()
    else problem $ "type supposed to be wirified but isn't: " ++ show typ


-- Indicate that something went very wrong (indicating a bug in the compiler).
problem :: String -> HW a
problem msg = logMessagesAndFail [Error $ "internal error: " ++ msg]

-- Indicate that something went very wrong but it's not our fault.
nonono :: String -> HW a
nonono msg = logMessagesAndFail [Error $ "internal error but your fault: " ++ msg]

hrmm :: String -> HW ()
hrmm msg = logMessages [Warning $ "warning: " ++ msg]

debug :: String -> HW ()
debug msg = return () -- logMessages [Warning $ "debug: " ++ msg]

----------------------------------------
-- Non-monadic helpers
----------------------------------------

isWirified :: Exp -> Bool
isWirified exp =
  case getInner exp of
    EInt _ -> True
    EBlob _ -> True
    EIfThenElse _ _ _ -> recurse
    EOpApp _ _ -> recurse
    EApp _ _ -> recurse
    EField _ _ -> recurse
    EFieldUpdate _ _ _ -> recurse
    ETuple _ -> recurse
    EProj _ _ -> recurse
    ETypeAnn _ _ -> recurse
    EWire _ -> True
    EEnum _ -> True
    EDontCare -> True
    ESignExtend _ -> recurse
    _ -> False
  where
    recurse = all isWirified $ children exp

isTypeWirified :: Type -> Bool
isTypeWirified typ =
  case typ of
    Address -> False
    _ -> all isTypeWirified $ typeChildren typ

-- An empty module, except for the name.
emptyModule :: ModuleName -> HardwareModule
emptyModule name = HardwareModule name M.empty [] [] M.empty [] {- M.empty -} [] S.empty

-- Save some typing.
bitwire :: Wire -> Exp
bitwire w = EWire w `ofType` bit

-- Save some typing but still with typing.
typewire :: Type -> Wire -> Exp
typewire t w = EWire w `ofType` t
