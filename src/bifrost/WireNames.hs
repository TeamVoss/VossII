module WireNames where

-- This module has everything related to names of wires (and more, actually) for
-- the hardware generation stage.

import Data.Char (isAlphaNum, toUpper)
import Data.List (isInfixOf)

import Types
import PrettyPrint (joinWith)

----------------------------------------
-- Functions
----------------------------------------

-- Make a wire name from several pieces in a manner that is *injective*.
wireName :: [String] -> Wire
wireName = safeNameJoin

-- Make a general name from several pieces in a manner that is *injective*.
safeNameJoin :: [String] -> String
safeNameJoin = checkName . joinWith delim . map (escape . checkName)

-- Make sure all the characters in a name are alright.
checkName :: String -> String
checkName s =
  if all ok s
    then s
    else error $ "illegal name in hardware generation: " ++ show s
  where
    ok c = isAlphaNum c || c `elem` ['_','\'']

-- Delimeter for sections of wire names.
delim :: String
delim = "'"
-- delim = "__"

-- Escape any occurrence of the delimiter by doubling it.
escape :: String -> String
escape s =
  if delim `isInfixOf` s
    then error $ "illegal name to escape: " ++ show s
    else checkName s

-- Generate an infinite list of wires by appending digits (with no separator).
pluralize :: Wire -> [Wire]
pluralize w = [w ++ show i | i <- [0..]]

-- Keep a wire with no digits too.
pluralizeWithOriginal :: Wire -> [Wire]
pluralizeWithOriginal w = w : pluralize w

----------------------------------------
-- Wire names for main module
----------------------------------------

rawClockWire :: Wire
rawClockWire = "clk"

rawResetWire :: Wire
rawResetWire = "reset"

sleepClockWire :: Wire
sleepClockWire = "eclk"

sleepResetWire :: Wire
sleepResetWire = "ereset"

needsClockWire :: Wire
needsClockWire = wireName ["needs", "clk"]

--powerReqWire :: Wire
--powerReqWire = "pwrreq"

--powerAckWire :: Wire
--powerAckWire = "pwrack"

clockReqWire :: Wire
clockReqWire = "clkenb"

reqWires :: [Wire]
reqWires = pluralizeWithOriginal "req"

ackWires :: [Wire]
ackWires = pluralizeWithOriginal "ack"

inputWire :: Var -> Wire
inputWire arg =
  case arg of
    LocalVar _ name -> wireName ["i", name]
    _ -> error $
      "assumption violated: inputs expected to be local vars (of main)"

outputWire :: Var -> Wire
outputWire res =
  case res of
    LocalVar _ name -> wireName ["o", name]
    _ -> error $
      "assumption violated: outputs expected to be local vars (of main)"

{-
actionReqWire :: ActionName -> Wire
actionReqWire act = wireName ["a", act, "req"]

actionAckWire :: ActionName -> Wire
actionAckWire act = wireName ["a", act, "ack"]
-}

actionReqWires :: ActionName -> [Wire]
actionReqWires act = pluralizeWithOriginal $ wireName ["a", act, "req"]

actionAckWires :: ActionName -> [Wire]
actionAckWires act = pluralizeWithOriginal $ wireName ["a", act, "ack"]

actionInputWire :: ActionName -> VarName -> Wire
actionInputWire act arg = wireName ["a", act, "i", arg]

actionOutputWire :: ActionName -> VarName -> Wire
actionOutputWire act res = wireName ["a", act, "o", res]

stateWire :: ModuleName -> Wire
stateWire moduleName = wireName ["state", moduleName]

clockGateWire :: Wire
clockGateWire = "clkgate"

runningWire :: Wire
runningWire = "running"

advanceWire :: Wire
advanceWire = "advance"

updateWire :: Wire
updateWire = "update"

holdWire :: Wire
holdWire = "hold"

startWire :: Wire
startWire = "start"

doneWire :: Wire
doneWire = "done"

mayContinueWire :: Wire
mayContinueWire = wireName ["may", "continue"]

-- for chocolate and mint
initialWire :: Wire
initialWire = "init"

-- for chocolate and mint
notInitialWire :: Wire
notInitialWire = wireName ["not", "init"]

-- for chocolate and mint
continueRunningWire :: Wire
continueRunningWire = wireName ["continue", "running"]

-- for chocolate and mint
notDoneWire :: Wire
notDoneWire = wireName ["not", "done"]

-- for mint
mintFinishingWire :: Wire
mintFinishingWire = wireName ["finishing"]

-- for mint
mintTmpStateWire :: Wire
mintTmpStateWire = wireName ["state_tmp"]

-- for mint
mintTmpStateIsDoneWire :: Wire
mintTmpStateIsDoneWire = wireName ["state_tmp_done"]

-- for two-phase
wasReqWire :: Wire
wasReqWire = wireName ["was", "req"]

-- for two-phase
wasAckWire :: Wire
wasAckWire = wireName ["was", "ack"]

varWire :: Var -> Wire
varWire v = wireName $
  case v of
    GlobalVar name -> ["g", name]
    LocalVar sub name -> ["l", sub, name]
    ReturnAddr sub -> ["ret", sub]
    MiscVar index -> ["misc", show index]

varNextWire :: Var -> Wire
varNextWire v = wireName $
  case v of
    GlobalVar name -> ["ng", name]
    LocalVar sub name -> ["nl", sub, name]
    ReturnAddr sub -> ["nret", sub]
    MiscVar index -> ["nmisc", show index]

varEnableWire :: Var -> Wire
varEnableWire v = wireName $
  case v of
    GlobalVar name -> ["eg", name]
    LocalVar sub name -> ["el", sub, name]
    ReturnAddr sub -> ["eret", sub]
    MiscVar index -> ["emisc", show index]

varTmpWire :: Var -> Wire
varTmpWire v = wireName $
  case v of
    GlobalVar name -> ["tmp","g", name]
    LocalVar sub name -> ["tmp","l", sub, name]
    ReturnAddr sub -> ["tmp","ret", sub]
    MiscVar index -> ["tmp","misc", show index]

sharedWire :: SharedRef -> Wire
sharedWire index = wireName ["s", show index]

-- Dirty because the state names are already delimited
--inStateWire :: EnumValue -> Wire
--inStateWire stname = checkName $ "is" ++ delim ++ stname

actionWantWire :: ActionName -> Wire
actionWantWire act = wireName ["a", act, "want"]

actionRunWire :: ActionName -> Wire
actionRunWire act = wireName ["a", act, "run"]

actionDoneWire :: ActionName -> Wire
actionDoneWire act = wireName ["a", act, "done"]

--actionPowerReqWire :: ActionName -> Wire
--actionPowerReqWire act = wireName ["a", act, "pwrreq"]

--actionPowerAckWire :: ActionName -> Wire
--actionPowerAckWire act = wireName ["a", act, "pwrack"]

actionClockReqWire :: ActionName -> Wire
actionClockReqWire act = wireName ["a", act, "clkenb"]

adapterInputWire :: ActionName -> VarName -> Wire
adapterInputWire act input = wireName ["a", act, "hi", input]

adapterOutputWire :: ActionName -> VarName -> Wire
adapterOutputWire act output = wireName ["a", act, "ho", output]

-- Arbitrated version of an action-related signal. Takes index that specifies
-- which "tap" on the arbiter we're referring to. Note: also used inside
-- arbiter modules.
arbify :: Integer -> Wire -> Wire
arbify index wire = "arb" ++ show index ++ delim ++ wire

-- Our tap on an arbiter (vs. taps used for sharing the action with others).
our :: Wire -> Wire
our = arbify 0


----------------------------------------
-- Names of wires inside an arbiter module
----------------------------------------

arbiterInsideClockWire :: Wire
arbiterInsideClockWire = "clk"

arbiterInsideResetWire :: Wire
arbiterInsideResetWire = "reset"

{-
arbiterInsideReqWire :: Wire
arbiterInsideReqWire = "req"

arbiterInsideAckWire :: Wire
arbiterInsideAckWire = "ack"
-}

arbiterInsideReqWires :: [Wire]
arbiterInsideReqWires = pluralizeWithOriginal "req"

arbiterInsideAckWires :: [Wire]
arbiterInsideAckWires = pluralizeWithOriginal "ack"

--arbiterInsidePowerReqWire :: Wire
--arbiterInsidePowerReqWire = "pwrreq"

--arbiterInsidePowerAckWire :: Wire
--arbiterInsidePowerAckWire = "pwrack"

arbiterInsideClockReqWire :: Wire
arbiterInsideClockReqWire = "clkenb"

arbiterInsideInputWire :: ActionName -> VarName -> Wire
arbiterInsideInputWire act arg = wireName ["i", arg]

arbiterInsideOutputWire :: ActionName -> VarName -> Wire
arbiterInsideOutputWire act res = wireName ["o", res]

arbiterInsideSelectedWire :: Wire
arbiterInsideSelectedWire = "selected"

arbiterInsideShiftWire :: Wire
arbiterInsideShiftWire = "shift"

----------------------------------------
-- Names of wires within a adapter module
----------------------------------------

adapterInsideClockWire :: Wire
adapterInsideClockWire = "clk"

adapterInsideResetWire :: Wire
adapterInsideResetWire = "reset"

adapterInsideRunWire :: Wire
adapterInsideRunWire = "run"

adapterInsideDoneWire :: Wire
adapterInsideDoneWire = "done"

adapterInsideHoldWire :: Wire
adapterInsideHoldWire = wireName ["hold"]

adapterInsideManagedInputWire :: VarName -> Wire
adapterInsideManagedInputWire input = wireName ["hi", input]

adapterInsideManagedOutputWire :: VarName -> Wire
adapterInsideManagedOutputWire output = wireName ["ho", output]

{-
adapterInsideReqWire :: Wire
adapterInsideReqWire = "req"

adapterInsideAckWire :: Wire
adapterInsideAckWire = "ack"
-}

adapterInsideReqWires :: [Wire]
adapterInsideReqWires = pluralizeWithOriginal "req"

adapterInsideAckWires :: [Wire]
adapterInsideAckWires = pluralizeWithOriginal "ack"

adapterInsideInputWire :: VarName -> Wire
adapterInsideInputWire input = wireName ["i", input]

adapterInsideOutputWire :: VarName -> Wire
adapterInsideOutputWire output = wireName ["o", output]

adapterInsideLatchWire :: Wire
adapterInsideLatchWire = "latch"

adapterInsideWasReqWire :: Wire
adapterInsideWasReqWire = "was_req"

adapterInsideWasAckWire :: Wire
adapterInsideWasAckWire = "was_ack"

adapterInsideStillDoneWire :: Wire
adapterInsideStillDoneWire = "still_done"

adapterInsideSentWire :: Wire
adapterInsideSentWire = "sent"


----------------------------------------
-- State names
----------------------------------------

doneState :: EnumValue
doneState = "DONE"

idleState :: EnumValue
idleState = "IDLE"

activityState :: ActivityLabel -> EnumValue
activityState (name, index) = safeNameJoin ["S", map toUpper name, show index]


----------------------------------------
-- Misc names (warning: fast and loose)
----------------------------------------

mainModuleName :: Environment -> ModuleName
mainModuleName = checkName . env_name

adapterModuleName :: Environment -> ActionName -> ModuleName
adapterModuleName env action = checkName $
  mainModuleName env ++ delim ++ "adapter" ++ delim ++ action

arbiterModuleName :: Environment -> ActionName -> ModuleName
arbiterModuleName env action = checkName $
  mainModuleName env ++ delim ++ "arbiter" ++ delim ++ action

fsmStateTypeName :: Environment -> TypeName
fsmStateTypeName env = checkName $
  mainModuleName env ++ "_state"

-- Module to use for clock gating.
clockGateModuleName :: ModuleName
clockGateModuleName = "clockgate"

rxModuleName :: String -> ModuleName
rxModuleName = ("bifrost_rx_" ++)

txModuleName :: String -> ModuleName
txModuleName = ("bifrost_tx_" ++)

arbModuleName :: String -> ModuleName
arbModuleName = ("bifrost_arb_" ++)

----------------------------------------
-- Visualization labels
----------------------------------------

-- No label.
unlabeled :: Maybe VisLabel
unlabeled = Nothing

-- Control wires/elements that don't match anything more specific below.
control :: Maybe VisLabel
control = Just "control"

-- Datapath wires/elements that don't match anything more specific below.
datapath :: Maybe VisLabel
datapath = Just "datapath"

-- Wires used for handshakes.
handshake :: Maybe VisLabel
handshake = Just "handshake"

-- Wires that carry the current state.
statemachine :: Maybe VisLabel
statemachine = Just "statemachine"

-- Protocol adapters and arbiters.
helper :: Maybe VisLabel
helper = Just "helper"

-- Clock and reset wires, their gated variants, and also clock gates.
clkreset :: Maybe VisLabel
clkreset = Just "clkreset"

-- Child modules that implement actions (e.g. `foo` in `action ... provided by "foo" ...`).
child :: Maybe VisLabel
child = Just "child"

