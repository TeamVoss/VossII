module PrettyPrint
  ( printActionStateInteraction
  , printActionType
  , printAtomicStm
  , printActivity
  , printActivityLabel
  , printSyncProgram
  , printBlock
  , printBlockProgram
  , printExp
  , printExp'
  , printDataProtocol
  , printFunPat
  , printLhs
  , printOp
  , printParam
  , printPattern
  , printPower
  , printPowerProtocol
  , printProtocol
  , printStm
  , printStructuredProgram
  , printSubroutineFlag
  , printType
  , printVar
  , bracket
  , joinWith
  , paren
  , sharedName
  ) where

import qualified Data.Map as M
import Types
import Expression
import StrUtil


-- Instances!

instance Show ActionStateInteraction where
  show = printActionStateInteraction

instance Show ActionType where
  show = printActionType

instance Show AtomicStm where
  show = printAtomicStm

instance Show SyncProgram where
  show = printSyncProgram

instance Show BlockProgram where
  show = printBlockProgram

instance Show DataProtocol where
  show = printDataProtocol

instance Show Exp where
  show = printExp

instance Show FunPat where
  show = printFunPat

instance Show Hardware where
  show = error "cannot actually show Hardware"  -- soz

instance Show Lhs where
  show = printLhs

instance Show Op where
  show = printOp

instance Show Power where
  show = printPower

instance Show PowerProtocol where
  show = printPowerProtocol

instance Show Protocol where
  show = printProtocol

instance Show Stm where
  show = printStm

instance Show StructuredProgram where
  show = printStructuredProgram

instance Show SubroutineFlag where
  show = printSubroutineFlag

instance Show Type where
  show = printType

instance Show Var where
  show = printVar


-- "Configuration"

debug :: Bool
debug = False  -- todo: unsafePerformIO to look up environment variable

displayTopLevelTypes :: Bool
displayTopLevelTypes = debug

displayAllTypes :: Bool
displayAllTypes = debug

displayLocalVarsWithSub :: Bool
displayLocalVarsWithSub = True

tab :: Int
tab = 4


-- The public print* functions, ordered alphabetically.

printActionStateInteraction :: ActionStateInteraction -> String
printActionStateInteraction asi =
  "reading " ++ list (asi_stateRead asi) ++
  " writing " ++ list (asi_stateWrite asi)
  where
    list = bracket . joinWith ", "

printActionType :: ActionType -> String
printActionType at = printFunPat (at_io at) ++ " " ++ printActionStateInteraction (at_state at)

printAtomicStm :: AtomicStm -> String
printAtomicStm astm =
  case astm of
    Assign lhs rhs -> semi $ printLhs lhs ++ " = " ++ printExp rhs
    Scissors -> "--%<--"
    HintPower action power -> semi $ unwords ["power", show power, action]
    RunSub name inline ->
      let inlineStr = if inline then " inline" else ""
      in semi $ "runsub " ++ name ++ inlineStr

printActivity :: (ActivityLabel, Activity) -> String
printActivity (name, activity) =
  "activity " ++ printActivityLabel name ++ "\n" ++ indentLines tab contents
  where
    contents = joinWith "\n" $ actions ++ updates ++ powers ++ next
    actions = map printAction $ M.toList $ actv_actions activity
    powers = map printPowerSetting $ M.toList $ actv_powers activity
    updates = map printUpdate $ M.toList $ actv_updates activity
    next = ["NEXT " ++ printExp (actv_next activity)]
    printAction (name, tree) = "ACTION " ++ name ++ " " ++ printTree tree
    printTree act =
      case act of
        ADepends exp t1 t2 -> "if " ++ printExp exp ++
          " then " ++ printTree t1 ++
          " else " ++ printTree t2
        ACall exps -> decorate $ joinWith ", " $ map printExp exps
        ADontCall -> "SKIP"
    printUpdate (var, exp) = "UPDATE " ++ printVar var ++ " := " ++ printExp exp
    printPowerSetting (action, ptree) = "POWER " ++ action ++ ": " ++
      printPowerTree ptree
    printPowerTree t =
        case t of
          PowDepends exp t1 t2 -> "if " ++ printExp exp ++
            " then " ++ printPowerTree t1 ++
            " else " ++ printPowerTree t2
          PowSet pow -> show pow
          PowDontSet -> "as-is"
    --decorate s = "«" ++ s ++ "»"
    decorate s = "" ++ s ++ ""

printActivityLabel :: ActivityLabel -> String
printActivityLabel (name, index) = name ++ "." ++ show index

printSyncProgram :: SyncProgram -> String
printSyncProgram bitp =
  -- nameLine ++ "\n" ++
  ioLine ++ "\n" ++
  entryLine ++ "\n" ++
  "\n" ++
  activities ++ "\n" ++
  "\n" ++
  shared ++ "\n"
  where
    -- nameLine = "// " ++ bitp_name bitp
    ioLine = "// program type: " ++ printFunPat (bitp_io bitp)
    entryLine = "// entry = " ++ printActivityLabel (bitp_entry bitp)
    activities = joinWith "\n\n" $ map printActivity $ M.toList $ bitp_activities bitp
    shared = joinWith "\n" $ map printShared $ M.toList $ bitp_shared bitp

printBlock :: (BlockLabel, Block) -> String
printBlock (name, Block stms next) =
  let pname = printBlockLabel name
      pstms = joinWith "\n" $ map printAtomicStm stms
      pnext = printNextBlock next
  in "block " ++ pname ++ "\n{\n" ++
     indentLines tab (pstms ++ "\n\n" ++ pnext) ++
     "\n}"

printBlockLabel :: BlockLabel -> String
printBlockLabel (name, index) = name ++ "." ++ show index

printBlockProgram :: BlockProgram -> String
printBlockProgram blop = 
  -- "// " ++ blop_name blop ++ "\n" ++
  "// main subroutine: " ++ blop_main blop ++ "\n" ++
  joinWith "\n" (map
                  (\(sub, lbl) ->
                      "// " ++ sub ++ " -> " ++ printBlockLabel lbl)
                  $ M.toList $ blop_subs blop) ++
  "\n\n" ++ 
  joinWith "\n\n" (map printBlock $ M.toList $ blop_blocks blop)

printDataProtocol :: DataProtocol -> String
printDataProtocol proto =
  case proto of
    Immediate b -> if b then "immediate-req" else "immediate"
--    TwoPhase -> "twophase"
--    FourPhase -> "fourphase"
--    PulseEcho -> "pulseecho"
--    AckPulse -> "ackpulse"
--    Agreement -> "agreement"
    OtherDataProtocol name reqt ackt -> joinWith ":" [name, show reqt, show ackt]  -- TODO fix

printExp :: Exp -> String
printExp = printExp' displayTopLevelTypes displayAllTypes

printFunPat :: FunPat -> String
printFunPat fp =
  joinWith " -> " $ map printPattern $ fp_arguments fp ++ [fp_result fp]

printLhs :: Lhs -> String
printLhs lhs =
  case lhs of
    LhsVarField v fs -> joinWith "-->" $ printVar v : fs
    LhsTuple lhss -> paren $ joinWith ", " $ map printLhs lhss
    LhsIgnore -> "_"

printOp :: Op -> String
printOp op =
  case op of
    Not -> "~"
    Or -> "|"
    And -> "&"
    Xor -> "^"
    Eq -> "=="
    Neq -> "!="
    Lt -> "<"
    Gt -> ">"
    Lte -> "<="
    Gte -> ">="
    Plus -> "+"
    Minus -> "-"
    Times -> "*"
    Div -> "/"
    Mod -> "%"
    ShiftL -> "<<"
    ShiftR -> ">>"
    ArithShiftR -> "|>>"

printParam :: Param -> String
printParam (name, typ) = name ++ ":" ++ printType typ

printPattern :: Pattern -> String
printPattern pat =
  case pat of
    PSingle parm -> printParam parm
    PTuple pats -> paren $ joinWith ", " $ map printPattern pats

printPower :: Power -> String
printPower p =
  case p of
    PowerOn -> "on"
    PowerOff -> "off"

printPowerProtocol :: PowerProtocol -> String
printPowerProtocol proto =
  case proto of
    AlwaysOn -> "alwayson"
    ClockGating -> "clockgating"
    --PowerShake -> "powershake"

printProtocol :: Protocol -> String
printProtocol proto =
  case proto of
    ProtocolAuto -> "auto"
    Protocol clock reset dat pwr ->
      "{{ clock = " ++ (if proto_clock proto then "1" else "0") ++
      ", reset = " ++ (if proto_reset proto then "1" else "0") ++
      ", data = " ++ printDataProtocol (proto_data proto) ++
      ", power = " ++ printPowerProtocol (proto_power proto) ++
      " }}"

printStm :: Stm -> String
printStm stm =
  case flatten stm of
    [_] -> printNonSeqStm stm
    things -> joinWith "\n" $ map printStm things
  where
    flatten :: Stm -> [Stm]
    flatten s =
      case s of
        Seq s1 s2 -> flatten s1 ++ flatten s2
        _ -> [s]

printStructuredProgram :: StructuredProgram -> String
printStructuredProgram strp =
  -- "name " ++ strp_name strp ++ "; " ++
  "// main subroutine: " ++ show (strp_main strp) ++ "\n\n" ++
  joinWith "\n\n" (map printSub $ M.toList $ strp_subs strp)

printSubroutineFlag :: SubroutineFlag -> String
printSubroutineFlag flag =
  case flag of
    Inline -> "inline"

printType :: Type -> String
printType t =
  case typeSpine t of
    [t'] ->
      case t' of
        Untyped -> "\129448"  -- unicode skunk
        NamedType name -> name
        Address -> "ADDRESS"
        Product ts -> paren $ joinWith ", " $ map printType ts
        FLType s -> "#FL#" ++ s
    spine -> paren $ joinWith " -> " $ map printType spine
  where
    typeSpine ta =
      case ta of
        Fun tb tc -> tb:(typeSpine tc)
        _ -> [ta]

printVar :: Var -> String
printVar v =
  case v of
    UnresolvedVar vn -> "?" ++ vn
    GlobalVar vn -> vn
    LocalVar sub vn ->
      if displayLocalVarsWithSub
        then vn ++ bracket sub
        else vn
    ReturnAddr sub -> "return" ++ bracket sub
    MiscVar i -> "$t" ++ show i


-- Helper for printExp and printSyncProgram
sharedName :: Integer -> String
sharedName i = "$e" ++ show i


-- Private helpers.

-- Helper for printExp
printExp' :: Bool -> Bool -> Exp -> String
printExp' topType innerTypes exp@(Exp inner label) =
  case spine of
    [_] -> printSingleExp topType innerTypes exp
    _ ->
      let pieces = map (printExp' innerTypes innerTypes) spine
          assembled = joinWith " " pieces
      in if topType
         then wrapWithLabel label assembled
         else paren assembled
  where
    spine = expSpine exp

-- Helper for printExp
printSingleExp :: Bool -> Bool -> Exp -> String
printSingleExp topType innerTypes exp@(Exp inner label) =
  let innerString = printSingleInnerExp innerTypes inner
  in if topType
    then wrapWithLabel label innerString
    else innerString

-- Helper for printExp
wrapWithLabel :: ExpLabel -> String  -> String
wrapWithLabel label s = "[" ++ s ++ " : " ++ printExpLabel label ++ "]"

-- Helper for printExp
printExpLabel :: ExpLabel -> String
printExpLabel = printType . el_type

-- Helper for printExp
printSingleInnerExp :: Bool -> InnerExp -> String
printSingleInnerExp types inner =
  case inner of
    EVar v -> printVar v
    EInt i -> show i
    EBlob s -> show s
    EIfThenElse e1 e2 e3 ->
      let s1 = recurse e1
          s2 = recurse e2
          s3 = recurse e3
      in paren $ "if " ++ s1 ++ " then " ++ s2 ++ " else " ++ s3
    EOpApp op args -> printOpApp types op args
    EField f e1 ->
      let s1 = recurse e1
      in s1 ++ "-->" ++ f
    EFieldUpdate f e1 e2 ->
      let s1 = recurse e1
          s2 = recurse e2
      in paren $ s1 ++ "-->" ++ f ++ "=" ++ s2
    ETuple es ->
      let ss = map recurse es
      in paren $ joinWith ", " ss
    EProj i e1 ->
      let s1 = recurse e1
      in s1 ++ "." ++ show i
    ETypeAnn t e1 ->
      let s1 = recurse e1
      in paren $ s1 ++ " :: " ++ printType t
    EActionCall name es ->
      let ss = map recurse es
      in paren $ unwords $ ["do", name] ++ ss
    ESubCall name es ->
      let ss = map recurse es
      in paren $ unwords $ ["call", name] ++ ss
    EActivityLabel bl -> printActivityLabel bl
    EShared i -> sharedName i
    EActionResult action param -> action ++ "!" ++ param
    EWire w -> w
    EEnum s -> s
    EDontCare -> "DONTCARE"
    ESignExtend e ->
      let s = recurse e
      in paren $ "SX " ++ s
    EStr s -> show s
    EDict m -> "{{ " ++ joinWith ", " (map (\(k,v) -> k ++ " = " ++ recurse v) $ M.toList m) ++ " }}"
    where
      recurse = printExp' types types

-- Helper for printExp
printOpApp :: Bool -> Op -> [Exp] -> String
printOpApp types op args =
  let arity = opArity op
  in if arity == length args
    then case opPosition op of
      Prefix -> paren $ joinWith " " (printOp op : map recurse args)
      Infix -> case args of
        [a0, a1] -> paren $ joinWith " " [recurse a0, printOp op, recurse a1]
        _ -> malformed "Infix operator with !=2 operands!?"
    else malformed "OpApp with wrong arity"
  where
    malformed msg = msg ++ ": " ++ joinWith " " (printOp op : map recurse args)
    recurse = printExp' types types

-- Helper for printStructuredProgram
printSub :: (SubroutineName, Stm) -> String
printSub (name, stm) = "subroutine " ++ name ++ "\n" ++ printBracketedStm stm

-- Helper for printStm
printNonSeqStm :: Stm -> String
printNonSeqStm stm =
  case findBranches stm of
    ([], _) -> printNonSeqNonBranchStm stm
    (branches, otherwise) -> printBranches branches otherwise
  where
    findBranches s =
      case s of
        Branch cond s1 s2 ->
          let (branches, otherwise) = findBranches s2
          in ((cond, s1):branches, otherwise)
        _ -> ([], s)

-- Helper for printStm
printBranches :: [(Exp, Stm)] -> Stm -> String
printBranches branches otherwise = foo True branches
  where
    foo isFirst branches' =
      case branches' of
        ((cond,stm):bs) ->
          let keyword = if isFirst then "if" else "else if"
              ifLine = keyword ++ parenIfNeeded (printExp cond)
          in ifLine ++ "\n" ++ printBracketedStm stm ++ "\n"
        [] -> "else\n" ++ printBracketedStm otherwise

-- Helper for printStm
printNonSeqNonBranchStm :: Stm -> String
printNonSeqNonBranchStm stm =
  case stm of
    Do astm -> printAtomicStm astm
    Nop -> semi "nop"
    Label lbl -> "label " ++ lbl ++ ":"
    Goto lbl -> "goto " ++ lbl ++ ";"
    Return -> semi "return"
    While exp stm1 -> "while" ++ parenIfNeeded (printExp exp) ++
      printBracketedStm stm1
    For stm1 exp stm2 stm3 ->
      "for(" ++
      (unsemi $ printStm stm1) ++ "; " ++
      printExp exp ++ "; " ++
      (unsemi $ printStm stm2) ++
      ")\n" ++
      printBracketedStm stm3
    ForEach var exp1 exp2 stm1 ->
      "foreach(" ++ printVar var ++ " in " ++
      printExp exp1 ++ " ... " ++ printExp exp2 ++
      ")\n" ++
      printBracketedStm stm1
    Sandwich stm1 exp stm2 ->
      let s1 = printBracketedStm stm1
          e = parenIfNeeded (printExp exp)
          s2 = printBracketedStm stm2
      in "sandwich\n" ++
         s1 ++ "\n" ++
         "check " ++ e ++ "\n" ++
         s2

-- Helper for printStm and printSub
printBracketedStm :: Stm -> String
printBracketedStm stm = "{\n" ++ indentLines tab (printStm stm) ++ "\n}"

-- Helper for printBlock
printNextBlock :: NextBlock -> String
printNextBlock next = "next: " ++ foo next
  where
    foo n =
      case n of
        NBGoto label -> printBlockLabel label
        NBBranch exp label1 label2 ->
          "if " ++ printExp exp ++
          " then " ++ printBlockLabel label1 ++
          " else " ++ printBlockLabel label2
        NBReturn sub -> "return from " ++ sub
        NBHCF msg -> "halt and catch fire: " ++ msg

-- Helper for printSyncProgram
printShared :: (Integer, Exp) -> String
printShared (i, exp) = sharedName i ++ " = " ++ printExp exp

