module FLOut (genFL) where

import Control.Monad
import Data.List (delete, sort)
import qualified Data.Map as M
import Data.Maybe (fromJust, isNothing)

import Types
import Expression
import Environment
import KitchenSink
import Message
import Compile
import PrettyPrint
import StrUtil

type F = KitchenSink Message FLState

data FLState = FLState
  { fls_env :: Environment
  }

genFL :: Hardware -> Compile String
genFL hw = mkCompile $ \env ->
  let initial = FLState env
      (mfl, messages, final) = runKitchenSink (flHardware hw) initial
  in case (mfl, filter isError messages) of
    (Just fl, []) -> (messages, Just (fls_env final, fl ++ "\n"))
    _ -> (messages, Nothing)

flHardware :: Hardware -> F String
flHardware hw =
  let primaryName = hw_primary hw
      [primaryMod] = filter ((== primaryName) . mod_name) $ hw_modules hw
  in do
    let header = "// " ++ primaryName ++ " (autogenerated)\n" ++
                 "\ncload \"ste.fl\";\n"
    env <- getEnv
    enums <- mapM flEnum $ M.toList $ env_enums env
    mods <- mapM (\mod -> flModule mod (mod_name mod == primaryName)) $ hw_modules hw
    examine <- flExamine primaryMod
    return $ joinWith "\n\n" ([header] ++ enums ++ mods ++ [examine])

-- Create an FL function that constructs a hardware module.
flModule :: HardwareModule -> Bool -> F String
flModule mod isMainModule = do
  let name = mod_name mod
  let header = (if isMainModule then ["// BIFROST_MAINMODULE"] else []) ++ ["let " ++ mod_name mod ++ " = "]
  commonDecls <- mapM flWireDecl $ common
  ioDecls  <- mapM (flWireDecl . lookupWire) $ mod_ordered_io mod
  intDecls <- mapM (flWireDecl . lookupWire) $ mod_ordered_internal mod
  let decls = ["// common"] ++ commonDecls ++
              ["// input-output"] ++ ioDecls ++
              ["// internal"] ++ intDecls
  let cellBegin = ["CELL \"" ++ name ++"\" ["]
  cellInsts <- mapM (flInst mod) $ mod_others mod
  cellFlops <- mapM (flFlop mod) $ mod_flops mod
  cellEquations <- mapM (flEquation mod) $ M.toList $ mod_equations mod
  let cellContents = lines $ joinWith ",\n" $
        cellInsts ++ cellFlops ++ cellEquations
  let cellEnd = ["]; bifrost_simplify " ++ mod_name mod ++ ";"]
  let footer = ["pcompile " ++ mod_name mod ++ ";"]
  return $ unlines $
    header ++
    (bump $
      decls ++
      cellBegin ++
      (bump cellContents) ++
      cellEnd) ++
    footer
  where
    lookupWire :: Wire -> (WireRole, Wire, Type)
    lookupWire w =
      let (Just info) = M.lookup w $ mod_wires mod
      in (wi_role info, w, wi_type info)

-- Create wire delcarations, e.g. "bit_input foo.".
flWireDecl :: (WireRole, Wire, Type) -> F String
flWireDecl (role, wire, typ) = do
  maybePrintedType <- flNamedType typ
  let typePrefix = maybe "" (\s -> s ++ "_") maybePrintedType
  let roleSuffix = case role of
        InputWire -> "input"
        OutputWire -> "output"
        InternalWire -> "internal"
  flwire <- flWire wire
  return $ typePrefix ++ roleSuffix ++ " " ++ flwire ++ "."

-- Create a module instantiation, e.g. "adder x y z".
flInst :: HardwareModule -> ModuleInstantiation -> F String
flInst mod (ModuleInstantiation name wires mlabel) = do
  wires' <- mapM flWire $ map (\(_,w,_)->w) common ++ wires
  return $ flLabel mlabel $ unwords $ name:wires'

-- Create flip-flop instantiations, e.g. "re_ff clk ack was_ack".
flFlop :: HardwareModule -> Floppish -> F String
flFlop mod flop = do
  let (Just wi) = M.lookup (flop_out flop) $ mod_wires mod
  let mlabel = wi_label wi
  dout <- flWire $ flop_out flop
  din <- flExp $ flop_in flop
  clk <- flExp $ flop_clk flop
  maybeRst <- case flop_reset flop of
                Just reset -> liftM Just $ flExp reset
                Nothing -> return Nothing
  maybeEn <- case flop_enable flop of
               Just en -> liftM Just $ flExp en
               Nothing -> return Nothing
  (variety, args) <-
        case (maybeEn, maybeRst, flop_transparent flop) of
          (Nothing, Nothing, False) -> return ("", [clk, din, dout])
          (Nothing, Nothing, True) -> nonsense
          (Nothing, Just rst, False) -> return ("_sreset", [clk, rst, din, dout])
          (Nothing, Just _, True) -> nonsense
          (Just en, Nothing, False) -> return ("_en", [clk, en, din, dout])
          (Just en, Nothing, True) -> return ("_en_mux", [clk, en, din, dout])
          (Just en, Just rst, False) -> return ("_en_sreset", [clk, en, rst, din, dout])
          (Just en, Just rst, True) -> return ("_en_sreset_mux", [clk, en, rst, din, dout])
  return $ flLabel mlabel $ unwords $ ("re_ff" ++ variety):args
  where
    nonsense = ourFaultReallyBad $
      "nonsense flop on wire " ++ show (flop_out flop) ++
      "; transparent but no enable"

-- Create "lhs <- rhs" statements. If the rhs type is not a nice named type,
-- then it is likely the wire's declaration doesn't mention its type, so we add
-- a typecast if possible in order to help the type system deduce the wire's
-- type.
flEquation :: HardwareModule -> (Wire, Exp) -> F String
flEquation mod (wire, exp) = do
  let (Just wi) = M.lookup wire $ mod_wires mod
  let mlabel = wi_label wi
  lhs <- flWire wire
  rhs <- flExp exp
  let typ = typeOf exp
  mRhsNamedType <- flNamedType typ
  mRhsType <- flType typ
  let shouldCast = isNothing mRhsNamedType
  let rhs' = case (shouldCast, mRhsType) of
               (True, Just rhsType) -> paren $
                 "{" ++ rhs ++ "::" ++ rhsType ++ "}"
               _ -> rhs
  return $ flLabel mlabel $ lhs ++ " <- " ++ rhs'

-- Create an enum type declaration.
flEnum :: (TypeName, ([EnumValue], EnumVariety)) -> F String
flEnum (name, (values, variety)) = do
  mname' <- flNamedType (NamedType name)
  let command = case variety of
        EnumRegular -> "ENUM"
        EnumMutex -> "MUTEX_ENUM"
  case mname' of
    Just name' -> do
      values' <- mapM flEnumValue values
      let valueLines = lines $ joinWith ",\n" $ map show values'
      return $ unlines $
        [unwords [command, show name', "["]] ++
        bump valueLines ++
        ["];"]
    Nothing -> ourFaultReallyBad $
      "enum type name doesn't have a corresponding FL type name: " ++ show name

-- Create an FL function to instantiate a module with symbolic inputs so that
-- we can examine its circuitry.
flExamine :: HardwareModule -> F String
flExamine mod = do
  let name = mod_name mod
  inputs <- mapM flWire $ map (\(_,w,_)->w) common ++ mod_ordered_io mod
  let args = map ("'" ++) inputs
  let inst = unwords $ name:args
  let s = "STE_debug " ++ (paren $ "pexlif2fsm " ++ (paren inst))
  return $ unwords $ ["let", "examine_" ++ name, "=", s]

-- Turn a wire into an FL identifier.
flWire :: Wire -> F String
flWire w = do
  when (not $ isValidFLWireName w) $ ourFault $
    "invalid FL wire name: " ++ show w
  return w

-- Maybe label a declaration.
flLabel :: Maybe VisLabel -> String -> String
flLabel mLabel decl =
  case mLabel of
    Just label -> show label ++ " ::: " ++ decl
    Nothing -> decl

-- Convert an FL expression to a string. Parenthesized if not trivial.
flExp :: Exp -> F String
flExp exp =
  case getInner exp of
    EInt i -> castme ("'" ++ show i)
    EBlob s -> castme (paren s)
    EIfThenElse e1 e2 e3 -> do
      s1 <- flExp e1
      s2 <- flExp e2
      s3 <- flExp e3
      return $ paren $ "IF " ++ s1 ++ " THEN " ++ s2 ++ " ELSE " ++ s3
    EOpApp op operands ->
      case (op, operands, map getInner operands) of
        (Eq, [_,e], [EEnum v, _]) -> do
          e' <- flExp e
          return $ paren $ "is_" ++ v ++ " " ++ e'
        (Eq, [e,_], [_, EEnum v]) -> do
          e' <- flExp e
          return $ paren $ "is_" ++ v ++ " " ++ e'
        _ -> do
          operands' <- mapM flExp operands
          flOpApp op operands'
    EApp e1 e2 -> do
      s1 <- flExp e1
      s2 <- flExp e2
      return $ paren $ s1 ++ " " ++ s2
    EField f e -> do
      s <- flExp e
      return $ paren $ s ++ "-->" ++ f
    EFieldUpdate f e1 e2 -> do
      s1 <- flExp e1
      s2 <- flExp e2
      return $ paren $ s1 ++ "<--" ++ f ++ " ? " ++ s2
    ETuple exps -> do
      strs <- mapM flExp exps
      return $ paren $ joinWith ", " strs
    EProj i e -> do
      let (Product ts) = typeOf e
      s <- flExp e
      flProjection i (length ts) s
    ETypeAnn t e -> do
      s <- flExp e
      if t == typeOf exp
        then castme s
        else ourFaultReallyBad $
          "annotation type doesn't match exp type: " ++
          unwords [show e, show t, show $ typeOf exp]
    EWire w -> flWire w
    EEnum e -> flEnumValue e
    ESignExtend e -> do
      let t1 = typeOf e
      let t2 = typeOf exp
      s <- flExp e
      inner <- cast s t1
      cast (paren $ "SX " ++ inner) t2
    _ -> ourFaultReallyBad $ "unexpected expression in flExp: " ++ show exp
  where
    castme expString = cast expString (typeOf exp)
    cast expString typ = do
      mt <- flType typ
      case mt of
        Just t -> return $ paren $ "{" ++ expString ++ "::" ++ t ++ "}"
        Nothing -> do
          warn $ "could not express type: " ++ show typ ++ " in FL, unable " ++
            "to write typecast when converting expression to FL: " ++ show exp
          return $ paren expString


flOpApp :: Op -> [String] -> F String
flOpApp op args =
  case op of
    Not -> unary "~"
    Or -> binary "|"
    And -> binary "&"
    Xor -> binary "^"
    Eq -> binary "="
    Neq -> binary "!="
    Lt -> binary "<"
    Gt -> binary ">"
    Lte -> binary "<="
    Gte -> binary ">="
    Plus -> binary "+"
    Minus -> binary "-"
    Times -> binary "*"
    Div -> binary "/"
    Mod -> binary "%"
    ShiftL -> binary "<<"
    ShiftR -> binary ">>"
    ArithShiftR -> binary "|>>"
  where
    unary o =
      let o' = "'" ++ o ++ "'"
          [a0] = args
      in helper [o',a0]
    binary o =
      let o' = "'" ++ o ++ "'"
          [a0,a1] = args
      in helper [a0,o',a1]
    helper = return . paren . unwords

flProjection :: Int -> Int -> String -> F String
flProjection index tupleSize expString =
  if index < 0 || index >= tupleSize
    then ourFaultReallyBad $ "project index negative or larger than tuple size"
    else if tupleSize == 1
      then return $ expString
      else if index == 0
        then return $ first expString
        else flProjection (index - 1) (tupleSize - 1) (second expString)
  where
    first s = paren $ "fst " ++ s
    second s = paren $ "snd " ++ s
  
-- Turn a type into an FL string, e.g. "bit#bit". Returns Nothing if the type
-- cannot be printed (which happens if it contains a NamedType that was declared
-- without a corresponding FL type name).
flType :: Type -> F (Maybe String)
flType typ =
  case typ of
    NamedType name -> flNamedType typ
    Product ts -> do
      mts' <- mapM flType ts
      return $ do
        ts' <- sequence mts'
        case ts' of
          [] -> return "void"
          _ -> return $ foldr1 (\x y -> x ++ "#" ++ paren y) ts'
    Fun t1 t2 -> do
      mt1' <- flType t1
      mt2' <- flType t2
      return $ do
        t1' <- mt1'
        t2' <- mt2'
        return $ paren $ t1' ++ "->" ++ t2'
    FLType s -> return $ return s

-- Turn a named type into a named FL type. Returns Nothing if the named type has
-- no corresponding FL type name (i.e. it was declared without one by the user).
flNamedType :: Type -> F (Maybe String)
flNamedType typ =
  case typ of
    NamedType name -> do
      env <- getEnv
      let (Just maybeCompiledName) = M.lookup name $ env_types env
      case maybeCompiledName of
        Just compiledName ->
          if isValidFLTypeName compiledName
            then return $ Just compiledName
            else do
              yourFault $ "not a valid FL type name: " ++ show compiledName
              return Nothing
        Nothing -> return Nothing
    _ -> return Nothing

flEnumValue :: EnumValue -> F String
flEnumValue ev = do
  when (not $ isValidFLEnumValue ev) $ ourFault $
    "not a valid FL enum value: " ++ show ev
  return ev

{-
flTrue :: String
flTrue = "{'1::bit}"

flFalse :: String
flFalse = "{'0::bit}"
-}

isValidFLEnumValue :: String -> Bool
isValidFLEnumValue = isValidFLIdent

isValidFLTypeName :: String -> Bool
isValidFLTypeName = isValidFLIdent

isValidFLWireName :: String -> Bool
isValidFLWireName = isValidFLIdent

isValidFLIdent :: String -> Bool
isValidFLIdent = all (`elem` (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ ['_','\'']))

-- Wires that are shared between all modules
common :: [(WireRole, Wire, Type)]
common = [] -- [(InputWire, clockWire, bit), (InputWire, resetWire, bit)]

-- clockWire :: Wire
-- clockWire = "clk"

-- resetWire :: Wire
-- resetWire = "reset"

getEnv :: F Environment
getEnv = liftM fls_env get

--problem :: String -> F a
--problem msg = logMessagesAndFail [Error $ "internal error: " ++ msg]

yourFault :: String -> F ()
yourFault msg = logMessages [Error $ msg]

ourFault :: String -> F ()
ourFault msg = logMessages [Error $ "internal error: " ++ msg]

ourFaultReallyBad :: String -> F a
ourFaultReallyBad msg = logMessagesAndFail [Error $ "internal error: " ++ msg]

warn :: String -> F ()
warn msg = logMessages [Warning $ msg]

bump :: [String] -> [String]
bump = map $ indent 4
