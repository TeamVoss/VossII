-- File generated by the BNF Converter (bnfc 2.9.4.1).

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | The abstract syntax of language Lang.

module Lang.AbsLang where

import Prelude (Integer, String)
import qualified Prelude as C (Eq, Ord, Show, Read)
import qualified Data.String

data Program = Prog [TopLevel]
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data TopLevel = TopDecl Decl | TopSub SubDef
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data Decl
    = NameDecl Ident
    | ProtocolDecl Protocol
    | FlavorDecl String
    | StateDecl [StateAspect]
    | ActionTypeDecl ActionTypeName ActionType
    | ActionDecl ActionName ActionType ActionArrayInfo Provider Protocol
    | GlobalVarDecl VarDecl
    | TypeDecl TypeName TypeCompileDefinition
    | TypeAliasDecl TypeName Type
    | FieldsDecl TypeName [FieldDef]
    | NumericDecl TypeName Integer
    | DefineDecl DefineName [DefineParam] Exp
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data VarDecl = MkVarDecl Type [VarName]
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data StateAspects = StateAspectList [StateAspect]
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data ActionArrayInfo = ActionSingle | ActionArray Exp
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data Provider
    = ProviderExternal
    | ProviderModule String
    | ProviderModuleShare String [ActionName]
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data Protocol = ProtocolAuto | ProtocolGiven Exp
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data TypeCompileDefinition
    = TypeCompileString String | TypeCompileInfer
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data FieldDef = MkFieldDef FieldName Type
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data FieldName = FieldNameIdent Ident | FieldNameString String
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data FunPat = FunPatPat Pattern | FunPatFun Pattern FunPat
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data Pattern
    = PatternParam Param
    | PatternTupleEmpty
    | PatternTupleTwoPlus Pattern [Pattern]
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data Param = MkParam Ident Type
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data ActionType
    = ActionTypeRaw FunPat StateAspects StateAspects
    | ActionTypeNamed ActionTypeName
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data Type
    = FunType Type Type
    | NamedType Ident
    | TupleZeroType
    | TupleTwoPlusType Type [Type]
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data SubDef = MkSub [SubFlag] SubroutineName FunPat [SubLine]
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data SubFlag = Inline
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data SubLine = SubVarDecl VarDecl | SubStm Stm
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data Stm = Normal NormalStm | Abnormal AbnormalStm
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data NormalStm
    = Assign Assignish
    | Return
    | Goto GotoLabel
    | HintPower Power ActionName
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data Power = PowerOn | PowerOff
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data Assignish
    = WithLhs Exp Exp
    | WithoutLhs Exp
    | PlusEquals Exp Exp
    | MinusEquals Exp Exp
    | Increment Exp
    | Decrement Exp
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data AbnormalStm
    = Label GotoLabel
    | Scissors
    | While Exp StmBlock
    | For Assignish Exp Assignish StmBlock
    | ForEach VarName Exp Exp StmBlock
    | Block StmBlock
    | IfLike IfLikeStm
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data IfLikeStm
    = IfElse Exp StmBlock PostElseStm | IfOnly Exp StmBlock
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data PostElseStm = ElseIf IfLikeStm | ElseBlock StmBlock
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data StmBlock = MkBlock [Stm]
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data StateAspect = MkStateAspect Ident
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data TypeName = MkTypeName Ident
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data VarName = MkVarName Ident
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data ActionTypeName = MkActionTypeName Ident
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data ActionName = MkActionName Ident
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data DefineName = MkDefineName Ident
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data DefineParam = MkDefineParam Ident
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data SubroutineName = MkSubroutineName Ident
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data GotoLabel = MkGotoLabel Ident
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data Call
    = ActionCall ActionName Args | SubCall SubroutineName Args
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data Args = MkArgs [Exp]
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data Dict = MkDict [DictAssig]
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data DictAssig = MkDictAssig Ident Exp
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data Exp
    = EIfThenElse Exp Exp Exp
    | EOr Exp Exp
    | EAnd Exp Exp
    | EXor Exp Exp
    | EEq Exp Exp
    | ENeq Exp Exp
    | ELt Exp Exp
    | EGt Exp Exp
    | ELte Exp Exp
    | EGte Exp Exp
    | EPlus Exp Exp
    | EMinus Exp Exp
    | ETimes Exp Exp
    | EDiv Exp Exp
    | EMod Exp Exp
    | EShiftL Exp Exp
    | EShiftR Exp Exp
    | EArithShiftR Exp Exp
    | ENot Exp
    | ECall Call
    | EApp Exp Exp
    | ETypeAnn Exp Type
    | EBlob String
    | EVar VarName
    | EIntLit Integer
    | EField Exp FieldName
    | ETupleZero
    | ETupleTwoPlus Exp [Exp]
    | EProj Exp Integer
    | EIgnore
    | EStr String
    | EDict Dict
  deriving (C.Eq, C.Ord, C.Show, C.Read)

newtype Ident = Ident String
  deriving (C.Eq, C.Ord, C.Show, C.Read, Data.String.IsString)
