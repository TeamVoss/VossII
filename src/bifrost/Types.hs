module Types where


import qualified Data.Map as M
import qualified Data.Set as S

----------------------------------------
-- Names etc, 99% string
----------------------------------------

-- Name for the program
type ProgramName = String

-- Name of a hardware module
type ModuleName = String

-- The name given to a variable.
type VarName = String

-- The name given to a type.
type TypeName = String

-- The name of a struct field.
type Field = String

-- The name of a subroutine.
type SubroutineName = String

-- The name of an action.
type ActionName = String

-- A reference to/name of a shared expression.
type SharedRef = Integer

-- The name of a wire in the hardware model.
type Wire = String

-- An enum value in the hardware model (= state name).
type EnumValue = String


----------------------------------------
-- Expressions
----------------------------------------

-- Expressions with attached label.
data Exp = Exp InnerExp ExpLabel
  deriving (Eq, Ord)

data InnerExp
  -- Present throughout the whole pipeline (mostly)
  = EVar Var                    -- a variable (note: not present in hardware)
  | EInt Integer                -- an integer
  | EBlob String                -- a raw expression in the backend language (i.e., FL)
  | EIfThenElse Exp Exp Exp     -- if x then y else z
  | EOpApp Op [Exp]             -- an operator (built-in function) applied to args
  | EApp Exp Exp                -- a pure function applied to an argument
  | EField Field Exp            -- a field from a struct
  | EFieldUpdate Field Exp Exp  -- update a field (first arg) in a struct (second arg) to a new value (third arg)
  | ETuple [Exp]                -- a tuple
  | EProj Int Exp               -- a projection from a tuple (beginning with index 0)
  | ETypeAnn Type Exp           -- annotate an expression with a type
  -- Present only in sequential program
  | EActionCall ActionName [Exp]  -- an action call with an argument
  | ESubCall SubroutineName [Exp] -- a subroutine call with an argument
  -- Present only in synchronous program
  | EActivityLabel ActivityLabel        -- a reference to a synchronous activity
  | EShared SharedRef           -- a reference to a shared expression
  | EActionResult ActionName VarName  -- a particular result from a particular action
  -- Present only in hardware model
  | EWire Wire                  -- wire/signal in hardware
  | EEnum EnumValue             -- an enum value (used for FSM state names)
  | EDontCare                   -- the value doesn't matter (note: these only exist temporarily)
  | ESignExtend Exp             -- sign extend a signal
  -- Present only during desugaring
  | EStr String                 -- a string
  | EDict (M.Map String Exp)    -- an associative map with strings as keys
  deriving (Eq, Ord)

-- Operators we recognize.
data Op
  = Not
  | Or
  | And
  | Xor
  | Eq
  | Neq
  | Lt
  | Gt
  | Lte
  | Gte
  | Plus
  | Minus
  | Times
  | Div
  | Mod
  | ShiftL
  | ShiftR
  | ArithShiftR
  deriving (Eq, Ord)

data OpPosition = Prefix | Infix

-- What we attach to each expression usually. Wrapped up in a type in case we
-- want to do something crazy like information flow control, in which case more
-- data can be added easily.
data ExpLabel = ExpLabel
  { el_type :: Type
  }
  deriving (Eq, Ord)


----------------------------------------
-- Environment, types, vars, etc
----------------------------------------

-- The environment, keeping track of declared types (and what they compile to),
-- and information about/types of variables, subroutines, and actions.
data Environment = Environment
  { env_name :: ProgramName   -- the name of the program
  , env_protocol :: Protocol  -- what protocol the final hw should use
  , env_types :: M.Map TypeName (Maybe String)
  , env_structs :: M.Map TypeName Struct
  , env_vars :: M.Map Var VarInfo
  , env_subTypes :: M.Map SubroutineName SubroutineType
  , env_stateAspects :: S.Set GlobalStateAspect
  , env_actions :: [ActionName]  -- we care about the declaration order
  , env_actionInfos :: M.Map ActionName ActionInfo
  , env_enums :: M.Map TypeName ([EnumValue], EnumVariety)
  , env_numeric :: M.Map TypeName NumericInfo
  , env_flavor :: Maybe Flavor  -- hint about which flavor we want
  }

-- A type that an something (expression, variable, etc) can have. Types that
-- contain Untyped in them cannot necessarily be meaningfully compared, but
-- Untyped disappears during typechecking.
data Type
  = Untyped              -- pre-typechecking, not a valid type
  | NamedType TypeName   -- we pretend to have an idea
  | Address              -- a return address (removed during Hardware stage)
  | Product [Type]       -- n-ary product
  | Fun Type Type        -- fun fun functions
  | FLType String        -- an anonymous type backed by some FL type
  deriving (Eq, Ord)

data Struct = Struct
  { struct_fields :: M.Map Field Type
  }

data NumericInfo = UnsignedNumeric { unsigned_modulo :: Integer }

-- Kinds of enums
data EnumVariety = EnumRegular | EnumMutex

-- Variables, both those declared in the program and created by the compiler.
data Var
  = UnresolvedVar VarName            -- prior to name resolution/checking
  | GlobalVar VarName                -- global vars
  | LocalVar SubroutineName VarName  -- local vars, subroutine params/results
  | ReturnAddr SubroutineName        -- subroutine return address (generated)
  | MiscVar Integer                  -- intermediate results, etc (generated)
  deriving (Eq, Ord)

-- Information about a variable.
data VarInfo = VarInfo
  { vi_type :: Type
  , vi_flags :: [VarFlag]
  }

-- Flags that can be applied to a variables.
data VarFlag
  = Transient  -- This variable *must* be compiled away. [Not implemented yet.]
  deriving (Eq, Ord)

-- Action typing information; inputs, outputs, state interaction.
data ActionType = ActionType
  { at_io :: FunPat
  , at_state :: ActionStateInteraction
  }

-- How does this type of action interact with global state?
data ActionStateInteraction = ActionStateInteraction
  { asi_stateRead :: [GlobalStateAspect]   -- it depends on these aspects...
  , asi_stateWrite :: [GlobalStateAspect]  -- ...and alters these aspects
  }

-- An aspect of the global state that an action can depend on/alter.
type GlobalStateAspect = String

-- Information about a particular action.
data ActionInfo = ActionInfo
  { ai_type     :: ActionType
  , ai_protocol :: Protocol
  , ai_provider :: ActionProvider
  , ai_array    :: Integer  -- size for arrays of actions, 0 if not array
  }

-- How a piece of hardware is driven/communicated with.
data Protocol
  = ProtocolAuto
  | Protocol
    { proto_clock :: Bool  -- has a clock?
    , proto_reset :: Bool  -- has a reset?
    , proto_data  :: DataProtocol
    , proto_power :: PowerProtocol
    }

data DataProtocol
  = Immediate Bool           -- output is immediately available; req wire optional
--  | TwoPhase               -- standard two-phase handshake
--  | FourPhase              -- standard four-phase handshake
--  | PulseEcho              -- request pulse, ack pulse (possibly immediate)
--  | AckPulse               -- request held, ack pulse (possibly immediate)
--  | Agreement              -- ready/valid but bidirectional txfr
  | OtherDataProtocol String Type Type  -- a named protocol with some types for req and ack
  deriving Eq

data PowerProtocol
  = AlwaysOn          -- the module is always on and ready to go
  | ClockGating       -- simple clock gating, no ack needed
  deriving Eq

-- The source of an action's implementation.
data ActionProvider = External | Instantiate ModuleName [ActionName]

-- Subroutine typing information; inputs, outputs, flags.
data SubroutineType = SubroutineType
  { st_io :: FunPat
  , st_flags :: [SubroutineFlag]
  }

-- Flags a subroutine can have.
data SubroutineFlag
  = Inline  -- All calls to this subroutine should be inlined.
  deriving (Eq, Ord)

-- Specification of arguments and results for an action or subroutine. A mixture
-- of function type definition and pattern-matching: each "argument" corresponds
-- to a single argument when the function is invoked and as far as its type is
-- concerned, but is represented not by a single name:type pair but rather by a
-- pattern (nested tuples of name:type pairs). The same applies to the function
-- result. Example:
--
--   n:bit -> (x:byte, y:byte) -> (w:word, u:word)
--
-- The above is a binary function with type bit -> (byte,byte) -> (word,word),
-- but is said to have three input variables (n, x, and y) and two output
-- variables (w and u).
data FunPat = FunPat
  { fp_arguments :: [Pattern]
  , fp_result :: Pattern
  }

-- Specification of an argument/result for an action or subroutine. Either a
-- single named parameter, or a tuple of patterns (n.b. recursive).
data Pattern
  = PSingle Param
  | PTuple [Pattern]

-- A single named parameter/result.
type Param = (VarName, Type)

-- Power status for actions.
data Power = PowerOn | PowerOff
  deriving (Eq, Ord)

----------------------------------------
-- Common sequential program stuff
----------------------------------------

-- "Atomic statements", statements with linear control flow. (Detours via
-- action/subroutine calls are fine.)
data AtomicStm
  = Assign Lhs Exp               -- assignment statement
  | Scissors                     -- timing hint: start a new activity
  | HintPower ActionName Power   -- request that an action be powered up/down
  | RunSub SubroutineName Bool   -- compiler-generated during chomping, bool
                                 -- indicates that the call is inline
  deriving (Eq, Ord)

-- The left-hand side of an assignment. Can have nested tuples.
data Lhs
  = LhsVarField Var [Field]  -- x, x.foo, x.foo.bar, etc
  | LhsTuple [Lhs]           -- (x,y,z)
  | LhsIgnore                -- _
  deriving (Eq, Ord)


----------------------------------------
-- "Structured" programs (already desugared somewhat)
----------------------------------------

data StructuredProgram = StructuredProgram
  { strp_main :: SubroutineName
  , strp_subs :: M.Map SubroutineName Stm
  }

data Stm
  = Do AtomicStm          -- perform an atomic statement
  | Nop                   -- lazy day
  | Seq Stm Stm           -- sequence two statements
  | Label GotoLabel       -- insert a label (and do nothing)
  | Goto GotoLabel        -- jump to a label
  | Return                -- return from a subroutine
  | Branch Exp Stm Stm    -- if(a){b}else{c}
  | While Exp Stm         -- while(a){b}
  | For Stm Exp Stm Stm   -- for(a;b;c){d}
  | ForEach Var Exp Exp Stm  -- for(i in a ... b)
  | Sandwich Stm Exp Stm  -- a;while(b){c;a} (generated by compiler)

type GotoLabel = String


----------------------------------------
-- Intermediate representation (sequential, basic blocks)
----------------------------------------

-- A program expressed as a graph of "basic blocks" (sequences of statements
-- with no internal control flow complication).
data BlockProgram = BlockProgram
  { blop_main :: SubroutineName                   -- main subroutine name
  , blop_subs :: M.Map SubroutineName BlockLabel  -- subroutine entry points
  , blop_blocks :: M.Map BlockLabel Block         -- blocks for *all subs*
  }

type BlockLabel = (String, Integer)

-- A block is a sequence of statements and a way of choosing the next block.
data Block = Block [AtomicStm] NextBlock
  deriving (Eq, Ord)

-- Possibilities for the next block. Note the lack of recursive branching.
data NextBlock
  = NBGoto BlockLabel                   -- always advance to the same block
  | NBBranch Exp BlockLabel BlockLabel  -- choose between two next blocks
  | NBReturn SubroutineName             -- return from a subroutine
  | NBHCF String                        -- halt and catch fire w/ message
  deriving (Eq, Ord)


----------------------------------------
-- Intermediate representation (synchronous)
----------------------------------------

data SyncProgram = SyncProgram
  { bitp_io :: FunPat                   -- original interface for entire program
  , bitp_inputs :: [Var]                -- flattened input variables
  , bitp_outputs :: [Var]               -- flattened output variables
  , bitp_ret :: Var                     -- return addr for entire program
  , bitp_entry :: ActivityLabel             -- entry point
  , bitp_activities :: M.Map ActivityLabel Activity  -- all of the synchronous activities
  , bitp_shared :: M.Map SharedRef Exp  -- shared combinational expressions
  }

type ActivityLabel = (String, Integer)

data Activity = Activity
  { actv_updates :: M.Map Var Exp                    -- changes to variables
  , actv_actions :: M.Map ActionName ActionCallTree  -- action calls made
  , actv_powers  :: M.Map ActionName PowerTree       -- power events
  , actv_next    :: Exp                              -- what activity to go to next
  }

-- A tree encoding how a single action is called in a activity.
data ActionCallTree
  = ADepends Exp ActionCallTree ActionCallTree  -- depending on a condition...
  | ACall [Exp]                                 -- call action with arguments
  | ADontCall                                   -- don't call the action
  deriving Eq

-- A tree encoding how the power for an action is changed in a activity.
data PowerTree
  = PowDepends Exp PowerTree PowerTree          -- depending on a condition...
  | PowSet Power                                -- turn the power on or off
  | PowDontSet                                  -- leave the power as is
  deriving Eq

----------------------------------------
-- Hardware representation
----------------------------------------

-- Note: clk and reset are assumed to be available and not made explicit in this
-- model.

data Hardware = Hardware
  { hw_primary :: ModuleName
  , hw_modules :: [HardwareModule]       -- ordered by dependence
  }

data HardwareModule = HardwareModule
  { mod_name :: ModuleName               -- the module's name
  , mod_wires :: M.Map Wire WireInfo     -- all wires, including fsm state wires
  , mod_ordered_io :: [Wire]             -- ordered input and output wires
  , mod_ordered_internal :: [Wire]       -- ordered internal wires
  , mod_equations :: M.Map Wire Exp      -- wire = expression
  , mod_flops :: [Floppish]              -- flip-flops
  , mod_others :: [ModuleInstantiation]  -- other subcomponents
  , mod_dbg_undefined :: S.Set Wire      -- wires without well-defined signals
  }

data WireInfo = WireInfo
  { wi_role :: WireRole
  , wi_type :: Type
  , wi_label :: Maybe VisLabel
  }

data WireRole = InputWire | OutputWire | InternalWire
  deriving Eq

data Floppish = Floppish
  { flop_out :: Wire
  , flop_in :: Exp
  , flop_clk :: Exp           -- the clock to use
  , flop_enable :: Maybe Exp  -- an enable, if it has one
  , flop_reset :: Maybe Exp   -- a (synchronous) reset signal, if it has one
  , flop_transparent :: Bool  -- is it transparent? (bypassed with a mux)
  }

data ModuleInstantiation = ModuleInstantiation ModuleName [Wire] (Maybe VisLabel)

-- Label to help with visualization.
type VisLabel = String

data Flavor
  = Vanilla        -- Design with IDLE state.
  | Chocolate      -- Design without IDLE state (faster but deeper logic).
  | Mint           -- Design without IDLE or DONE states.
  | Strawberry     -- Combinational design.
  deriving (Eq, Show)

