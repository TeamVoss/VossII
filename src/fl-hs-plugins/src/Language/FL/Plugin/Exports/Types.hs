-- | Types explaining Haskell functions and types to Fl.
module Language.FL.Plugin.Exports.Types where
import Foreign (Ptr, FunPtr)
import Foreign.C.String (CString)
import Data.Typeable (TypeRep)
import Language.FL.Plugin.FlPtr
import Language.FL.Plugin.Foreign.Types

data Associativity = L | R
  deriving (Show, Read, Eq)

-- | Fixity of an Fl function.
data Fixity
  = NoFixity
    -- ^ The function has no fixity. This is the default.
  | Prefix Int
    -- ^ The function is a prefix operator, with the given precedence clamped to
    --   @[0,1]@.
  | Infix Associativity Int
    -- ^ The function is an infix operator with the given associativity and
    --   precedence clamped to @[0,9]@
  | Postfix
    -- ^ The function is a postfix operator.
    deriving (Show, Read, Eq)

-- | A function exported to Fl.
--   Currently, exported functions are strict in all arguments.
data Export = Export
  { -- | Name under which the function will be exported to Fl.
    funName :: String

    -- | Pointer to the implementation of the function.
  , funPtr :: FunPtr (Redex -> IO ())

    -- | Must be delayed until after types have been registered with Fl.
  , mkTypeAndArity :: IO (Ptr FlTypeExp, Int)

    -- | Any non base types appearing in the export's type signature.
  , funCustomTypes :: [TypeExport]

    -- | Fixity of the exported function.
  , fixity :: Fixity
  }

-- | A type exported to FL.
data TypeExport = TypeExport
  { exportTypeName :: String
  , exportTypeRep :: TypeRep
  , mark :: FlPtr -> IO ()
  , sweep :: IO ()
  , equals :: FlPtr -> FlPtr -> IO (Ptr Formula)
  , obj2string :: FlPtr -> IO CString
  , save :: Ptr FILE -> FlPtr -> IO ()
  , load :: Ptr FILE -> IO FlPtr
  , gmap :: Ptr GmapInfo -> FlPtr -> IO FlPtr
  , gmap2 :: Ptr GmapInfo -> FlPtr -> FlPtr -> IO FlPtr
  }

-- | Structure representing an Fl plugin.
data Plugin = Plugin
  { -- | Name of the plugin. Should be something unique, as Fl will refuse to
    --   load two plugins with the same name.
    pluginName :: String

    -- | Version of the plugin. Fl currently doesn't care about this.
  , pluginVersion :: Int

    -- | List of functions to be exported to Fl.
    --
    --   Any type which a) is mentioned in the type signature of at least one
    --   exported function, b) is an instance of 'FlType', and c) is not an Fl
    --   base type (i.e. any type @t@ such that
    --   @not (isBaseType (Proxy :: Proxy t))@) is automatically exported.
  , pluginExports :: [Export]
  }
