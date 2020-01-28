{-# LANGUAGE DefaultSignatures #-}
module Language.FL.Plugin.FlType where
import Control.Monad ((>=>))
import qualified Data.ByteString.Char8 as BS
import Data.Typeable (Proxy (..), Typeable, typeRep)
import Foreign.Ptr (Ptr)
import Language.FL.Plugin.API (flNewExtObj, flGetExtObj, flMakeRedexExtObj)
import Language.FL.Plugin.FlPtr (deRefFlPtr)
import Language.FL.Plugin.Foreign.Types (Node, Redex, FlTypeExp)
import Language.FL.Plugin.TypeDict (typeExp, getTypeInfo)

-- | Any type which is understood by Fl.
class Typeable a => FlType a where
  -- | Is the given type an Fl base type?
  isBaseType :: Proxy a -> Bool
  isBaseType _ = False

  -- | Create an Fl type expression representing this type.
  createFlType :: Proxy a -> IO (Ptr FlTypeExp)
  createFlType p = typeExp <$> getTypeInfo (typeRep p)

  -- | Read a value of type @a@ from the given Fl node.
  fromNode :: Node -> IO a
  fromNode = flGetExtObj >=> deRefFlPtr

  -- | Overwrite the given redex with the given value.
  overwriteRedex :: Redex -> a -> IO ()
  overwriteRedex = flMakeRedexExtObj

  -- | Allocate a new node and marshal the given value into it.
  new :: a -> IO Node
  new = flNewExtObj

  -- | Serialize the given value into a ByteString, for disk storage.
  --   Must be the inverse of 'deserialize'.
  serialize :: a -> BS.ByteString
  default serialize :: Show a => a -> BS.ByteString
  serialize = BS.pack . show

  -- | Construct a value from the given ByteString.
  --   Must be the inverse of 'serialize'.
  deserialize :: BS.ByteString -> a
  default deserialize :: Read a => BS.ByteString -> a
  deserialize = read . BS.unpack

  -- | Pretty-print the given value.
  --   This method differs from 'serialize' in that its return value is used
  --   to display values in the Fl REPL, whereas 'serialize'd values are used
  --   to store values on disk. Thus, @pretty@ may take shortcuts to make values
  --   more readable, omit things, etc. that @serialize@ cannot.
  pretty :: a -> BS.ByteString
  default pretty :: Show a => a -> BS.ByteString
  pretty = serialize

  -- | Are the two values equal?
  equals :: a -> a -> Bool
  default equals :: Eq a => a -> a -> Bool
  equals = (==)
