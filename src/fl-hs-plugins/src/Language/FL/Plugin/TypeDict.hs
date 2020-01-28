-- | Dictionary of types exported to Fl.
module Language.FL.Plugin.TypeDict
  ( TypeInfo (..)
  , registerTypeInfo, getTypeInfo
  ) where
import Data.IORef
import Data.Map as M
import Data.Typeable (TypeRep)
import Foreign.C (CInt)
import Foreign.Ptr (Ptr)
import System.IO.Unsafe
import Language.FL.Plugin.Foreign.Types (FlTypeExp)

data TypeInfo = TypeInfo
  { typeOid :: CInt
  , typeExp :: Ptr FlTypeExp
  }

{-# NOINLINE typeInfos #-}
typeInfos :: IORef (Map TypeRep TypeInfo)
typeInfos = unsafePerformIO $ newIORef M.empty

-- | Register Fl type information for the given type.
registerTypeInfo :: TypeRep -> TypeInfo -> IO ()
registerTypeInfo rep info = atomicModifyIORef' typeInfos $ \m ->
  (M.insert rep info m, ())

-- | Get Fl type information for the given Haskell type.
--   Explodes if the given type hasn't been previously registered with
--   'registerTypeInfo'.
getTypeInfo :: TypeRep -> IO TypeInfo
getTypeInfo rep = justOrFail . M.lookup rep <$> readIORef typeInfos
  where
    justOrFail (Just x) = x
    justOrFail Nothing  = error $ "type " ++ show rep ++ " not registered"
