{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TypeApplications, ScopedTypeVariables #-}
-- | Explicitly exporting functions to Fl.
module Language.FL.Plugin.Exports where
import Control.Exception (SomeException (..), Handler (..), catches)
import Data.Typeable (Proxy (..))
import Foreign.Ptr (FunPtr)
import System.IO.Unsafe
import Language.FL.Plugin.API
import Language.FL.Plugin.Exports.Types
import Language.FL.Plugin.FlExport
import Language.FL.Plugin.FlType (overwriteRedex)
import Language.FL.Plugin.Foreign.Types (Redex (..))

foreign import ccall "wrapper"
  mkExportFun :: (Redex -> IO ()) -> IO (FunPtr (Redex -> IO ()))

-- | Exports the given function to fl under the given name.
export :: forall a. FlExport a => String -> a -> Export
export name f = unsafePerformIO $ do
  fptr <- mkExportFun $ \redex -> do
    args <- gatherArguments (Proxy @a) [] redex
    (overwriteRedex redex =<< callExportFun args f) `catches`
      [ Handler $ \(FlError e) -> flMakeRedexFail False redex e
      , Handler $ \(SomeException e) -> flMakeRedexFail True redex (show e)
      ]
  return Export
    { funName = name
      -- Memory leak, but this memory should never be freed anyway, so...
    , funPtr = fptr
    , mkTypeAndArity = mkFlType (Proxy @a)
    , funCustomTypes = gatherTypes (Proxy @a)
    , fixity = NoFixity
    }
