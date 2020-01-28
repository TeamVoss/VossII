{-# LANGUAGE ForeignFunctionInterface, OverloadedStrings #-}
{-# LANGUAGE TypeApplications, ScopedTypeVariables #-}
-- | Low-level omplementation of the hooks required for custom Fl types.
module Language.FL.Plugin.Foreign.TypeHooks (mkTypeExport) where
import qualified Data.ByteString.Char8 as BS
import Data.Char (isAlphaNum)
import Data.Typeable (Proxy (..), typeRep)
import Foreign.C (CInt (..), CString)
import Foreign.Marshal (alloca, allocaBytes)
import Foreign.Storable (peek)
import Foreign.Ptr (Ptr)
import Language.FL.Plugin.API (flDie, flWastrSave, flOne, flZero)
import Language.FL.Plugin.FlPtr (newFlPtr, deRefFlPtr, markFlPtr, sweepFlPtrs)
import Language.FL.Plugin.FlType (FlType, deserialize, serialize, pretty)
import qualified Language.FL.Plugin.FlType as FlType (equals)
import Language.FL.Plugin.Exports.Types (TypeExport (..))
import Language.FL.Plugin.Foreign.Types (FILE)

foreign import ccall "fprintf"
  fprintf :: Ptr FILE -> CString -> IO ()

foreign import ccall "fgets"
  fgets :: CString -> CInt -> Ptr FILE -> IO ()

foreign import ccall "fscanf"
  fscanf_cint :: Ptr FILE -> CString -> Ptr CInt -> IO ()

mkTypeExport :: forall a. FlType a => Proxy a -> TypeExport
mkTypeExport p = TypeExport
  { exportTypeName = mangle (show (typeRep p))
  , exportTypeRep = typeRep p
  , mark = markFlPtr
  , sweep = sweepFlPtrs
  , equals = \pa pb -> do
      x <- deRefFlPtr @a pa
      y <- deRefFlPtr @a pb
      if x `FlType.equals` y then flOne else flZero
  , obj2string = \px -> do
      x <- deRefFlPtr @a px
      BS.useAsCString (pretty x) flWastrSave
  , save = \fp ptr -> do
      x <- deRefFlPtr @a ptr
      let val = serialize x
          len = BS.pack (show (BS.length str))
          str = BS.concat [len, " ", val, "\n"]
      BS.useAsCString str (fprintf fp)
  , load = \fp -> do
      len <- alloca $ \lenPtr -> do
        BS.useAsCString "%d " $ \fmt -> fscanf_cint fp fmt lenPtr
        peek lenPtr
      -- +2 bytes for newline + null terminator
      str <- allocaBytes (fromIntegral len+2) $ \cstr -> do
        fgets cstr (len+2) fp
        BS.packCString cstr
      newFlPtr (deserialize @a str)
  , gmap = \_ x -> pure x
  , gmap2 = \_ _ _ -> flDie "gmap2 not supported for custom Haskell types"
  }
  where
    mangle s = "hs_" ++ map (\c -> if isAlphaNum c then c else '_') s
