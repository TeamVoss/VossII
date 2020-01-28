{-# LANGUAGE ForeignFunctionInterface, TypeApplications #-}
-- | Low-level functionality for interacting with the plugin API exposed by Fl.
module Language.FL.Plugin.Foreign (FlInit, FlApi, withAPI, flInit) where
import Control.Monad (foldM_, when, void)
import Data.Function (on)
import Data.Int
import Data.List (genericLength, groupBy, sortBy)
import Data.Typeable
import Foreign.C (CInt (..), CLong (..))
import Foreign.C.String (CString, newCString)
import Foreign.Marshal.Alloc (mallocBytes)
import Foreign.Ptr
import Foreign.Storable (peekByteOff, pokeByteOff)
import Language.FL.Plugin.API
import Language.FL.Plugin.Exports.Types
import Language.FL.Plugin.FlPtr
import Language.FL.Plugin.Foreign.Types
import Language.FL.Plugin.TypeDict
import Language.FL.Plugin.API.Type

#include "plugin.h"

foreign import ccall "dynamic"
  mkRegisterPlugin :: FunPtr (Ptr Plugin -> IO (Ptr FlApi))
                   -> Ptr Plugin -> IO (Ptr FlApi)

foreign import ccall "wrapper"
  mkEqualsExport :: (FlPtr -> FlPtr -> IO (Ptr Formula))
                 -> IO (FunPtr (FlPtr -> FlPtr -> IO (Ptr Formula)))

foreign import ccall "wrapper"
  mkLoadExport :: (Ptr FILE -> IO FlPtr) -> IO (FunPtr (Ptr FILE -> IO FlPtr))

foreign import ccall "wrapper"
  mkObj2StrExport :: (FlPtr -> IO CString) -> IO (FunPtr (FlPtr -> IO CString))

foreign import ccall "wrapper"
  mkMarkExport :: (FlPtr -> IO ()) -> IO (FunPtr (FlPtr -> IO ()))

foreign import ccall "wrapper"
  mkSweepExport :: (IO ()) -> IO (FunPtr (IO ()))

foreign import ccall "wrapper"
  mkSaveExport :: (Ptr FILE -> FlPtr -> IO ())
               -> IO (FunPtr (Ptr FILE -> FlPtr -> IO ()))

foreign import ccall "wrapper"
  mkGmapExport :: (Ptr GmapInfo -> FlPtr -> IO FlPtr)
               -> IO (FunPtr (Ptr GmapInfo -> FlPtr -> IO FlPtr))

foreign import ccall "wrapper"
  mkGmap2Export :: (Ptr GmapInfo -> FlPtr -> FlPtr -> IO FlPtr)
                -> IO (FunPtr (Ptr GmapInfo -> FlPtr -> FlPtr -> IO FlPtr))

-- | Type of the @fl_init@ export expected from all Fl plugins.
type FlInit = FunPtr (Ptr Plugin -> IO (Ptr FlApi)) -> IO ()

-- | Initialize the plugin, using the given computation to create the plugin
--   data to be exported to Fl.
flInit :: IO Plugin -> FlInit
flInit mkPlugin registerPluginPtr = do
    plugin <- mkPlugin
    pluginPtr <- mallocBytes #{size fl_plugin_rec}
    writePlugin plugin pluginPtr
    apiPtr <- mkRegisterPlugin registerPluginPtr pluginPtr
    when (apiPtr /= nullPtr) $ do
      api <- readApi apiPtr
      activateAPI api plugin
      (typeReps, types) <- flTypes plugin
      result <- apiRegisterTypes api (genericLength typeReps) types
      when (result == 0) $ do
        foldM_ registerType types typeReps
        void $ uncurry (apiRegisterFuns api) =<< flFuns
  where
    registerType ptr rep = do
      ty <- #{peek fl_plugin_type_rec, type} ptr
      oid <- #{peek fl_plugin_type_rec, class} ptr
      registerTypeInfo rep (TypeInfo oid ty)
      return (plusPtr ptr #{size fl_plugin_type_rec})

-- | Produce the list of types to export to Fl.
flTypes :: Plugin -> IO ([TypeRep], Ptr FlTypes)
flTypes plugin = do
    let types = allCustomTypes plugin
    typesPtr <- mallocBytes (#{size fl_plugin_type_rec} * length types)
    foldM_ writeType typesPtr types
    return (map exportTypeRep types, typesPtr)
  where
    writeType ptr ty = do
      writeTypeExport ptr ty
      return (plusPtr ptr #{size fl_plugin_type_rec})
    writeTypeExport ptr ty = do
      name <- flWastrSave =<< newCString (exportTypeName ty)
      markPtr <- mkMarkExport (mark ty)
      sweepPtr <- mkSweepExport (sweep ty)
      equalsPtr <- mkEqualsExport (equals ty)
      obj2stringPtr <- mkObj2StrExport (obj2string ty)
      savePtr <- mkSaveExport (save ty)
      loadPtr <- mkLoadExport (load ty)
      gmapPtr <- mkGmapExport (gmap ty)
      gmap2Ptr <- mkGmap2Export (gmap2 ty)
      #{poke fl_plugin_type_rec, name} ptr name
      #{poke fl_plugin_type_rec, mark} ptr markPtr
      #{poke fl_plugin_type_rec, sweep} ptr sweepPtr
      #{poke fl_plugin_type_rec, save} ptr savePtr
      #{poke fl_plugin_type_rec, load} ptr loadPtr
      #{poke fl_plugin_type_rec, obj2string} ptr obj2stringPtr
      #{poke fl_plugin_type_rec, equals} ptr equalsPtr
      #{poke fl_plugin_type_rec, gmap} ptr gmapPtr
      #{poke fl_plugin_type_rec, gmap2} ptr gmap2Ptr

allCustomTypes :: Plugin -> [TypeExport]
allCustomTypes = nubTypes . concat . map funCustomTypes . pluginExports
  where
    nubTypes = map head
      . groupBy ((==) `on` exportTypeRep)
      . sortBy (compare `on` exportTypeRep)

-- | Produce the list of functions to export to Fl.
flFuns :: IO (CInt, Ptr FlFuns)
flFuns = do
    funs <- getPluginExports
    funsPtr <- mallocBytes (#{size fl_plugin_fun_rec} * length funs)
    foldM_ writeFun funsPtr funs
    return (fromIntegral $ length funs, funsPtr)
  where
    writeFun ptr fun = do
      writeExport fun ptr
      return (plusPtr ptr #{size fl_plugin_fun_rec})


-- * (De)serializing Fl plugin structures

foreign import ccall "dynamic"
  mkRegisterFun :: FunPtr (CInt -> Ptr a -> IO CInt)
                -> CInt -> Ptr a -> IO CInt

foreign import ccall "dynamic"
  mkBinaryTypeFun :: FunPtr (Ptr FlTypeExp -> Ptr FlTypeExp -> IO (Ptr FlTypeExp))
                  -> Ptr FlTypeExp -> Ptr FlTypeExp -> IO (Ptr FlTypeExp)

foreign import ccall "dynamic"
  mkNullaryTypeFun :: FunPtr (IO (Ptr FlTypeExp)) -> IO (Ptr FlTypeExp)

foreign import ccall "dynamic"
  mkNullaryPtrFun :: FunPtr (IO (Ptr a)) -> IO (Ptr a)

foreign import ccall "dynamic"
  mkUnaryPtrFun :: FunPtr (Ptr a -> IO (Ptr b)) -> Ptr a -> IO (Ptr b)

foreign import ccall "dynamic"
  mkUnaryDoubleFun :: FunPtr (Ptr a -> IO Double) -> Ptr a -> IO Double

foreign import ccall "dynamic"
  mkBinaryPtrFun :: FunPtr (Ptr a -> Ptr b -> IO (Ptr c))
                 -> Ptr a -> Ptr b -> IO (Ptr c)

foreign import ccall "dynamic"
  mkGetExtObjFun :: FunPtr (Ptr Node -> IO FlPtr) -> Ptr Node -> IO FlPtr

foreign import ccall "dynamic"
  mkUnaryVoidFun :: FunPtr (Ptr a -> IO ()) -> Ptr a -> IO ()

foreign import ccall "dynamic"
  mkUnaryBoolFun :: FunPtr (Ptr a -> IO Bool) -> Ptr a -> IO Bool

foreign import ccall "dynamic"
  mkTernaryVoidFun :: FunPtr (Ptr a -> Ptr b -> Ptr c -> IO ())
                   -> Ptr a -> Ptr b -> Ptr c -> IO ()

foreign import ccall "dynamic"
  mkFailPtr :: FunPtr (Bool -> Ptr Redex -> Ptr a -> IO ())
            -> Bool -> Ptr Redex -> Ptr a -> IO ()

foreign import ccall "dynamic"
  mkMakeRedexPtr :: FunPtr (Ptr Redex -> Ptr a -> IO ())
                 -> Ptr Redex -> Ptr a -> IO ()

foreign import ccall "dynamic"
  mkMakeRedexDouble :: FunPtr (Ptr Redex -> Double -> IO ())
                    -> Ptr Redex -> Double -> IO ()

foreign import ccall "dynamic"
  mkMakeRedexLongPtr :: FunPtr (Ptr Redex -> CLong -> IO ())
                     -> Ptr Redex -> CLong -> IO ()

foreign import ccall "dynamic"
  mkMakeRedexExtObjPtr :: FunPtr (Ptr Redex -> CInt -> FlPtr -> IO ())
                       -> Ptr Redex -> CInt -> FlPtr -> IO ()

foreign import ccall "dynamic"
  mkNewExtObj :: FunPtr (CInt -> FlPtr -> IO (Ptr Node))
              -> CInt -> FlPtr -> IO (Ptr Node)

foreign import ccall "dynamic"
  mkNewLong :: FunPtr (CLong -> IO (Ptr Node)) -> CLong -> IO (Ptr Node)

foreign import ccall "dynamic"
  mkNewDouble :: FunPtr (Double -> IO (Ptr Node)) -> Double -> IO (Ptr Node)

readApi :: Ptr a -> IO FlApi
readApi ptr = do
  one <- mkNullaryPtrFun . castPtrToFunPtr =<< #{peek fl_plugin_api_rec, one} ptr
  zero <- mkNullaryPtrFun . castPtrToFunPtr =<< #{peek fl_plugin_api_rec, zero} ptr
  FlApi
    -- Registration functions
    <$> ((mkRegisterFun . castPtrToFunPtr) <$> #{peek fl_plugin_api_rec, register_types} ptr)
    <*> ((mkRegisterFun . castPtrToFunPtr) <$> #{peek fl_plugin_api_rec, register_funs} ptr)
    -- Type language
    <*> ((mkBinaryTypeFun . castPtrToFunPtr) <$> #{peek fl_plugin_api_rec, make_arrow} ptr)
    <*> ((mkBinaryTypeFun . castPtrToFunPtr) <$> #{peek fl_plugin_api_rec, make_tuple} ptr)
    <*> ((mkUnaryPtrFun . castPtrToFunPtr) <$> #{peek fl_plugin_api_rec, make_list} ptr)
    <*> ((mkNullaryTypeFun . castPtrToFunPtr) <$> #{peek fl_plugin_api_rec, make_void} ptr)
    <*> ((mkNullaryTypeFun . castPtrToFunPtr) <$> #{peek fl_plugin_api_rec, make_bool} ptr)
    <*> ((mkNullaryTypeFun . castPtrToFunPtr) <$> #{peek fl_plugin_api_rec, make_int} ptr)
    <*> ((mkNullaryTypeFun . castPtrToFunPtr) <$> #{peek fl_plugin_api_rec, make_double} ptr)
    <*> ((mkNullaryTypeFun . castPtrToFunPtr) <$> #{peek fl_plugin_api_rec, make_string} ptr)
    -- Constants
    <*> pure one
    <*> pure zero
    -- Working with arguments
    <*> ((mkUnaryPtrFun . castPtrToFunPtr) <$> #{peek fl_plugin_api_rec, get_apply_left} ptr)
    <*> ((mkUnaryPtrFun . castPtrToFunPtr) <$> #{peek fl_plugin_api_rec, get_apply_right} ptr)
    <*> ((mkUnaryPtrFun . castPtrToFunPtr) <$> #{peek fl_plugin_api_rec, get_int} ptr)
    <*> ((mkUnaryDoubleFun . castPtrToFunPtr) <$> #{peek fl_plugin_api_rec, get_double} ptr)
    <*> ((mkUnaryPtrFun . castPtrToFunPtr) <$> #{peek fl_plugin_api_rec, get_string} ptr)
    <*> ((mkUnaryPtrFun . castPtrToFunPtr) <$> #{peek fl_plugin_api_rec, get_bool} ptr)
    <*> ((mkUnaryBoolFun . castPtrToFunPtr) <$> #{peek fl_plugin_api_rec, is_nil} ptr)
    <*> ((mkUnaryPtrFun . castPtrToFunPtr) <$> #{peek fl_plugin_api_rec, get_head} ptr)
    <*> ((mkUnaryPtrFun . castPtrToFunPtr) <$> #{peek fl_plugin_api_rec, get_tail} ptr)
    <*> ((mkGetExtObjFun . castPtrToFunPtr) <$> #{peek fl_plugin_api_rec, get_ext_obj} ptr)
    -- Return values
    <*> ((mkMakeRedexLongPtr . castPtrToFunPtr) <$> #{peek fl_plugin_api_rec, make_redex_small_int} ptr)
    <*> ((mkMakeRedexPtr . castPtrToFunPtr) <$> #{peek fl_plugin_api_rec, make_redex_large_int} ptr)
    <*> ((mkMakeRedexDouble . castPtrToFunPtr) <$> #{peek fl_plugin_api_rec, make_redex_double} ptr)
    <*> ((mkMakeRedexPtr . castPtrToFunPtr) <$> #{peek fl_plugin_api_rec, make_redex_string} ptr)
    <*> ((mkMakeRedexPtr . castPtrToFunPtr) <$> #{peek fl_plugin_api_rec, make_redex_bool} ptr)
    <*> ((mkMakeRedexExtObjPtr . castPtrToFunPtr) <$> #{peek fl_plugin_api_rec, make_redex_ext_obj} ptr)
    <*> ((mkFailPtr . castPtrToFunPtr) <$> #{peek fl_plugin_api_rec, fail} ptr)
    <*> ((mkUnaryVoidFun . castPtrToFunPtr) <$> #{peek fl_plugin_api_rec, make_redex_void} ptr)
    <*> ((mkUnaryVoidFun . castPtrToFunPtr) <$> #{peek fl_plugin_api_rec, make_redex_nil} ptr)
    <*> ((mkTernaryVoidFun . castPtrToFunPtr) <$> #{peek fl_plugin_api_rec, make_redex_cons} ptr)
    -- Allocating new values
    <*> ((mkBinaryPtrFun . castPtrToFunPtr) <$> #{peek fl_plugin_api_rec, new_cons} ptr)
    <*> ((mkNullaryPtrFun . castPtrToFunPtr) <$> #{peek fl_plugin_api_rec, new_nil} ptr)
    <*> ((mkNewLong . castPtrToFunPtr) <$> #{peek fl_plugin_api_rec, new_small_int} ptr)
    <*> ((mkUnaryPtrFun . castPtrToFunPtr) <$> #{peek fl_plugin_api_rec, new_large_int} ptr)
    <*> ((mkNewDouble . castPtrToFunPtr) <$> #{peek fl_plugin_api_rec, new_double} ptr)
    <*> ((mkUnaryPtrFun . castPtrToFunPtr) <$> #{peek fl_plugin_api_rec, new_bool} ptr)
    <*> ((mkUnaryPtrFun . castPtrToFunPtr) <$> #{peek fl_plugin_api_rec, new_string} ptr)
    <*> ((mkNewExtObj . castPtrToFunPtr) <$> #{peek fl_plugin_api_rec, new_ext_obj} ptr)
    <*> ((mkNullaryPtrFun . castPtrToFunPtr) <$> #{peek fl_plugin_api_rec, new_void} ptr)
    <*> ((mkBinaryPtrFun . castPtrToFunPtr) <$> #{peek fl_plugin_api_rec, make_apply} ptr)
    -- Evaluation & memory
    <*> ((mkUnaryVoidFun . castPtrToFunPtr) <$> #{peek fl_plugin_api_rec, dec_ref_cnt} ptr)
    <*> ((mkUnaryVoidFun . castPtrToFunPtr) <$> #{peek fl_plugin_api_rec, inc_ref_cnt} ptr)
    <*> ((mkUnaryPtrFun . castPtrToFunPtr) <$> #{peek fl_plugin_api_rec, wastrsave} ptr)
    <*> ((mkUnaryVoidFun . castPtrToFunPtr) <$> #{peek fl_plugin_api_rec, die} ptr)
    <*> ((mkUnaryPtrFun . castPtrToFunPtr) <$> #{peek fl_plugin_api_rec, force} ptr)
    <*> ((mkUnaryVoidFun . castPtrToFunPtr) <$> #{peek fl_plugin_api_rec, gc_protect} ptr)
    <*> ((mkUnaryVoidFun . castPtrToFunPtr) <$> #{peek fl_plugin_api_rec, gc_unprotect} ptr)
    <*> ((mkUnaryVoidFun . castPtrToFunPtr) <$> #{peek fl_plugin_api_rec, print_node_type} ptr)
    <*> ((mkUnaryBoolFun . castPtrToFunPtr) <$> #{peek fl_plugin_api_rec, is_fail} ptr)
    <*> ((mkUnaryPtrFun . castPtrToFunPtr) <$> #{peek fl_plugin_api_rec, get_fail_string} ptr)

-- | Write a plugin record to the given pointer.
writePlugin :: Plugin -> Ptr a -> IO ()
writePlugin p ptr = do
  -- Leaks memory, but this memory should never be freed anyway, so...
  abi <- newCString #{const_str FL_ABI_VERSION}
  name <- newCString (pluginName p)

  #{poke fl_plugin_rec, name} ptr name
  #{poke fl_plugin_rec, version} ptr (fromIntegral (pluginVersion p) :: Int32)
  #{poke fl_plugin_rec, abi_version} ptr abi
  #{poke fl_plugin_rec, num_funs} ptr (genericLength (pluginExports p) :: Int32)

  let numTypes = genericLength $ allCustomTypes p
  #{poke fl_plugin_rec, num_types} ptr (numTypes :: Int32)

-- | Write a single export to the given pointer.
writeExport :: Export -> Ptr a -> IO ()
writeExport e ptr = do
    (ty, arity) <- mkTypeAndArity e

    -- Leaks memory, but this memory should never be freed anyway, so...
    name <- newCString (funName e)
    strictness <- newCString (replicate arity '1')

    #{poke fl_plugin_fun_rec, name} ptr name
    #{poke fl_plugin_fun_rec, strictness} ptr strictness
    #{poke fl_plugin_fun_rec, non_lazy} ptr True -- TODO: does this even matter?
    #{poke fl_plugin_fun_rec, fixity} ptr (fx :: Int32)
    #{poke fl_plugin_fun_rec, precedence} ptr (fromIntegral precedence :: Int32)
    #{poke fl_plugin_fun_rec, type} ptr ty
    #{poke fl_plugin_fun_rec, fun_ptr} ptr (funPtr e)
  where
    (fx, precedence) =
      case fixity e of
        NoFixity     -> (#{const NO_FIXITY}, 0)
        Prefix prec  -> (#{const PREFIX}, max 0 (min 1 prec))
        Infix L prec -> (#{const INFIXL}, max 0 (min 9 prec))
        Infix R prec -> (#{const INFIXR}, max 0 (min 9 prec))
        Postfix      -> (#{const POSTFIX}, 0)
