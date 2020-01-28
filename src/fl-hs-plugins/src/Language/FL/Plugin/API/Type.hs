-- | Type for the FL API.
module Language.FL.Plugin.API.Type where
import Foreign.Ptr (Ptr)
import Foreign.C (CInt, CLong, CString)
import Language.FL.Plugin.FlPtr (FlPtr)
import Language.FL.Plugin.Foreign.Types

-- | Data structure representing the Fl plugin API.
data FlApi = FlApi
  { -- Type/fun registration
    apiRegisterTypes :: CInt -> Ptr FlTypes -> IO CInt
  , apiRegisterFuns :: CInt -> Ptr FlFuns -> IO CInt

    -- Type language
  , apiMakeArrow :: Ptr FlTypeExp -> Ptr FlTypeExp -> IO (Ptr FlTypeExp)
  , apiMakeTuple :: Ptr FlTypeExp -> Ptr FlTypeExp -> IO (Ptr FlTypeExp)
  , apiMakeList :: Ptr FlTypeExp -> IO (Ptr FlTypeExp)
  , apiMakeVoid :: IO (Ptr FlTypeExp)
  , apiMakeBool :: IO (Ptr FlTypeExp)
  , apiMakeInt :: IO (Ptr FlTypeExp)
  , apiMakeDouble :: IO (Ptr FlTypeExp)
  , apiMakeString :: IO (Ptr FlTypeExp)

    -- Constants
  , apiOne :: Ptr Formula
  , apiZero :: Ptr Formula

    -- Working with arguments
  , apiGetApplyLeft :: Ptr Redex -> IO (Ptr Redex)
  , apiGetApplyRight :: Ptr Redex -> IO (Ptr Node)
  , apiGetInt :: Ptr Node -> IO (Ptr FlInteger)
  , apiGetDouble :: Ptr Node -> IO Double
  , apiGetString :: Ptr Node -> IO CString
  , apiGetBool :: Ptr Node -> IO (Ptr Formula)
  , apiIsNil :: Ptr Node -> IO Bool
  , apiGetHead :: Ptr Node -> IO (Ptr Node)
  , apiGetTail :: Ptr Node -> IO (Ptr Node)
  , apiGetExtObj :: Ptr Node -> IO FlPtr

    -- Returning values
  , apiMakeRedexSmallInt :: Ptr Redex -> CLong -> IO ()
  , apiMakeRedexLargeInt :: Ptr Redex -> CString -> IO ()
  , apiMakeRedexDouble :: Ptr Redex -> Double -> IO ()
  , apiMakeRedexString :: Ptr Redex -> CString -> IO ()
  , apiMakeRedexBool :: Ptr Redex -> Ptr Formula -> IO ()
  , apiMakeRedexExtObj :: Ptr Redex -> CInt -> FlPtr -> IO ()
  , apiMakeRedexFail :: Bool -> Ptr Redex -> CString -> IO ()
  , apiMakeRedexVoid :: Ptr Redex -> IO ()
  , apiMakeRedexNil :: Ptr Redex -> IO ()
  , apiMakeRedexCons :: Ptr Redex -> Ptr Redex -> Ptr Redex -> IO ()

    -- Allocating new values
  , apiNewCons :: Ptr Redex -> Ptr Redex -> IO (Ptr Node)
  , apiNewNil :: IO (Ptr Node)
  , apiNewSmallInt :: CLong -> IO (Ptr Node)
  , apiNewLargeInt :: CString -> IO (Ptr Node)
  , apiNewDouble :: Double -> IO (Ptr Node)
  , apiNewBool :: Ptr Formula -> IO (Ptr Node)
  , apiNewString :: CString -> IO (Ptr Node)
  , apiNewExtObj :: CInt -> FlPtr -> IO (Ptr Node)
  , apiNewVoid :: IO (Ptr Node)
  , apiApplyLambda :: Ptr Redex -> Ptr Redex -> IO (Ptr Redex)

    -- Evaluation control and memory management
  , apiDecRefCount :: Ptr Redex -> IO ()
  , apiIncRefCount :: Ptr Redex -> IO ()
  , apiWastrSave :: CString -> IO CString
  , apiDie :: CString -> IO ()
  , apiForce :: Ptr Redex -> IO (Ptr Node)
  , apiGCProtect :: Ptr Redex -> IO ()
  , apiGCUnprotect :: Ptr Redex -> IO ()
  , apiPrintNodeType :: Ptr Redex -> IO ()
  , apiIsError :: Ptr Node -> IO Bool
  , apiGetError :: Ptr Node -> IO CString
  }
