{-# LANGUAGE TypeApplications #-}
-- | API available to FL plugins.
--   Not type safe, so should never be exported to the user.
module Language.FL.Plugin.API where
import Control.Exception (Exception (..), throwIO)
import Control.Monad (when, (<=<))
import Data.Bits
import Data.ByteString.Char8 as ByteString (packCString, pack, useAsCString)
import Data.Int
import Data.Word
import Data.IORef
import Data.Text as Text (Text, unpack)
import Data.Text.Encoding (decodeLatin1)
import Data.Typeable (Typeable, typeOf)
import Foreign.C (CLong)
import Foreign.C.String (CString, withCString, peekCString)
import Foreign.Ptr
import Foreign.Storable (peek)
import System.IO.Unsafe
import Language.FL.Plugin.API.Type
import Language.FL.Plugin.Exports.Types
import Language.FL.Plugin.FlPtr
import Language.FL.Plugin.Foreign.Types
import Language.FL.Plugin.TypeDict

-- | An error thrown by an Fl function.
newtype FlError = FlError String

instance Show FlError where
  show (FlError e) = e

instance Exception FlError

-- | Perform the given action over the FL API.
withAPI :: (FlApi -> IO a) -> IO a
withAPI f = f =<< getFlApi

-- | Set the global API and plugin info.
--
--   This function must be called before using any other function from
--   this module.
activateAPI :: FlApi -> Plugin -> IO ()
activateAPI api plugin = writeIORef pluginState (api, plugin)

-- | Get all functions exported by the plugin.
getPluginExports :: IO [Export]
getPluginExports = pluginExports . snd <$> readIORef pluginState

-- | Get a handle to the Fl API.
getFlApi :: IO FlApi
getFlApi = fst <$> readIORef pluginState

{-# NOINLINE pluginState #-}
pluginState :: IORef (FlApi, Plugin)
pluginState = unsafePerformIO $ newIORef (undefined, undefined)

-- * Constants

flOne, flZero :: IO (Ptr Formula)
flOne = withAPI (pure . apiOne)
flZero = withAPI (pure . apiZero)


-- * Type language

flMakeIntTy, flMakeBoolTy, flMakeStringTy, flMakeVoidTy :: IO (Ptr FlTypeExp)
flMakeDoubleTy :: IO (Ptr FlTypeExp)
flMakeIntTy = withAPI apiMakeInt
flMakeDoubleTy = withAPI apiMakeDouble
flMakeBoolTy = withAPI apiMakeBool
flMakeStringTy = withAPI apiMakeString
flMakeVoidTy = withAPI apiMakeVoid

flMakeListTy :: Ptr FlTypeExp -> IO (Ptr FlTypeExp)
flMakeListTy t = withAPI $ \api -> apiMakeList api t

flMakeArrowTy :: Ptr FlTypeExp -> Ptr FlTypeExp -> IO (Ptr FlTypeExp)
flMakeArrowTy a b = withAPI $ \api -> apiMakeArrow api a b

flMakeTupleTy :: Ptr FlTypeExp -> Ptr FlTypeExp -> IO (Ptr FlTypeExp)
flMakeTupleTy a b = withAPI $ \api -> apiMakeTuple api a b


-- * Working with arguments

-- | Split the given application node into an argument and an application node.
--   Will explode if the given redex is not an application node.
flExtractArgument :: Redex -> IO (Redex, Node)
flExtractArgument (Redex redex) = withAPI $ \api -> do
  app <- apiGetApplyLeft api redex
  arg <- apiGetApplyRight api redex
  return (Redex app, Node arg)

flGetInt :: Node -> IO Integer
flGetInt (Node n) = withAPI $ \api -> do
    ptr <- apiGetInt api n
    len <- peek (castPtr (ptr `plusPtr` (-2)))
    sumBlocks len (castPtr ptr)
  where
    sumBlocks :: Word16 -> Ptr Word64 -> IO Integer
    sumBlocks 1 p = fromIntegral <$> peek @Int64 (castPtr p)
    sumBlocks i p = do
      x <- fromIntegral <$> peek p
      y <- sumBlocks (i-1) (p `plusPtr` 8)
      return (x + fromIntegral y `shiftL` 64)

flGetDouble :: Node -> IO Double
flGetDouble (Node n) = withAPI $ \api -> do
  apiGetDouble api n

flGetString :: Node -> IO Text
flGetString (Node n) = withAPI $ \api -> do
  cstr <- apiGetString api n
  decodeLatin1 <$> packCString cstr

flGetBool :: Node -> IO Bool
flGetBool (Node n) = withAPI $ \api -> do
  b <- apiGetBool api n
  return $ case () of
    _ | b == apiOne api  -> True
      | b == apiZero api -> False
      | otherwise        -> error "can only pass true/false formulas to plugins"

flIsNil :: Node -> IO Bool
flIsNil (Node n) = withAPI $ \api -> apiIsNil api n

flGetHead :: Node -> IO Node
flGetHead (Node n) = withAPI $ \api -> Node <$> apiGetHead api n

flGetTail :: Node -> IO Node
flGetTail (Node n) = withAPI $ \api -> Node <$> apiGetTail api n

flGetExtObj :: Node -> IO FlPtr
flGetExtObj (Node n) = withAPI $ \api -> apiGetExtObj api n


-- * Returning values

flWastrSave :: CString -> IO CString
flWastrSave s = withAPI $ \api -> apiWastrSave api s

flMakeRedexInt :: Redex -> Integer -> IO ()
flMakeRedexInt (Redex r) x = withAPI $ \api -> do
    -- TODO: build large aint_T directly instead of via strings
    if x >= min_long && x <= max_long
      then apiMakeRedexSmallInt api r (fromIntegral x)
      else withCString (show x) (apiMakeRedexLargeInt api r)
  where
    min_long = fromIntegral (minBound @CLong) :: Integer
    max_long = fromIntegral (maxBound @CLong) :: Integer

flMakeRedexDouble :: Redex -> Double -> IO ()
flMakeRedexDouble (Redex r) d = withAPI $ \api -> do
  apiMakeRedexDouble api r d

flMakeRedexString :: Redex -> Text -> IO ()
flMakeRedexString (Redex r) s = withAPI $ \api -> do
  useAsCString (ByteString.pack $ Text.unpack s)
               (apiMakeRedexString api r <=< apiWastrSave api)

flMakeRedexBool :: Redex -> Bool -> IO ()
flMakeRedexBool (Redex r) x = withAPI $ \api -> do
  let x' = if x then apiOne api else apiZero api
  apiMakeRedexBool api r x'

flMakeRedexExtObj :: Typeable a => Redex -> a -> IO ()
flMakeRedexExtObj (Redex r) x = withAPI $ \api -> do
  ptr <- newFlPtr x
  oid <- typeOid <$> getTypeInfo (typeOf x)
  apiMakeRedexExtObj api r oid ptr

flMakeRedexFail :: Bool -> Redex -> String -> IO ()
flMakeRedexFail trace (Redex r) s = withAPI $ \api -> do
  withCString s (apiMakeRedexFail api trace r <=< apiWastrSave api)

flMakeRedexVoid :: Redex -> IO ()
flMakeRedexVoid (Redex r) = withAPI $ \api -> apiMakeRedexVoid api r

flMakeRedexCons :: Redex -> Redex -> Redex -> IO ()
flMakeRedexCons (Redex r) (Redex x) (Redex xs) =
  withAPI $ \api -> apiMakeRedexCons api r x xs

flMakeRedexNil :: Redex -> IO ()
flMakeRedexNil (Redex r) = withAPI $ \api -> apiMakeRedexNil api r


-- * Allocating new values

flApplyLambda :: Redex -> Redex -> IO Redex
flApplyLambda (Redex f) (Redex x) = withAPI $ \api -> Redex <$> apiApplyLambda api f x

flNewNil :: IO Node
flNewNil = Node <$> withAPI apiNewNil

flNewCons :: Redex -> Redex -> IO Node
flNewCons (Redex x) (Redex xs) = withAPI $ \api -> Node <$> apiNewCons api x xs

flNewExtObj :: Typeable a => a -> IO Node
flNewExtObj x = withAPI $ \api -> do
  ptr <- newFlPtr x
  oid <- typeOid <$> getTypeInfo (typeOf x)
  Node <$> apiNewExtObj api oid ptr

flNewInt :: Integer -> IO Node
flNewInt x = fmap Node . withAPI $ \api -> do
    -- TODO: build large aint_T directly instead of via strings
    if x >= min_long && x <= max_long
      then apiNewSmallInt api (fromIntegral x)
      else withCString (show x) (apiNewLargeInt api)
  where
    min_long = fromIntegral (minBound @CLong) :: Integer
    max_long = fromIntegral (maxBound @CLong) :: Integer

flNewDouble :: Double -> IO Node
flNewDouble x = withAPI $ \api -> Node <$> apiNewDouble api x

flNewVoid :: IO Node
flNewVoid = Node <$> withAPI apiNewVoid

flNewString :: Text -> IO Node
flNewString s = withAPI $ \api -> do
  Node <$> useAsCString (ByteString.pack $ Text.unpack s)
                        (apiNewString api <=< apiWastrSave api)

flNewBool :: Bool -> IO Node
flNewBool x = withAPI $ \api -> do
  let x' = if x then apiOne api else apiZero api
  Node <$> apiNewBool api x'


-- * Memory management and evaluation control

flDecrementRedexRefCount :: Redex -> IO ()
flDecrementRedexRefCount (Redex r) = withAPI $ \api -> apiDecRefCount api r

flIncrementRedexRefCount :: Redex -> IO ()
flIncrementRedexRefCount (Redex r) = withAPI $ \api -> apiIncRefCount api r

flDie :: String -> IO a
flDie s = withAPI $ \api -> do
  withCString s (apiDie api <=< flWastrSave)
  return (error "die")

flForce :: Redex -> IO Node
flForce (Redex r) = withAPI $ \api -> Node <$> apiForce api r

flNode2Redex :: Node -> Redex
flNode2Redex (Node n) = Redex (castPtr n)

flGCProtect :: Redex -> IO ()
flGCProtect (Redex r) = withAPI $ \api -> apiGCProtect api r

flGCUnprotect :: Redex -> IO ()
flGCUnprotect (Redex r) = withAPI $ \api -> apiGCUnprotect api r

flPrintNodeType :: Redex -> IO ()
flPrintNodeType (Redex r) = withAPI $ \api -> apiPrintNodeType api r

flThrowOnError :: Node -> IO ()
flThrowOnError (Node n) = withAPI $ \api -> do
  is_fail <- apiIsError api n
  when is_fail $ do
    msg <- peekCString =<< apiGetError api n
    throwIO (FlError msg)
