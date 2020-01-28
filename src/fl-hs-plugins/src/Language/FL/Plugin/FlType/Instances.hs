{-# LANGUAGE TypeApplications, ScopedTypeVariables, FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts, UndecidableInstances #-}
-- | FlType instances.
module Language.FL.Plugin.FlType.Instances () where
import Control.Monad ((>=>))
import Data.Proxy (Proxy (..))
import Data.Text as Text (Text, pack, unpack)
import Language.FL.Plugin.API
import Language.FL.Plugin.FlType
import Language.FL.Plugin.Foreign.Types (Node)

instance FlType Integer where
  isBaseType _ = True
  createFlType _ = flMakeIntTy
  fromNode = flGetInt
  overwriteRedex = flMakeRedexInt
  new = flNewInt

instance FlType Double where
  isBaseType _ = True
  createFlType _ = flMakeDoubleTy
  fromNode = flGetDouble
  overwriteRedex = flMakeRedexDouble
  new = flNewDouble

instance FlType Bool where
  isBaseType _ = True
  createFlType _ = flMakeBoolTy
  fromNode = flGetBool
  overwriteRedex = flMakeRedexBool
  new = flNewBool

instance FlType Text where
  isBaseType _ = True
  createFlType _ = flMakeStringTy
  fromNode = flGetString
  overwriteRedex = flMakeRedexString
  new = flNewString

instance {-# OVERLAPPING #-} FlType String where
  isBaseType _ = True
  createFlType _ = flMakeStringTy
  fromNode = fmap Text.unpack . fromNode
  overwriteRedex r = overwriteRedex r . Text.pack
  new = new . Text.pack

instance FlType () where
  isBaseType _ = True
  createFlType _ = flMakeVoidTy
  fromNode _ = pure ()
  overwriteRedex r () = flMakeRedexVoid r
  new _ = flNewVoid

instance {-# OVERLAPPABLE #-} FlType a => FlType [a] where
  isBaseType _ = True
  createFlType _ = createFlType (Proxy @a) >>= flMakeListTy
  fromNode = fromList
    where
      getHead = flGetHead >=> fromNode
      getTail = flGetTail >=> fromList
      fromList n = do
        is_nil <- flIsNil n
        if is_nil
          then return []
          else (:) <$> getHead n <*> getTail n
  overwriteRedex r [] = flMakeRedexNil r
  overwriteRedex r (x:xs) = do
    xr <- flNode2Redex <$> new x
    xsr <- flNode2Redex <$> makeList xs
    flMakeRedexCons r xr xsr
  serialize _ = error "unreachable"
  deserialize _ = error "unreachable"
  pretty _ = error "unreachable"
  equals xs ys = and (zipWith equals xs ys)
  new = makeList

makeList :: FlType a => [a] -> IO Node
makeList (a:as) = do
  ar <- flNode2Redex <$> new a
  asr <- flNode2Redex <$> makeList as
  flNewCons ar asr
makeList [] = do
  flNewNil
