{-# LANGUAGE TypeApplications, FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds, KindSignatures, TypeFamilies, DataKinds #-}
{-# LANGUAGE TypeOperators, ScopedTypeVariables, UndecidableInstances #-}
-- | FlType instances for higher order functions.
module Language.FL.Plugin.FlType.FunctionInstances () where
import Data.Kind (Constraint)
import Data.Typeable
import Foreign.Ptr (Ptr)
import GHC.TypeLits
import System.IO.Unsafe
import Language.FL.Plugin.API
import Language.FL.Plugin.FlType
import Language.FL.Plugin.Foreign.Types

type family NonValueTypeError a :: Constraint where
  NonValueTypeError a =
    TypeError
      ('ShowType a ':<>: 'Text " is not a value type." ':$$:
       'Text "Only first or second order functions may be exported to Fl.")

type family Value a :: Constraint where
  Value (a -> b) = NonValueTypeError (a -> b)
  Value (IO a)   = NonValueTypeError (IO a)
  Value _        = ()

class Typeable a => FlFun a where
  mkHsFun :: Redex -> a
  mkFunType :: Proxy a -> IO (Ptr FlTypeExp)

instance {-# OVERLAPPING #-} (Value a, FlType a, FlFun b) => FlFun (a -> b) where
  mkHsFun f x = unsafePerformIO $! do
    x' <- flNode2Redex <$> new x
    f' <- flApplyLambda f x'
    return $! mkHsFun f'
  mkFunType _ = do
    ta <- createFlType (Proxy @a)
    tb <- mkFunType (Proxy @b)
    flMakeArrowTy ta tb

mkHsFunF :: FlFun (a -> b) => FRedex -> a -> b
mkHsFunF f x = unsafePerformIO $ withFRedex f $ \f' -> return $! mkHsFun f' x

instance {-# OVERLAPPABLE #-} (Value a, Typeable a, FlType a) => FlFun a where
  mkHsFun f = unsafePerformIO $! mkHsFun f
  mkFunType _ = createFlType (Proxy @a)

instance {-# OVERLAPPING #-} (Value a, Typeable a, FlType a) => FlFun (IO a) where
  mkHsFun f = do
    x <- flForce f
    flThrowOnError x
    y <- fromNode x
    return $! y
  mkFunType _ = createFlType (Proxy @a)

instance {-# OVERLAPPABLE #-} (Typeable (a -> b), FlFun (a -> b)) =>
                              FlType (a -> b) where
  isBaseType _ = True
  createFlType = mkFunType
  fromNode f = do
    let fn = flNode2Redex f
    flGCProtect fn
    f' <- newFRedex fn (flGCUnprotect fn)
    pure $! mkHsFunF f'
  overwriteRedex _ _ = error "unreachable"
  serialize _ = error "unreachable"
  deserialize _ = error "unreachable"
  pretty _ = error "unreachable"
  equals _ _ = error "unreachable"
  new = error "unreachable"
