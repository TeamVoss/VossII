{-# LANGUAGE ScopedTypeVariables, TypeApplications, FlexibleInstances #-}
{-# LANGUAGE TupleSections, UndecidableInstances, TypeFamilies #-}
module Language.FL.Plugin.FlExport.Instances where
import Data.Typeable (Typeable, Proxy (..))
import Language.FL.Plugin.API (flExtractArgument, flMakeArrowTy)
import Language.FL.Plugin.FlExport (FlExport (..), Result)
import Language.FL.Plugin.FlType (FlType (..))
import Language.FL.Plugin.Foreign.TypeHooks (mkTypeExport)

instance {-# OVERLAPPING #-} (FlType a, FlExport b) => FlExport (a -> b) where
  callExportFun (arg:args) f = do
    x <- fromNode arg
    callExportFun args $! f x
  callExportFun [] _ = error "unreachable"

  mkFlType _ = do
    argTy <- createFlType (Proxy @a)
    (resultTy, resultArity) <- mkFlType (Proxy @b)
    ty <- flMakeArrowTy argTy resultTy
    return (ty, resultArity + 1)

  gatherArguments _ args redex = do
    (redex', node) <- flExtractArgument redex
    gatherArguments (Proxy @b) (node : args) redex'

  gatherTypes _ =
      ty_a ++ gatherTypes (Proxy @b)
    where
      ty_a | isBaseType (Proxy @a) = []
           | otherwise             = [mkTypeExport (Proxy @a)]

instance {-# OVERLAPPABLE #-} (Typeable a, FlType a, a ~ Result a) => FlExport a where
  callExportFun [] x = pure $! x
  callExportFun _ _ = error "unreachable"

  mkFlType p = (, 0) <$> createFlType p

  gatherArguments _ args _ = pure args

  gatherTypes _
    | isBaseType (Proxy @a) = []
    | otherwise             = [mkTypeExport (Proxy @a)]

instance {-# OVERLAPPING #-} FlType a => FlExport (IO a) where
  callExportFun [] m = do
    x <- m
    pure $! x
  callExportFun _ _ = error "unreachable"

  mkFlType _ = (, 0) <$> createFlType (Proxy @a)

  gatherArguments _ args _ = pure args

  gatherTypes _
    | isBaseType (Proxy @a) = []
    | otherwise             = [mkTypeExport (Proxy @a)]
