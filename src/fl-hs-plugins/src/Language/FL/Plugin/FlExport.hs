{-# LANGUAGE ScopedTypeVariables, TypeApplications, TupleSections #-}
{-# LANGUAGE TypeFamilies, FlexibleContexts #-}
-- | Converting Haskell functions to Fl functions.
module Language.FL.Plugin.FlExport where
import Data.Typeable (Typeable, Proxy (..))
import Foreign.Ptr (Ptr)
import Language.FL.Plugin.Foreign.Types (Node, Redex, FlTypeExp)
import Language.FL.Plugin.FlType (FlType)
import Language.FL.Plugin.Exports.Types (TypeExport)

type family Result a where
  Result (a -> b) = Result b
  Result (IO a)   = a
  Result a        = a

-- | Any function that can be exported by an Fl plugin.
class (Typeable a, FlType (Result a)) => FlExport a where
  callExportFun :: [Node] -> a -> IO (Result a)
  mkFlType :: Proxy a -> IO (Ptr FlTypeExp, Int)
  gatherArguments :: Proxy a -> [Node] -> Redex -> IO [Node]
  gatherTypes :: Proxy a -> [TypeExport]
