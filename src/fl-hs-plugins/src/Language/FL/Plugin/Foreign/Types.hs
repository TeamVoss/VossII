-- | Basic types for low-level interaction with FL.
module Language.FL.Plugin.Foreign.Types where
import Foreign (Ptr)
import Foreign.Concurrent (newForeignPtr)
import Foreign.ForeignPtr (ForeignPtr, withForeignPtr)

data FlTypes
data FlFuns
data GmapInfo
data FILE
data FlTypeExp
data FlInteger
data Formula

newFRedex :: Redex -> IO () -> IO FRedex
newFRedex (Redex r) = fmap FRedex . newForeignPtr r

withFRedex :: FRedex -> (Redex -> IO a) -> IO a
withFRedex (FRedex r) f = withForeignPtr r (f . Redex)

-- | A 'Redex' with an attached finalizer.
newtype FRedex = FRedex (ForeignPtr Redex)
  deriving (Show, Eq)

-- | A reducible Fl expression.
newtype Redex = Redex (Ptr Redex)
  deriving (Show, Eq)

-- | A fully reduced node in the Fl graph.
newtype Node = Node (Ptr Node)
  deriving (Show, Eq)
