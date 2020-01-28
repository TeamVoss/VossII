-- | Pointers that don't get mangled by Fl.
module Language.FL.Plugin.FlPtr
  ( FlPtr (..)
  , deRefFlPtr, newFlPtr
  , markFlPtr, sweepFlPtrs
  ) where
import Control.Concurrent.MVar
import Control.Monad (foldM)
import Data.Bits
import Data.IORef
import Foreign.StablePtr
import Foreign.Ptr
import System.IO.Unsafe

-- | A pointer to an object exported to Fl.
--   Fl pointers must *never* be retained by Haskell code, as they may be
--   freed by Fl's garbage collection. Retain the underlying object obtained
--   by dereferencing the pointer instead.
newtype FlPtr = FlPtr IntPtr
  deriving (Show, Eq, Ord)

data ExtObj a = ExtObj
  { value :: !a
  , flagRef :: !(IORef Bool)
  }

{-# NOINLINE flHeap #-}
flHeap :: MVar [FlPtr]
flHeap = unsafePerformIO $ newMVar []

-- | Number of low bits used by Fl for memory management.
flAlign :: Int
flAlign = 2

-- | Get the stable pointer associated with the given pointer.
flPtrToStablePtr :: FlPtr -> StablePtr a
flPtrToStablePtr (FlPtr ptr) =
  castPtrToStablePtr (intPtrToPtr (ptr `shiftR` flAlign))

flPtrToExtObj :: FlPtr -> IO (ExtObj a)
flPtrToExtObj = deRefStablePtr . flPtrToStablePtr

deRefFlPtr :: FlPtr -> IO a
deRefFlPtr = fmap value . flPtrToExtObj

-- | Create a new pointer to an Fl external object and add it to the heap of
--   such objects.
newFlPtr :: a -> IO FlPtr
newFlPtr x = do
  ref <- newIORef False
  ptr <- ptrToIntPtr . castStablePtrToPtr <$> newStablePtr (ExtObj x ref)
  let flPtr = FlPtr (ptr `shiftL` flAlign)
  modifyMVarMasked_ flHeap (pure . (flPtr :))
  return flPtr

-- | Mark the given pointer as reachable.
markFlPtr :: FlPtr -> IO ()
markFlPtr ptr = do
  ref <- flagRef <$> flPtrToExtObj ptr
  writeIORef ref True

-- | Free all pointers that haven't been explicitly marked using 'markFlPtr',
--   then unmark all remaining pointers.
sweepFlPtrs :: IO ()
sweepFlPtrs = modifyMVarMasked_ flHeap (foldM collect [])
  where
    collect keep ptr = do
      let stablePtr = flPtrToStablePtr ptr
      ref <- flagRef <$> deRefStablePtr stablePtr
      reachable <- readIORef ref
      if reachable
        then writeIORef ref False >> return (ptr : keep)
        else freeStablePtr stablePtr >> return keep
