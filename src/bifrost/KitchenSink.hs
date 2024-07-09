module KitchenSink (KitchenSink, failure, get, logMessages, logMessagesAndFail, modify, put, runKitchenSink, unfail) where

-- A remarkably expansive monad representing a computation that:
-- - Accumulates messages of some type `m` (like Writer)
-- - Maintains state of some type `s` (like State)
-- - Can produce a result of type `a` or fail to do so (like Maybe)
-- ...because I don't know how to use monad transformers.
--
-- One interesting aspect is that a new state is always produced, even if the
-- computation failed. This would normally have no effect due to the definition
-- of >>=, but by using the `unfail` function one can bring the failed
-- computation back from the dead (as a Maybe), and continue onward with this
-- new state. Whether or not this is useful or desireable is uncertain.

import Control.Monad (when)
import Data.Maybe

newtype KitchenSink m s a = KitchenSink (s -> (Maybe a, [m], s))

instance Functor (KitchenSink m s) where
  fmap f (KitchenSink h) = KitchenSink $ \s ->
    let (mres, ms, s') = h s
    in (fmap f mres, ms, s')

instance Applicative (KitchenSink m s) where
  pure x = KitchenSink $ \s -> (pure x, [], s)
  (KitchenSink h1) <*> (KitchenSink h2) = KitchenSink $ \s ->
    let (mfun, ms1, s1) = h1 s
        (marg, ms2, s2) = h2 s1
    in (mfun <*> marg, ms1 ++ ms2, s2)

instance Monad (KitchenSink m s) where
  return = pure
  (KitchenSink h1) >>= f = KitchenSink $ \s ->
    let (mres1, ms1, s1) = h1 s
    in case mres1 of
      Just x ->
        let (KitchenSink h2) = f x
            (mres2, ms2, s2) = h2 s1
        in (mres2, ms1 ++ ms2, s2)
      Nothing -> (Nothing, ms1, s1)

-- Fail, producing no result.
failure :: KitchenSink m s a
failure = KitchenSink $ \s -> (Nothing, [], s)

-- Get the current state.
get :: KitchenSink m s s
get = KitchenSink $ \s -> (Just s, [], s)

-- Append messages to the log.
logMessages :: [m] -> KitchenSink m s ()
logMessages ms = KitchenSink $ \s -> (Just (), ms, s)

-- Append messages to the log and fail.
logMessagesAndFail :: [m] -> KitchenSink m s a
logMessagesAndFail ms = do
  logMessages ms
  failure

-- Modify the current state.
modify :: (s -> s) -> KitchenSink m s ()
modify f = KitchenSink $ \s -> (Just (), [], f s)

-- Replace the current state.
put :: s -> KitchenSink m s ()
put s = KitchenSink $ \_ -> (Just (), [], s)

-- Turn a computation that might fail into one that will always succeed, but
-- only giving a Maybe result. In the case that the computation failed, the
-- original state is restored.
rollback :: KitchenSink m s a -> KitchenSink m s (Maybe a)
rollback ks = do
  s <- get
  mres <- unfail ks
  when (isNothing mres) $ put s
  return mres

-- Run a computation (given an initial state) and return the result (if
-- successful), the accumulated messages, and the new state.
runKitchenSink :: KitchenSink m s a -> s -> (Maybe a, [m], s)
runKitchenSink (KitchenSink h) = h

-- Turn a computation that might fail into one that will always succeed, but
-- only giving a Maybe result. The state is *not* rolled back on failure.
unfail :: KitchenSink m s a -> KitchenSink m s (Maybe a)
unfail (KitchenSink h) = KitchenSink $ \s ->
  let (mres, errs, s') = h s
  in (Just mres, errs, s')
