module Compile (Compile, mkCompile, runCompile) where

-- A monad for representing stages of the compilation pipeline. Both awkwardly
-- similar to and awkwardly different from KitchenSink. We don't use the monadic
-- aspect in practice.

import Types
import Message
import KitchenSink

newtype Compile a = Compile (Environment -> ([Message], Maybe (Environment, a)))

mkCompile :: (Environment -> ([Message], Maybe (Environment, a))) -> Compile a
mkCompile = Compile

runCompile :: Compile a -> Environment -> ([Message], Maybe (Environment, a))
runCompile (Compile h) env = h env

{-
viaKitchenSink
viaKitchenSink ks mkInitialState getEnv mkResult prog = mkCompile $ \env ->
  let inital = mkInitialState prog env
      (mval, messages, final) = runKitchenSink ks
  in case (mval, filter isError messages) of
    (Just val, []) ->
      let env' = getEnv final
          result = mkResult val final
      in (messages, Just (env', result))
    _ -> (messages, Nothing)
-}

instance Functor Compile where
  fmap f (Compile p) = Compile $ \env -> fmap (fmap (fmap f)) (p env)

instance Applicative Compile where
  pure x = Compile $ \env -> ([], Just (env, x))
  (Compile cf) <*> (Compile cx) = Compile $ \env ->
    let (ms1, res1) = cf env
    in case res1 of
      Just (env1, f) ->
        let (ms2, res2) = cx env1
        in case res2 of
          Just (env2, x) -> (ms1 ++ ms2, Just (env2, f x))
          Nothing -> (ms1 ++ ms2, Nothing)
      Nothing -> (ms1, Nothing)

instance Monad Compile where
  return = pure
  (Compile c) >>= f = Compile $ \env ->
    let (ms1, res1) = c env
    in case res1 of
      Just (env1, x) ->
        let (Compile c') = f x
            (ms2, res2) = c' env1
        in (ms1 ++ ms2, res2)
      Nothing -> (ms1, Nothing)
