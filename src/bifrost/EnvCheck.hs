module EnvCheck (envCheck) where

-- Module that checks the sanity of an Environment.
-- Not complete, but also *in theory* not neccessary.

import Types
import Message
import Environment
import Expression
import Compile
import KitchenSink

envCheck :: a -> Compile a
envCheck prog = mkCompile $ \env ->
  let (mres, messages, _) = runKitchenSink envCheck' env
      errors = filter isError messages
  in (messages, case (errors, mres) of
         ([], Just ()) -> Just (env, prog)
         _ -> Nothing)

type EC = KitchenSink Message Environment

envCheck' :: EC ()
envCheck' = do
  env <- get
  undefined  -- TODO
