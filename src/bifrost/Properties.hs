module Properties (isInstant) where

{-
  Important properties that we want to be interpreted consistently.
-}

import Types

-- Is the result of this available immediately?
-- Precondition: protocol is Protocol, not ProtocolAuto
isInstant :: Protocol -> Bool
isInstant proto =
  case proto of
    ProtocolAuto -> error "cannot apply isInstant to ProtocolAuto"
    _ -> case (proto_power proto, proto_data proto) of
      (AlwaysOn, Immediate _) -> True
      _ -> False
