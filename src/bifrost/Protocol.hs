module Protocol (protocolFromExp) where

import qualified Data.Map as M
import qualified Data.List (break)
import Control.Monad (foldM)
import Types
import PrettyPrint
import Expression
import StrUtil

protocolFromExp :: Exp -> Maybe Protocol
protocolFromExp exp =
  case getInner exp of
    EDict m -> foldM update defaultProtocol $ M.toList m
    _ -> Nothing

update :: Protocol -> (String, Exp) -> Maybe Protocol
update proto (key, vExp) =
  case key of
    "data" -> do
      value <- getString
      case value of
        "immediate" -> change $ \p -> p { proto_data = Immediate False }
        "immediate-req" -> change $ \p -> p { proto_data = Immediate True }
        _ -> let pd = case splitBy ":" value of
                        [name] ->  -- "name"
                            OtherDataProtocol name bit bit
                        [name, reqt, ackt] ->  -- "name:reqtype:acktype"
                            OtherDataProtocol name (FLType reqt) (FLType ackt)
             in change $ \p -> p { proto_data = pd }
        -- "twophase"  -> change $ \p -> p { proto_data = TwoPhase }
        -- "fourphase" -> change $ \p -> p { proto_data = FourPhase }
        -- "pulseecho" -> change $ \p -> p { proto_data = PulseEcho }
        -- "ackpulse" -> change $ \p -> p { proto_data = AckPulse }
        -- "agreement" -> change $ \p -> p { proto_data = Agreement }
        -- 'f':'l':':':rest -> 
        --     let 
        --     in change $ \p -> p { proto_data = 
        -- _ -> Nothing
    "clock" -> do
      value <- getBoolean
      change $ \p -> p { proto_clock = value }
    "reset" -> do
      value <- getBoolean
      change $ \p -> p { proto_reset = value }
    "power" -> do
      value <- getString
      case value of
        "alwayson"    -> change $ \p -> p { proto_power = AlwaysOn }
        "clockgating" -> change $ \p -> p { proto_power = ClockGating }
        _ -> Nothing
    _ -> Nothing
  where
    rx_for name = "bifrost_rx_" ++ name
    tx_for name = "bifrost_tx_" ++ name
    change f = return $ f proto
    getString =
      case getInner vExp of
        EStr s -> Just s
        _ -> Nothing
    getInteger =
      case getInner vExp of
        EInt i -> Just i
        _ -> Nothing
    getBoolean = do
      i <- getInteger
      case i of
        0 -> return False
        1 -> return True
        _ -> Nothing

defaultProtocol :: Protocol
defaultProtocol = Protocol True True (Immediate False) AlwaysOn
