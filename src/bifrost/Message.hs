module Message where

-- Error and warning messages.

import Data.Either

data Message = Error String | Warning String

instance Show Message where
  show m =
    case m of
      Error s -> "[E] " ++ s
      Warning s -> "[W] " ++ s

isError :: Message -> Bool
isError m =
  case m of
    Error _ -> True
    _ -> False

isWarning :: Message -> Bool
isWarning m =
  case m of
    Warning _ -> True
    _ -> False

messageFromEither :: Either String String -> Message
messageFromEither eith =
  case eith of
    Left s -> Error s
    Right s -> Warning s

messageToEither :: Message -> Either String String
messageToEither msg =
  case msg of
    Error s -> Left s
    Warning s -> Right s

mapMessage :: (String -> String) -> (String -> String) -> Message -> Message
mapMessage f1 f2 m =
  case m of
    Error s -> Error $ f1 s
    Warning s -> Warning $ f2 s
