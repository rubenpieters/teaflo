module Client.Network where

import Prelude

import Shared.Board
import Shared.ClientMessage
import Shared.ServerMessage

import Data.Either (Either(..))

import Control.Monad.Eff
import Control.Monad.Eff.Console (log)

import Data.Argonaut.Decode.Class (class DecodeJson, decodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson, encodeJson)
import Data.Argonaut.Parser (jsonParser)

onServerStrMessage :: forall f r.
  { log :: String -> f Unit
  , onGetCurrentTop :: { top :: Array Int } -> f Unit
  , onGetCurrentBoard :: { board :: Board } -> f Unit
  | r } ->
  String ->
  f Unit
onServerStrMessage k message = do
  let eSvMsg = jsonParser message >>= decodeJson
  case eSvMsg of
    Left errs -> k.log ("malformed message: " <> message)
    Right (svMsg :: ServerMessage) -> onServerMessage k svMsg

onServerMessage :: forall f r.
  { onGetCurrentTop :: { top :: Array Int } -> f Unit
  , onGetCurrentBoard :: { board :: Board } -> f Unit
  | r } ->
  ServerMessage ->
  f Unit
onServerMessage k (CurrentTop msg) = k.onGetCurrentTop msg
--log ("top " <> show top)
onServerMessage k (CurrentBoard msg) = k.onGetCurrentBoard msg
--log ("board " <> show board)
