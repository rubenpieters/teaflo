module Client.Network where

import Prelude

import Shared.ClientMessage
import Shared.ServerMessage

import Data.Either (Either(..))

import Control.Monad.Eff
import Control.Monad.Eff.Console (log)

import Data.Argonaut.Decode.Class (class DecodeJson, decodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson, encodeJson)
import Data.Argonaut.Parser (jsonParser)

onServerStrMessage ::
  String ->
  Eff _ Unit
onServerStrMessage message = do
  let eSvMsg = jsonParser message >>= decodeJson
  case eSvMsg of
    Left errs -> log ("malformed message: " <> message)
    Right (svMsg :: ServerMessage) -> onServerMessage svMsg

onServerMessage ::
  ServerMessage ->
  Eff _ Unit
onServerMessage (CurrentTop { top }) = log ("top " <> show top)
onServerMessage (CurrentBoard { board }) = log ("board " <> show board)
