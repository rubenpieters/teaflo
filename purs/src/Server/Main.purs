module Server.Main where

import Prelude

import Server.WS

import Shared.ClientMessage
import Shared.ServerMessage

import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Data.Foreign.Callback

import Data.Argonaut.Core (stringify)
import Data.Argonaut.Decode.Class (class DecodeJson, decodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson, encodeJson)
import Data.Argonaut.Parser (jsonParser)

import Control.Monad.Eff (kind Effect, Eff)
import Control.Monad.Eff.Console (CONSOLE, log)

import Node.Process as Node

main :: Eff _ Unit
main = startServer

startServer :: Eff _ Unit
startServer = do
  log "Server started"
  envPort <- Node.lookupEnv "PORT"
  let parsedPort = case (envPort >>= Int.fromString) of
        Just p -> p
        Nothing -> 8080
  log ("binding to port: " <> show parsedPort)
  wss <- mkServer { port: parsedPort }
  wss # onClConnect (onSocketConnection wss)
  pure unit

onSocketConnection ::
  (Server ClientMessage ServerMessage) ->
  (Client ServerMessage ClientMessage) ->
  Eff _ Unit
onSocketConnection server client = do
  log ("Player Connect")
  client # onClMessage (onClientStrMessage server client)
  client # onClDisconnect (onDisconnect)
  client # sendMessage (CurrentTop { top: [1,2,3] })

onClientStrMessage ::
  (Server ClientMessage ServerMessage) ->
  (Client ServerMessage ClientMessage) ->
  String ->
  Eff _ Unit
onClientStrMessage server client message = do
  log ("received message " <> message)
  let eSvMsg = jsonParser message >>= decodeJson
  case eSvMsg of
    Left errs -> log ("malformed message")
    Right (clMsg :: ClientMessage) -> onClientMessage server client clMsg

onClientMessage ::
  (Server ClientMessage ServerMessage) ->
  (Client ServerMessage ClientMessage) ->
  ClientMessage ->
  Eff _ Unit
onClientMessage server client _ = pure unit

onDisconnect :: Eff _ Unit
onDisconnect = do
  log ("Player Disconnect")
