module Server.Main where

import Prelude

import Server.WS as WS
import Server.DB as DB

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
main = do
  -- initialize server
  log "Server started"
  envPort <- Node.lookupEnv "PORT"
  let parsedPort = case (envPort >>= Int.fromString) of
        Just p -> p
        Nothing -> 8080
  log ("binding to port: " <> show parsedPort)
  wss <- WS.mkServer { port: parsedPort }
  wss # WS.onClConnect (onSocketConnection k)
  -- initialize db connection
  -- TODO
  where
  k =
    { log: log
    , onClMessage: WS.onClMessage
    , onClDisconnect: WS.onClDisconnect
    , sendMessage: WS.sendMessage
    , getCurrentTop: pure { top: [1,2,3] }
    }

onSocketConnection :: forall f client r.
  Monad f =>
  { log :: String -> f Unit
  , onClMessage :: (String -> f Unit) -> client -> f Unit
  , onClDisconnect :: f Unit -> client -> f Unit
  , sendMessage :: ServerMessage -> client -> f Unit
  , getCurrentTop :: f { top :: Array Int }
  | r } ->
  client ->
  f Unit
onSocketConnection k client = do
  k.log ("Player Connect")
  -- set event handlers
  client # k.onClMessage (onClientStrMessage k client)
  client # k.onClDisconnect (onDisconnect k)
  -- read current top solutions and send to client
  refreshCurrentTop k client

refreshCurrentTop :: forall f client r.
  Monad f =>
  { log :: String -> f Unit
  , sendMessage :: ServerMessage -> client -> f Unit
  , getCurrentTop :: f { top :: Array Int }
  | r } ->
  client ->
  f Unit
refreshCurrentTop k client = do
  currentTop <- k.getCurrentTop
  client # k.sendMessage (CurrentTop currentTop)

onClientStrMessage :: forall f client r.
  Monad f =>
  { log :: String -> f Unit
  , onClMessage :: (String -> f Unit) -> client -> f Unit
  , onClDisconnect :: f Unit -> client -> f Unit
  , sendMessage :: ServerMessage -> client -> f Unit
  , getCurrentTop :: f { top :: Array Int }
  | r } ->
  client ->
  String ->
  f Unit
onClientStrMessage k client message = do
  k.log ("received message " <> message)
  let eSvMsg = jsonParser message >>= decodeJson
  case eSvMsg of
    Left errs -> k.log ("malformed message")
    Right (clMsg :: ClientMessage) -> onClientMessage k client clMsg

onClientMessage :: forall f client r.
  Monad f =>
  { log :: String -> f Unit
  , onClMessage :: (String -> f Unit) -> client -> f Unit
  , onClDisconnect :: f Unit -> client -> f Unit
  , sendMessage :: ServerMessage -> client -> f Unit
  , getCurrentTop :: f { top :: Array Int }
--  , addNewTop
  | r } ->
  client ->
  ClientMessage ->
  f Unit
onClientMessage k client (RefreshCurrentTop) = do
  refreshCurrentTop k client
onClientMessage k client (SubmitSolution) = do
  -- validate submitted solution
  -- if not validated, inform client
  -- if validated, check against current top
  -- if better, add current solution as top
  -- if not better, inform client
  pure unit

onDisconnect :: forall f client r.
  Monad f =>
  { log :: String -> f Unit
  , onClMessage :: (String -> f Unit) -> client -> f Unit
  , onClDisconnect :: f Unit -> client -> f Unit
  , sendMessage :: ServerMessage -> client -> f Unit
  | r } ->
  f Unit
onDisconnect k = do
  k.log ("Player Disconnect")
