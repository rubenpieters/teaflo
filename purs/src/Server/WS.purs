module Server.WS where

import Prelude

import Data.Foreign.Callback

import Data.Argonaut.Core (stringify)
import Data.Argonaut.Decode.Class (class DecodeJson, decodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson, encodeJson)

import Control.Monad.Eff (kind Effect, Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Uncurried

foreign import data WS :: Effect

-- first `Type`: received message types
-- second `Type`: sent message types
-- a server receives cmsg :: client messages
-- a server sends smsg :: server messages
foreign import data Server :: Type -> Type -> Type
-- a server receives smsg :: server messages
-- a server sends cmsg :: client messages
foreign import data Client :: Type -> Type -> Type

foreign import mkServer :: forall e cmsg smsg.
  { port :: Int } -> Eff (ws :: WS | e) (Server cmsg smsg)

sendMessage :: forall cmsg smsg.
  (EncodeJson smsg) =>
  smsg ->
  (Client smsg cmsg) ->
  Eff _ Unit
sendMessage msg client = do
  client # unsafeSendMessage (msg # encodeJson >>> stringify)

foreign import unsafeSendMessage :: forall e cmsg smsg.
  String -> (Client cmsg smsg) -> Eff (ws :: WS | e) Unit

foreign import unsafeOn :: forall event cb obj eff.
  EffFn3 (ws :: WS | eff) event cb obj Unit

initializeServer :: forall cmsg smsg.
  { port :: Int }
  -> Eff _ (Server cmsg smsg)
initializeServer config = mkServer config

onClConnect :: forall e cmsg smsg.
  (Client smsg cmsg -> Eff e Unit) -> Server cmsg smsg -> Eff _ Unit
onClConnect cb server = runEffFn3 unsafeOn "connection" (callback1 cb) server

onClMessage :: forall e cmsg smsg.
  (String -> Eff e Unit) -> Client smsg cmsg -> Eff _ Unit
onClMessage cb server = runEffFn3 unsafeOn "message" (callback1 cb) server

onClDisconnect :: forall e cmsg smsg.
  (Eff e Unit) -> Client smsg cmsg -> Eff _ Unit
onClDisconnect cb server = runEffFn3 unsafeOn "close" (callback0 cb) server
