module Shared.ClientMessage where

import Prelude

import Shared.Solution

import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Decode.Generic.Rep (genericDecodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson)
import Data.Argonaut.Encode.Generic.Rep (genericEncodeJson)
import Data.Generic.Rep as Rep
import Data.Generic.Rep.Show (genericShow)

-- message from client to server
data ClientMessage
  = RefreshCurrentTop
  | GetCurrentBoard
  | SubmitSolution { solution :: Solution }
    --{ solution :: , mapId ::  }

derive instance genericClientMessage :: Rep.Generic ClientMessage _
instance encodeJsonClientMessage :: EncodeJson ClientMessage
  where encodeJson = genericEncodeJson
instance decodeClientMessage :: DecodeJson ClientMessage
  where decodeJson = genericDecodeJson
instance showClientMessage :: Show ClientMessage
  where show = genericShow
