module Shared.Solution where

import Prelude

import Shared.Board

import Data.Map (Map(..))

import Control.Monad.Eff.Exception.Unsafe (unsafeThrow)

import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Decode.Generic.Rep (genericDecodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson)
import Data.Argonaut.Encode.Generic.Rep (genericEncodeJson)
import Data.Generic.Rep as Rep
import Data.Generic.Rep.Show (genericShow)


newtype Connection = Connection
  { to :: Node
  , distance :: Number
  }

foreign import kind V

foreign import data Verified :: V
foreign import data Unverified :: V

newtype Solution (x :: V) =
  Solution (Map Int (Array Connection))

derive instance genericConnection :: Rep.Generic Connection _
instance encodeJsonConnection :: EncodeJson Connection
  where encodeJson = genericEncodeJson
instance decodeJsonConnection :: DecodeJson Connection
  where decodeJson = genericDecodeJson
instance showConnection :: Show Connection
  where show = genericShow

derive instance genericSolution :: Rep.Generic (Solution x) _
instance encodeJsonSolutionVerified :: EncodeJson (Solution Verified)
  where encodeJson = genericEncodeJson
instance decodeJsonSolutionVerified :: DecodeJson (Solution Verified)
  where decodeJson = genericDecodeJson
instance showSolution :: Show (Solution x)
  where show = genericShow
