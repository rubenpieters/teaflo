module Shared.Solution where

import Prelude

import Shared.MathUtils as MathUtils
import Shared.Node
import Shared.Board

import Data.Either (Either(..))
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

data RejectReason
  = Angle

addConnection ::
  -- distance = distance from to
  -- fromResources at the 'from' node, precalculated
  { from :: Node, to :: Node, distance :: Number, fromResources :: Record ResourcesR } ->
  Solution Verified ->
  Either { rejectReason :: RejectReason } { newSolution :: Solution Verified }
addConnection { from, to, distance } sol = Left { rejectReason: Angle }

verifyAngle ::
  { closest :: Node, furthest :: Node } ->
  Boolean
verifyAngle { closest: (Node closest), furthest: (Node furthest) } =
  (closest.nodeType /= Start) && (verifyAngle <= 90.0) && (verifyAngle >= (-90.0))
  where
  angleNewLine = MathUtils.rad2Deg (MathUtils.angleBetween { point1: closest, point2: furthest })
  angleCenter = MathUtils.rad2Deg (MathUtils.angleBetween { point1: { x: 0.0, y: 0.0 }, point2: closest })
  verifyAngle = MathUtils.wrapAngle (angleCenter - angleNewLine)
