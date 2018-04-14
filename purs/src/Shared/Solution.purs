module Shared.Solution where

import Prelude


import Shared.MathUtils as MathUtils
import Shared.Node
import Shared.Board

import Data.Foldable
import Data.Newtype
import Data.Monoid
import Data.Monoid.Endo
import Data.Foldable (elem)
import Data.Either (Either(..))
import Data.Map (Map(..))
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Array as Array

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

newtype Solution =
  Solution (Map Int (Array Connection))

derive instance genericConnection :: Rep.Generic Connection _
instance encodeJsonConnection :: EncodeJson Connection
  where encodeJson = genericEncodeJson
instance decodeJsonConnection :: DecodeJson Connection
  where decodeJson = genericDecodeJson
instance showConnection :: Show Connection
  where show = genericShow

derive instance genericSolution :: Rep.Generic Solution _
instance encodeJsonSolution :: EncodeJson Solution
  where encodeJson = genericEncodeJson
instance decodeJsonSolution :: DecodeJson Solution
  where decodeJson = genericDecodeJson
instance showSolution :: Show Solution
  where show = genericShow

data RejectReason
  = Angle
  | FurthestConnected
  | ClosestNotConnected
  | NotEnoughResources

findClosest ::
  { node1 :: Record NodeR, node2 :: Record NodeR } ->
  { closest :: Record NodeR, furthest :: Record NodeR }
findClosest { node1, node2 } =
  let
    dist1 = MathUtils.distance { point1: { x: 0.0, y: 0.0 }, point2: node1}
    dist2 = MathUtils.distance { point1: { x: 0.0, y: 0.0 }, point2: node2}
  in if (dist1 > dist2)
    then { closest: node2, furthest: node1 }
    else { closest: node1, furthest: node2 }

addConnection ::
  { from :: Node
  , to :: Node
  -- resources at the 'from' node, precalculated
  , fromResources :: Record ResourcesR
  -- ids of already connected nodes
  , connectedNodeIds :: Array Int
  } ->
  Solution ->
  Either
    { rejectReason :: Array RejectReason }
    { newSolution :: Solution
    , newResources :: Resources
    , furthestId :: Int
    }
addConnection { from: (Node from), to: (Node to), fromResources, connectedNodeIds } (Solution sol) =
  let
    { closest, furthest } = findClosest { node1: from, node2: to }
    distance = MathUtils.distance { point1: from, point2: to }
    -- TODO: verify distance, check with current growth
    verifyFurthest = not $ connectedNodeIds # elem furthest.id
    verifyClosest = connectedNodeIds # elem closest.id
    verifyNodeAngle = verifyAngle { closest: (Node closest), furthest: (Node furthest) }
    { canBuy: verifyCost, newResources } = verifyCost (Resources fromResources) furthest.nodeType
    (verifyList :: Array RejectReason) =
      [ ifFalse verifyFurthest FurthestConnected
      , ifFalse verifyClosest ClosestNotConnected
      , ifFalse verifyNodeAngle Angle
      -- TODO: not enough resource message should contain more information
      , ifFalse verifyCost NotEnoughResources
      ] # Array.catMaybes
    in
      if verifyList # Array.null
        then Left { rejectReason: verifyList }
        else Right { newSolution: Solution (sol # addLink { closest: (Node closest)
                                                          , furthest: (Node furthest)
                                                          , distance: distance
                                                          }
                                            )
                   , newResources: newResources
                   , furthestId: furthest.id
                   }
  --Left { rejectReason: [Angle] }

addLink ::
  { closest :: Node, furthest :: Node, distance :: Number } ->
  Map Int (Array Connection) ->
  Map Int (Array Connection)
addLink { closest, furthest, distance } cxns =
  cxns # Map.alter addLink' closest'.id
  where
    (Node closest') = closest
    newCxn = Connection { to: furthest, distance: distance }
    addLink' (Just l) = Just (Array.cons newCxn l)
    addLink' (Nothing) = Just ([newCxn])

ifFalse :: forall a. Boolean -> a -> Maybe a
ifFalse false a = Just a
ifFalse true _ = Nothing

verifyAngle ::
  { closest :: Node, furthest :: Node } ->
  Boolean
verifyAngle { closest: (Node closest), furthest: (Node furthest) } =
  (closest.nodeType /= Start) && (verifyAngle <= 90.0) && (verifyAngle >= (-90.0))
  where
  angleNewLine = MathUtils.rad2Deg (MathUtils.angleBetween { point1: closest, point2: furthest })
  angleCenter = MathUtils.rad2Deg (MathUtils.angleBetween { point1: { x: 0.0, y: 0.0 }, point2: closest })
  verifyAngle = MathUtils.wrapAngle (angleCenter - angleNewLine)

verifyCost ::
  Resources ->
  NodeType ->
  { canBuy :: Boolean, newResources :: Resources }
verifyCost res Start = { canBuy: true, newResources: res }
verifyCost res (ResourceNode { gain, cost }) =
  { canBuy: checkResources, newResources: newResources `plus` gain }
  where
  newResources = res `minus` cost
  checkResources = newResources # isValid
verifyCost res (VictoryNode { vp, cost }) =
  { canBuy: checkResources, newResources: newResources }
  where
  newResources = res `minus` cost
  checkResources = newResources # isValid

--------------------------------------
-- Traverse Solution
--------------------------------------

centerId :: Int
centerId = 0

traverseConnections = traverseConnections' centerId

traverseConnections' :: forall a.
  (Monoid a) =>
  Int ->
  (Connection -> a) ->
  Solution ->
  a
traverseConnections' id f sol'@(Solution sol) =
  case sol # Map.lookup id of
    Just links ->
      let
        { result: result, nextIds: nextIds } = traverseLink f links
      in
        result <> (nextIds # foldMap (\i -> traverseConnections' i f sol'))
    Nothing -> mempty

traverseLink :: forall a.
  (Monoid a) =>
  (Connection -> a) ->
  Array Connection ->
  { result :: a, nextIds :: Array Int }
traverseLink f links = case (Array.uncons links) of
  Just { head: link'@(Connection link), tail: t } ->
    let
      { result: result, nextIds: nextIds } = traverseLink f t
      (Node toNode) = link.to
    in
      { result: f link' <> result, nextIds: Array.cons toNode.id nextIds }
  Nothing -> { result: mempty, nextIds: [] }

--------------------------------------
-- Calculate VP on Board
--------------------------------------

type ResourceResult =
  { growth :: Number
  , white :: Int
  , blue :: Int
  , red :: Int
  , green :: Int
  , yellow :: Int
  }

type VPResult =
  { vp :: Int
  }

calculateVP :: ResourceResult -> Solution -> VPResult
calculateVP totals cxns = un Endo (calculateVP' totals cxns) { vp: 0 }
  where
  calculateVP' :: ResourceResult -> Solution -> Endo VPResult
  calculateVP' totals = traverseConnections f
  f :: Connection -> Endo VPResult
  f cxn = Endo (f' cxn)
  f' :: Connection -> VPResult -> VPResult
  f' (Connection { to, distance }) r@{ vp } =
    let
      (Node node) = to
    in case node.nodeType of
      VictoryNode { vp: gain } ->
        { vp:  vp + ((gain `times` (Resources totals)) # sumColors) }
      _ -> r

--------------------------------------
-- Calculate Resources on Board
--------------------------------------

initialResources :: ResourceResult
initialResources =
  { growth: 100.0
  , white: 0
  , blue: 0
  , red: 0
  , green: 0
  , yellow: 0
  }

calculateResources :: Solution -> ResourceResult
calculateResources cxns = un Endo (calculateResources' cxns) initialResources
  where
  calculateResources' :: Solution -> Endo ResourceResult
  calculateResources' = traverseConnections f
  f :: Connection -> Endo ResourceResult
  f cxn = Endo (f' cxn)
  f' :: Connection -> ResourceResult -> ResourceResult
  f' (Connection { to, distance }) { growth, white, blue, red, green, yellow } =
    let
      (Node node) = to
      gain = node.nodeType # nodeGain
    in
      { growth: growth - distance
      , white: white + gain.white
      , blue: blue + gain.blue
      , red: red + gain.red
      , green: green + gain.green
      , yellow: yellow + gain.yellow
      }
