module Client.Main (module Client.Main, module Exported) where

import Tupc (Pos(..)) as Exported
import Data.Maybe (Maybe(..)) as Exported

import Prelude

import Shared.Node
import Shared.Board
import Client.Draw
import Client.Network

import Tupc

import Data.Map (Map)
import Data.Map as Map
import Data.StrMap (StrMap)
import Data.StrMap as StrMap
import Data.Tuple (Tuple(..))
import Data.String as String
import Data.SubRecord as SubRecord
import Data.Maybe (Maybe(..))
import Data.Array as Array
import Data.Monoid
import Data.Monoid.Endo
import Data.Foldable
import Data.Newtype
import Data.SubRecord
import Data.Decimal as Decimal

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (throw)

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq as Rep
import Data.Generic.Rep.Show as Rep

import Debug.Trace

-- UI

tupcMap ::
  SubJsonConfigContent -> Eff _ (StrMap EnrichedPos)
tupcMap x = do
  map <- fromJson { throw: throw } x
  pure $ convert map
  where
    convert :: Map Char EnrichedPos -> StrMap EnrichedPos
    convert =
      (Map.toUnfoldable :: Map Char EnrichedPos -> Array (Tuple Char EnrichedPos)) >>>
      map (\(Tuple k v) -> Tuple (String.singleton k) v) >>>
      StrMap.fromFoldable

buttonMapConfig :: SubJsonConfigContent
buttonMapConfig =
  { subJsonConfig: SubRecord.mkSubRecord
    { scale: 50
    }
  , content:
    -- 1 2 3 4 5 6 7 8
    [ "----------------"
    , "----1------1----"
    , "----------------"
    , "-----2----2-----"
    , "------3--3------"
    , "----------------"
    , "------3--3------"
    , "-----4----4-----"
    , "-----5----5-----"
    , "----------------"
    , "----------------"
    , "----------------"
    ]
  }

buttonMap :: Eff _ (StrMap EnrichedPos)
buttonMap = tupcMap buttonMapConfig

gameUiMapConfig :: SubJsonConfigContent
gameUiMapConfig =
  { subJsonConfig: SubRecord.mkSubRecord
    { scale: 50
    }
  , content:
    -- 1 2 3 4 5 6 7 8
    [ "----------------"
    , "----------------"
    , "----------------"
    , "----------------"
    , "----------------"
    , "----------------"
    , "----------------"
    , "----------------"
    , "----------------"
    , "----------------"
    , "--------------11"
    , "--------------11"
    ]
  }

gameUiMap :: Eff _ (StrMap EnrichedPos)
gameUiMap = tupcMap gameUiMapConfig

-- Node

type Bulb =
  { x :: Int
  , y :: Int
  , id :: Int
  }

centerId :: Int
centerId = 0

type Connection =
  { to :: Node
  , distance :: Number
  }

type Connections =
  Map Int (Array Connection)

initCxns :: Connections
initCxns = Map.empty

addLink :: { closest :: Node, furthest :: Node, distance :: Number } -> Connections -> Connections
addLink { closest, furthest, distance } cxns =
  cxns # Map.alter addLink' closest'.id
  where
    (Node closest') = closest
    newCxn = { to: furthest, distance: distance }
    addLink' (Just l) = Just (Array.cons newCxn l)
    addLink' (Nothing) = Just ([newCxn])

traverseConnections = traverseConnections' centerId

traverseConnections' :: forall a.
  (Monoid a) =>
  Int ->
  (Connection -> a) ->
  Connections ->
  a
traverseConnections' id f cxns =
  case cxns # Map.lookup id of
    Just links ->
      let
        { result: result, nextIds: nextIds } = traverseLink f links
      in
        result <> (nextIds # foldMap (\i -> traverseConnections' i f cxns))
    Nothing -> mempty

traverseLink :: forall a.
  (Monoid a) =>
  (Connection -> a) ->
  Array Connection ->
  { result :: a, nextIds :: Array Int }
traverseLink f links = case (Array.uncons links) of
  Just { head: link, tail: t } ->
    let
      { result: result, nextIds: nextIds } = traverseLink f t
      (Node toNode) = link.to
    in
      { result: f link <> result, nextIds: Array.cons toNode.id nextIds }
  Nothing -> { result: mempty, nextIds: [] }

type ResourceResult =
  { growth :: Number
  , white :: Int
  , blue :: Int
  , red :: Int
  , green :: Int
  , yellow :: Int
  }

initialResources :: ResourceResult
initialResources =
  { growth: 100.0
  , white: 0
  , blue: 0
  , red: 0
  , green: 0
  , yellow: 0
  }

calcResource :: Connections -> ResourceResult
calcResource cxns = un Endo (calcResource' cxns) initialResources

calcResource' :: Connections -> Endo ResourceResult
calcResource' = traverseConnections f
  where
    f :: Connection -> Endo ResourceResult
    f cxn = Endo (f' cxn)
    f' :: Connection -> ResourceResult -> ResourceResult
    f' { to, distance } { growth, white, blue, red, green, yellow } =
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

type VPResult =
  { vp :: Int
  }

calcVP :: ResourceResult -> Connections -> VPResult
calcVP totals cxns = un Endo (calcVP' totals cxns) { vp: 0 }

calcVP' :: ResourceResult -> Connections -> Endo VPResult
calcVP' totals = traverseConnections f
  where
  f :: Connection -> Endo VPResult
  f cxn = Endo (f' cxn)
  f' :: Connection -> VPResult -> VPResult
  f' { to, distance } r@{ vp } =
    let
      (Node node) = to
    in case node.nodeType of
      VictoryNode { vp: gain } ->
        { vp:  vp + ((gain `times` totals) # sumColors) }
      _ -> r

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

generateBoardJS :: forall e r.
  { nextId :: Eff e Int
  , integerInRange :: Int -> Int -> Eff e Int
  | r } ->
  Eff e Board
generateBoardJS k = generateBoard k boardData

drawBoardJS :: forall e r.
  { drawNode :: Node -> Eff e Unit
  | r } ->
  Board ->
  Eff e Unit
drawBoardJS k board = drawBoard k board

nodeColorJS = nodeColor

nodeTextJS = nodeText

resourceText :: Resources -> String
resourceText { growth, white, blue, red, green, yellow } =
  "gr:" <> show (growth #
    Decimal.fromNumber >>>
    Decimal.toSignificantDigits 4 >>>
    Decimal.toNumber
    ) <>
  " W: " <> show white <>
  " B: " <> show blue <>
  " R: " <> show red <>
  " G: " <> show green <>
  " Y: " <> show yellow

-- Network

onServerStrMessageJS = onServerStrMessage

main :: Eff _ Unit
main = pure unit
