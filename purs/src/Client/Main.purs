module Client.Main (module Client.Main, module Exported) where

import Client.Draw
import Client.Network
import Data.Foldable
import Data.Monoid
import Data.Monoid.Endo
import Data.Newtype
import Data.SubRecord
import Prelude
import Shared.Board
import Shared.Node
import Shared.Solution
import Tupc

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log, CONSOLE)
import Control.Monad.Eff.Exception (throw)

import Data.Array as Array
import Data.Decimal as Decimal
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq as Rep
import Data.Generic.Rep.Show as Rep
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Maybe (Maybe(..)) as Exported
import Data.StrMap (StrMap)
import Data.StrMap as StrMap
import Data.String as String
import Data.SubRecord as SubRecord
import Data.Tuple (Tuple(..))
import Tupc (Pos(..)) as Exported


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
    [ "11------------22"
    , "33--------------"
    , "----------------"
    , "33--------------"
    , "44--------------"
    , "----------------"
    , "44--------------"
    , "----------------"
    , "----------------"
    , "----------------"
    , "----------------"
    , "11------------22"
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

{-
type Connection =
  { to :: Node
  , distance :: Number
  }

type Connections =
  Map Int (Array Connection)
-}

initCxns :: Solution Verified
initCxns = Solution Map.empty

addConnectionJS ::
  { from :: Record NodeR
  , to :: Record NodeR
  -- resources at the 'from' node, precalculated
  , fromResources :: Record ResourcesR
  -- ids of already connected nodes
  , connectedNodeIds :: Array Int
  } ->
  Solution Verified ->
  SubRecord
    ( added :: Boolean
    , rejectReason :: Array RejectReason
    , newSolution :: Solution Verified
    , newResources :: Resources
    , furthestId :: Int
    )
addConnectionJS { from, to, fromResources, connectedNodeIds } sol =
  case (addConnection
    { from: Node from
    , to: Node to
    , fromResources: fromResources
    , connectedNodeIds: connectedNodeIds
    }) sol of
      Left { rejectReason } ->
        mkSubRecord { added: false, rejectReason: rejectReason }
      Right { newSolution, newResources, furthestId } ->
        mkSubRecord { added: true
                    , newSolution: newSolution
                    , newResources: newResources
                    , furthestId: furthestId
                    }


addLink :: { closest :: Node, furthest :: Node, distance :: Number } -> Solution Verified -> Solution Verified
addLink { closest, furthest, distance } (Solution sol) =
  Solution (sol # Map.alter addLink' closest'.id)
  where
    (Node closest') = closest
    newCxn = Connection { to: furthest, distance: distance }
    addLink' (Just l) = Just (Array.cons newCxn l)
    addLink' (Nothing) = Just [newCxn]

traverseConnections = traverseConnections' centerId

traverseConnections' :: forall a.
  (Monoid a) =>
  Int ->
  (Connection -> a) ->
  Solution Verified ->
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

calcResource :: Solution Verified -> ResourceResult
calcResource cxns = un Endo (calcResource' cxns) initialResources

calcResource' :: Solution Verified -> Endo ResourceResult
calcResource' = traverseConnections f
  where
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

type VPResult =
  { vp :: Int
  }

calcVP :: ResourceResult -> Solution Verified -> VPResult
calcVP totals cxns = un Endo (calcVP' totals cxns) { vp: 0 }

calcVP' :: ResourceResult -> Solution Verified -> Endo VPResult
calcVP' totals = traverseConnections f
  where
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

resourceText :: Record ResourcesR -> String
resourceText { growth, white, blue, red, green, yellow } =
  "gr: " <> show (growth #
    Decimal.fromNumber >>>
    Decimal.toSignificantDigits 4 >>>
    Decimal.toNumber
    ) <>
  "\nW: " <> show white <>
  "\nB: " <> show blue <>
  "R: " <> show red <>
  "\nG: " <> show green <>
  " Y: " <> show yellow

-- Network

onServerStrMessageJS :: forall eff r.
  { onGetCurrentTop :: { top :: Array Int } -> Eff (console :: CONSOLE | eff) Unit
  , onGetCurrentBoard :: { board :: Board } -> Eff (console :: CONSOLE | eff) Unit
  | r } ->
  String ->
  Eff (console :: CONSOLE | eff) Unit
onServerStrMessageJS k = onServerStrMessage
  { log: log
  , onGetCurrentTop: k.onGetCurrentTop
  , onGetCurrentBoard: k.onGetCurrentBoard
  }

main :: Eff _ Unit
main = pure unit
