module Shared.Board where

import Prelude

import Shared.Node
import Shared.MathUtils

import Data.Foldable (length)
import Data.Traversable (for)

import Math as Math
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.Array as Array
import Data.Array ((!!))
import Data.Enum (enumFromTo)
import Data.SubRecord
import Data.SubRecord.Builder as SubRecord
import Data.Record as Record
import Data.Symbol (class IsSymbol, SProxy(..))

import Control.Monad.Eff.Exception.Unsafe (unsafeThrow)

import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Decode.Generic.Rep (genericDecodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson)
import Data.Argonaut.Encode.Generic.Rep (genericEncodeJson)
import Data.Generic.Rep as Rep
import Data.Generic.Rep.Show (genericShow)

newtype Node = Node
  { id :: Int
  , x :: Number
  , y :: Number
  , nodeType :: NodeType
  }

derive instance genericNode :: Rep.Generic Node _
instance encodeJsonNode :: EncodeJson Node
  where encodeJson = genericEncodeJson
instance decodeJsonNode :: DecodeJson Node
  where decodeJson = genericDecodeJson
instance showNode :: Show Node
  where show = genericShow

newtype Board = Board
  { nodes :: Array Node
  }

derive instance genericBoard :: Rep.Generic Board _
instance encodeJsonBoard :: EncodeJson Board
  where encodeJson = genericEncodeJson
instance decodeJsonBoard :: DecodeJson Board
  where decodeJson = genericDecodeJson
instance showBoard :: Show Board
  where show = genericShow

generateBoard :: forall f r.
  Monad f =>
  { nextId :: f Int
  , integerInRange :: Int -> Int -> f Int
  | r } ->
  (forall s. { integerInRange :: Int -> Int -> f Int | s } -> BoardData f) ->
  f Board
generateBoard k boardData = do
  startNode <- generateStartNode
  (levels :: Array (Array Node)) <- for (boardData k) $ \level -> do
    let sectionData = generateSectionData level
    (x :: Array (Array Node)) <- for sectionData $ \section -> do
      generateSection k section { amount: level.amount }
    pure (Array.concat x)
  pure (Board { nodes: Array.cons startNode (Array.concat levels) })

generateStartNode :: forall f.
  Monad f =>
  f Node
generateStartNode = do
  pure (Node { id: 0, x: 0.0, y: 0.0, nodeType: Start })

type LevelData f =
  { ampMin :: Int
  , ampMax :: Int
  , quadrants :: Array (f NodeType)
  , levels :: Int
  , amount :: Int
  }

type BoardData f = Array (LevelData f)

boardData :: forall f r.
  Monad f =>
  { integerInRange :: Int -> Int -> f Int
  | r } ->
  BoardData f
boardData k =
  [ { ampMin: 0
    , ampMax: 0
    , quadrants:
      [ pure Start
      ]
    , levels: 1
    , amount: 1
    }
  , { ampMin: 50
    , ampMax: 250
    , quadrants:
      [ pure (basicNode 1)
      , pure (basicNode 1)
      , pure (basicNode 1)
      , pure (basicNode 1)
      ]
    , levels: 3
    , amount: 2
    }
  , { ampMin: 200
    , ampMax: 700
    , quadrants:
      [ pure (blueNode 1)
      , pure (redNode 1)
      , pure (greenNode 1)
      , pure (yellowNode 1)
      ]
    , levels: 4
    , amount: 2
    }
  , { ampMin: 600
    , ampMax: 800
    , quadrants:
      [ victoryNode k { color1: SProxy :: SProxy "red", color2: SProxy :: SProxy "green", color3: SProxy :: SProxy "yellow" }
      , victoryNode k { color1: SProxy :: SProxy "green", color2: SProxy :: SProxy "yellow", color3: SProxy :: SProxy "blue" }
      , victoryNode k { color1: SProxy :: SProxy "yellow", color2: SProxy :: SProxy "blue", color3: SProxy :: SProxy "red" }
      , victoryNode k { color1: SProxy :: SProxy "blue", color2: SProxy :: SProxy "red", color3: SProxy :: SProxy "green" }
      ]
    , levels: 2
    , amount: 3
    }
  ]

victoryNode k colors = do
  a <- k.integerInRange 1 5
  choice <- k.integerInRange 1 3
  pure $ VictoryNode
    { vp: Resources $ choiceToColors { choice: choice, val1: a, val2: (5 - a) } colors
    , cost: Resources $ defaultResources
    , name: "victory"
    }

choiceToColors { choice: 1, val1, val2 } { color1, color2, color3 } =
  Record.set color1 val1 (Record.set color2 val2 defaultResources)
choiceToColors { choice: 2, val1, val2 } { color1, color2, color3 } =
  Record.set color1 val1 (Record.set color3 val2 defaultResources)
choiceToColors { choice: 3, val1, val2 } { color1, color2, color3 } =
  Record.set color2 val1 (Record.set color3 val2 defaultResources)
choiceToColors _ _ = unsafeThrow "invalid choice"

type SectionData f =
  { ampMin :: Int
  , ampMax :: Int
  , angleMin :: Int
  , angleMax :: Int
  , nodeTypes :: f NodeType
  }

generateSectionData :: forall f r.
  { ampMin :: Int
  , ampMax :: Int
  , quadrants :: Array (f NodeType)
  , levels :: Int
  | r } ->
  Array (SectionData f)
generateSectionData { ampMin, ampMax, quadrants, levels } = do
  (level :: Int) <- enumFromTo 1 levels
  (quadrant :: { i :: Int, x :: f NodeType }) <-
    quadrants # Array.mapWithIndex (\i x -> { i: i, x: x })
  let levelAmp = (ampMax - ampMin) / levels
  let quadrantSize = 360 / (length quadrants)
  pure $
    { ampMin: ampMin + ((level - 1) * levelAmp)
    , ampMax: ampMin + (level * levelAmp)
    , angleMin: quadrant.i * quadrantSize
    , angleMax: (quadrant.i + 1) * quadrantSize
    , nodeTypes: quadrant.x
    }

generateSection :: forall f r.
  Monad f =>
  { nextId :: f Int
  , integerInRange :: Int -> Int -> f Int
  | r } ->
  SectionData f ->
  { amount :: Int } ->
  f (Array Node)
generateSection k { ampMin, ampMax, angleMin, angleMax, nodeTypes } { amount } =
  for (enumFromTo 1 amount) $ \i -> do
    r <- k.integerInRange ampMin ampMax <#> Int.toNumber
    φ <- k.integerInRange angleMin angleMax <#> Int.toNumber >>> deg2Rad
    id <- k.nextId
    nodeType <- nodeTypes
    pure (Node { id: id
               , x: r * Math.cos φ
               , y: r * Math.sin φ
               , nodeType: nodeType
               })

chooseSet :: forall f a r.
  Monad f =>
  { integerInRange :: Int -> Int -> f Int
  | r } ->
  Array a -> f a
chooseSet k l = do
  let len = length l
  i <- k.integerInRange 0 (len - 1)
  pure $ case l !! i of
    Just x -> x
    Nothing -> unsafeThrow "chooseSet error: should not happen"

