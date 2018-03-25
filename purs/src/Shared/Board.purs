module Shared.Board where

import Prelude

import Shared.Node

import Data.Foldable (length)
import Data.Traversable (for)

import Math as Math
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.Array as Array
import Data.Array ((!!))
import Data.Enum (enumFromTo)

import Control.Monad.Eff.Exception.Unsafe (unsafeThrow)

newtype Node = Node
  { id :: Int
  , x :: Number
  , y :: Number
  , nodeType :: NodeType
  }

newtype Board = Board
  { nodes :: Array Node
  }

generateBoard :: forall f r.
  Monad f =>
  { nextId :: f Int
  , integerInRange :: Int -> Int -> f Int
  | r } ->
  BoardData ->
  f Board
generateBoard k boardData = do
  startNode <- generateStartNode
  (levels :: Array (Array Node)) <- for boardData $ \level -> do
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

type LevelData =
  { ampMin :: Int
  , ampMax :: Int
  , quadrants :: Array (Array NodeType)
  , levels :: Int
  , amount :: Int
  }

type BoardData = Array LevelData

boardData :: BoardData
boardData =
  [ { ampMin: 0
    , ampMax: 0
    , quadrants:
      [ [Start]
      ]
    , levels: 1
    , amount: 1
    }
  , { ampMin: 50
    , ampMax: 250
    , quadrants:
      [ [basicNode 1]
      , [basicNode 1]
      , [basicNode 1]
      , [basicNode 1]
      ]
    , levels: 3
    , amount: 2
    }
  , { ampMin: 200
    , ampMax: 700
    , quadrants:
      [ [blueNode 1]
      , [redNode 1]
      , [greenNode 1]
      , [yellowNode 1]
      ]
    , levels: 4
    , amount: 2
    }
  , { ampMin: 600
    , ampMax: 800
    , quadrants:
      [ [Start]
      , [Start]
      , [Start]
      , [Start]
      ]
    , levels: 2
    , amount: 3
    }
  ]

type SectionData =
  { ampMin :: Int
  , ampMax :: Int
  , angleMin :: Int
  , angleMax :: Int
  , nodeTypes :: Array NodeType
  }

generateSectionData :: forall r.
  { ampMin :: Int
  , ampMax :: Int
  , quadrants :: Array (Array NodeType)
  , levels :: Int
  | r } ->
  Array SectionData
generateSectionData { ampMin, ampMax, quadrants, levels } = do
  (level :: Int) <- enumFromTo 1 levels
  (quadrant :: { i :: Int, x :: Array NodeType }) <-
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
  SectionData ->
  { amount :: Int } ->
  f (Array Node)
generateSection k { ampMin, ampMax, angleMin, angleMax, nodeTypes } { amount } =
  for (enumFromTo 1 amount) $ \i -> do
    r <- k.integerInRange ampMin ampMax <#> toNumber
    φ <- k.integerInRange angleMin angleMax <#> toNumber >>> deg2Rad
    id <- k.nextId
    nodeType <- (chooseSet k) nodeTypes
    pure (Node { id: id
               , x: r * Math.cos φ
               , y: r * Math.sin φ
               , nodeType: nodeType
               })

deg2Rad :: Number -> Number
deg2Rad x = x * Math.pi / 180.0

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
