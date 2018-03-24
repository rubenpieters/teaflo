module Shared.Board where

import Prelude

import Shared.Node

import Data.Traversable (for)

import Math as Math
import Data.Int (toNumber)
import Data.Array as Array
import Data.Enum (enumFromTo)

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
  -- TODO: implement chooseSet in terms of integerInRange?
  , chooseSet :: forall x. Array x -> f x
  | r } ->
  BoardData ->
  f Board
generateBoard k boardData = do
  startNode <- generateStartNode
  (levels :: Array (Array Node)) <- for boardData $ \level -> do
    let sectionData = generateSectionData level
    (x :: Array (Array Node)) <- for sectionData $ \section -> do
      generateSection k section { amount: level.amount, nodeTypes: level.nodeTypes}
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
  , quadrants :: Int
  , levels :: Int
  , amount :: Int
  , nodeTypes :: Array NodeType
  }

type BoardData = Array LevelData

boardData :: BoardData
boardData =
  [ { ampMin: 0
    , ampMax: 0
    , quadrants: 1
    , levels: 1
    , amount: 1
    , nodeTypes: [Start]
    }
  , { ampMin: 50
    , ampMax: 250
    , quadrants: 4
    , levels: 3
    , amount: 2
    , nodeTypes: [Start]
    }
  , { ampMin: 200
    , ampMax: 700
    , quadrants: 4
    , levels: 4
    , amount: 2
    , nodeTypes: [Start]
    }
  , { ampMin: 600
    , ampMax: 800
    , quadrants: 4
    , levels: 2
    , amount: 3
    , nodeTypes: [Start]
    }
  ]

type SectionData =
  { ampMin :: Int
  , ampMax :: Int
  , angleMin :: Int
  , angleMax :: Int
  }

generateSectionData :: forall r.
  { ampMin :: Int
  , ampMax :: Int
  , quadrants :: Int
  , levels :: Int
  | r } ->
  Array SectionData
generateSectionData { ampMin, ampMax, quadrants, levels } = do
  level <- enumFromTo 1 levels
  quadrant <- enumFromTo 1 quadrants
  let levelAmp = (ampMax - ampMin) / levels
  let quadrantSize = 360 / quadrants
  pure $
    { ampMin: ampMin + ((level - 1) * levelAmp)
    , ampMax: ampMin + (level * levelAmp)
    , angleMin: (quadrant - 1) * quadrantSize
    , angleMax: quadrant * quadrantSize
    }

generateSection :: forall f r.
  Monad f =>
  { nextId :: f Int
  , integerInRange :: Int -> Int -> f Int
  , chooseSet :: forall x. Array x -> f x
  | r } ->
  SectionData ->
  { amount :: Int, nodeTypes :: Array NodeType } ->
  f (Array Node)
generateSection k { ampMin, ampMax, angleMin, angleMax } { amount, nodeTypes } =
  for (enumFromTo 1 amount) $ \i -> do
    r <- k.integerInRange ampMin ampMax <#> toNumber
    φ <- k.integerInRange angleMin angleMax <#> toNumber >>> deg2Rad
    id <- k.nextId
    nodeType <- k.chooseSet nodeTypes
    pure (Node { id: id
               , x: r * Math.cos φ
               , y: r * Math.sin φ
               , nodeType: nodeType
               })

deg2Rad :: Number -> Number
deg2Rad x = x * Math.pi / 180.0
