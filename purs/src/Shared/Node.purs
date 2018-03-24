module Shared.Node where

import Data.SubRecord

type ResourcesR =
  ( growth :: Number
  , white :: Int
  , blue :: Int
  , red :: Int
  , green :: Int
  , yellow :: Int
  , vp :: Int
  )

type Resources = Record ResourcesR

defaultResources :: Resources
defaultResources =
  { growth: 0.0
  , white: 0
  , blue: 0
  , red: 0
  , green: 0
  , yellow: 0
  , vp: 0
  }

data NodeType
  = Start
  | ResourceNode
      { gain :: Resources
      , cost :: Resources
      , name :: String
      }

basicNode :: Int -> NodeType
basicNode x = ResourceNode
  { gain: withDefaults defaultResources (mkSubRecord { white: x })
  , cost: defaultResources
  , name: "basic"
  }

blueNode :: Int -> NodeType
blueNode x = ResourceNode
  { gain: withDefaults defaultResources (mkSubRecord { blue: x })
  , cost: withDefaults defaultResources (mkSubRecord { white: 2 })
  , name: "blue"
  }

redNode :: Int -> NodeType
redNode x = ResourceNode
  { gain: withDefaults defaultResources (mkSubRecord { red: x })
  , cost: withDefaults defaultResources (mkSubRecord { white: 2 })
  , name: "red"
  }

greenNode :: Int -> NodeType
greenNode x = ResourceNode
  { gain: withDefaults defaultResources (mkSubRecord { green: x })
  , cost: withDefaults defaultResources (mkSubRecord { white: 2 })
  , name: "green"
  }

yellowNode :: Int -> NodeType
yellowNode x = ResourceNode
  { gain: withDefaults defaultResources (mkSubRecord { yellow: x })
  , cost: withDefaults defaultResources (mkSubRecord { white: 2 })
  , name: "yellow"
  }

