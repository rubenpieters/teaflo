module Shared.Node where

import Prelude

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

minus :: Resources -> Resources -> Resources
minus
  { growth: growth0, white: white0, blue: blue0, red: red0, green: green0, yellow: yellow0, vp: vp0 }
  { growth: growth1, white: white1, blue: blue1, red: red1, green: green1, yellow: yellow1, vp: vp1 }
  =
  { growth: growth0 - growth1
  , white: white0 - white1
  , blue: blue0 - blue1
  , red: red0 - red1
  , green: green0 - green1
  , yellow: yellow0 - yellow1
  , vp: vp0 - vp1
  }

plus :: Resources -> Resources -> Resources
plus
  { growth: growth0, white: white0, blue: blue0, red: red0, green: green0, yellow: yellow0, vp: vp0 }
  { growth: growth1, white: white1, blue: blue1, red: red1, green: green1, yellow: yellow1, vp: vp1 }
  =
  { growth: growth0 + growth1
  , white: white0 + white1
  , blue: blue0 + blue1
  , red: red0 + red1
  , green: green0 + green1
  , yellow: yellow0 + yellow1
  , vp: vp0 + vp1
  }

isValid :: Resources -> Boolean
isValid
  { growth, white, blue, red, green, yellow }
  =
  growth > 0.0 && white >= 0 && blue >= 0 && red >= 0 && green >= 0 && yellow >= 0

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

