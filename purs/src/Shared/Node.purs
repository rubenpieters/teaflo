module Shared.Node where

import Prelude

import Data.SubRecord

import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Decode.Generic.Rep (genericDecodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson)
import Data.Argonaut.Encode.Generic.Rep (genericEncodeJson)
import Data.Generic.Rep as Rep
import Data.Generic.Rep.Show (genericShow)
import Data.Generic.Rep.Eq (genericEq)

type ResourcesR =
  ( growth :: Number
  , white :: Int
  , blue :: Int
  , red :: Int
  , green :: Int
  , yellow :: Int
  )

newtype Resources = Resources (Record ResourcesR)

derive instance genericResources :: Rep.Generic Resources _
instance encodeJsonResources :: EncodeJson Resources
  where encodeJson = genericEncodeJson
instance decodeJsonResources :: DecodeJson Resources
  where decodeJson = genericDecodeJson
instance showResources :: Show Resources
  where show = genericShow
instance eqResources :: Eq Resources
  where eq = genericEq

minus :: Resources -> Resources -> Resources
minus
  (Resources { growth: growth0, white: white0, blue: blue0, red: red0, green: green0, yellow: yellow0 })
  (Resources { growth: growth1, white: white1, blue: blue1, red: red1, green: green1, yellow: yellow1 })
  = Resources
  { growth: growth0 - growth1
  , white: white0 - white1
  , blue: blue0 - blue1
  , red: red0 - red1
  , green: green0 - green1
  , yellow: yellow0 - yellow1
  }

plus :: Resources -> Resources -> Resources
plus
  (Resources { growth: growth0, white: white0, blue: blue0, red: red0, green: green0, yellow: yellow0 })
  (Resources { growth: growth1, white: white1, blue: blue1, red: red1, green: green1, yellow: yellow1 })
  = Resources
  { growth: growth0 + growth1
  , white: white0 + white1
  , blue: blue0 + blue1
  , red: red0 + red1
  , green: green0 + green1
  , yellow: yellow0 + yellow1
  }

times :: Resources -> Resources -> Resources
times
  (Resources { growth: growth0, white: white0, blue: blue0, red: red0, green: green0, yellow: yellow0 })
  (Resources { growth: growth1, white: white1, blue: blue1, red: red1, green: green1, yellow: yellow1 })
  = Resources
  { growth: growth0 * growth1
  , white: white0 * white1
  , blue: blue0 * blue1
  , red: red0 * red1
  , green: green0 * green1
  , yellow: yellow0 * yellow1
  }

sumColors :: Resources -> Int
sumColors (Resources { growth, white, blue, red, green, yellow }) =
  white + blue + red + green + yellow

isValid :: Resources -> Boolean
isValid
  (Resources { growth, white, blue, red, green, yellow })
  =
  growth > 0.0 && white >= 0 && blue >= 0 && red >= 0 && green >= 0 && yellow >= 0

defaultResources :: Record ResourcesR
defaultResources =
  { growth: 0.0
  , white: 0
  , blue: 0
  , red: 0
  , green: 0
  , yellow: 0
  }

data NodeType
  = Start
  | ResourceNode
      { gain :: Resources
      , cost :: Resources
      , name :: String
      }
  | VictoryNode
      { vp :: Resources
      , cost :: Resources
      , name :: String
      }

derive instance genericNodeType :: Rep.Generic NodeType _
instance encodeJsonNodeType :: EncodeJson NodeType
  where encodeJson = genericEncodeJson
instance decodeJsonNodeType :: DecodeJson NodeType
  where decodeJson = genericDecodeJson
instance showNodeType :: Show NodeType
  where show = genericShow
instance eqNodeType :: Eq NodeType
  where eq = genericEq

nodeGain :: NodeType -> Record ResourcesR
nodeGain Start = defaultResources
nodeGain (ResourceNode { gain: (Resources gain) }) = gain
nodeGain (VictoryNode _) = defaultResources

type ResultR =
  ( totals :: Resources
  )

type Result = Record ResultR

basicNode :: Int -> NodeType
basicNode x = ResourceNode
  { gain: Resources $ withDefaults defaultResources (mkSubRecord { white: x })
  , cost: Resources $ defaultResources
  , name: "basic"
  }

blueNode :: Int -> NodeType
blueNode x = ResourceNode
  { gain: Resources $ withDefaults defaultResources (mkSubRecord { blue: x })
  , cost: Resources $ withDefaults defaultResources (mkSubRecord { white: 2 })
  , name: "blue"
  }

redNode :: Int -> NodeType
redNode x = ResourceNode
  { gain: Resources $ withDefaults defaultResources (mkSubRecord { red: x })
  , cost: Resources $ withDefaults defaultResources (mkSubRecord { white: 2 })
  , name: "red"
  }

greenNode :: Int -> NodeType
greenNode x = ResourceNode
  { gain: Resources $ withDefaults defaultResources (mkSubRecord { green: x })
  , cost: Resources $ withDefaults defaultResources (mkSubRecord { white: 2 })
  , name: "green"
  }

yellowNode :: Int -> NodeType
yellowNode x = ResourceNode
  { gain: Resources $ withDefaults defaultResources (mkSubRecord { yellow: x })
  , cost: Resources $ withDefaults defaultResources (mkSubRecord { white: 2 })
  , name: "yellow"
  }

