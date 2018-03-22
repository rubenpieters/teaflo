module Client.Main (module Client.Main, module Exported) where

import Tupc (Pos(..)) as Exported
import Data.Maybe (Maybe(..)) as Exported

import Prelude

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

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (throw)

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq as Rep
import Data.Generic.Rep.Show as Rep

import Debug.Trace

tupcMap ::
  SubJsonConfigContent -> Eff _ (StrMap Pos)
tupcMap x = do
  map <- fromJson { throw: throw } x
  pure $ convert map
  where
    convert :: Map Char Pos -> StrMap Pos
    convert =
      (Map.toUnfoldable :: Map Char Pos -> Array (Tuple Char Pos)) >>>
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
    , "----------------"
    , "----1------1----"
    , "----------------"
    , "------2--2------"
    , "----------------"
    , "------2--2------"
    , "----------------"
    , "----------------"
    , "----------------"
    , "----------------"
    , "----------------"
    ]
  }

buttonMap :: Eff _ (StrMap Pos)
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

gameUiMap :: Eff _ (StrMap Pos)
gameUiMap = tupcMap gameUiMapConfig

--
--

type Bulb =
  { x :: Int
  , y :: Int
  , id :: Int
  }

centerId :: Int
centerId = 0

type Connection =
  { to :: Bulb
  , distance :: Number
  }

type Connections =
  Map Int (Array Connection)

initCxns :: Connections
initCxns = Map.empty

addLink :: { closest :: Bulb, furthest :: Bulb, distance :: Number } -> Connections -> Connections
addLink { closest, furthest, distance } cxns =
  cxns # Map.alter addLink' closest.id
  where
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
    in
      { result: f link <> result, nextIds: Array.cons link.to.id nextIds }
  Nothing -> { result: mempty, nextIds: [] }

type VerifyResultR =
  { valids :: Array Int
  , invalids :: Array Int
  }

newtype VerifyResult = VerifyResult VerifyResultR

derive instance newtypeVerifyResult :: Newtype VerifyResult _

derive instance genericVerifyResult :: Generic VerifyResult _
instance eqVerifyResult :: Eq VerifyResult where
  eq = Rep.genericEq
instance showVerifyResult :: Show VerifyResult where
  show = Rep.genericShow

instance semigroupVerifyResult :: Semigroup VerifyResult where
  append
    (VerifyResult { valids: valids1, invalids: invalids1 })
    (VerifyResult { valids: valids2, invalids: invalids2 })
    = (VerifyResult { valids: valids1 <> valids2, invalids: invalids1 <> invalids2 })

instance monoidVerifyResult :: Monoid VerifyResult where
  mempty = VerifyResult { valids: [], invalids: [] }

verifyLinks :: Connections -> VerifyResult
verifyLinks = traverseConnections f
  where
    f :: Connection -> VerifyResult
    f cxn = VerifyResult { valids: [cxn.to.id], invalids: [] }

type ResourceResult =
  { growth :: Number
  }

initialResources :: ResourceResult
initialResources = { growth: 100.0 }

calcResource :: Connections -> ResourceResult
calcResource cxns = un Endo (calcResource' cxns) initialResources

calcResource' :: Connections -> Endo ResourceResult
calcResource' = traverseConnections f
  where
    f :: Connection -> Endo ResourceResult
    f cxn = Endo (f' cxn)
    f' :: Connection -> ResourceResult -> ResourceResult
    f' { to, distance } { growth } = { growth: growth - distance }

{-
calcResources' ::
  Int ->
  Connections ->
  ResourceResult
calcResources' id cxns =
  case cxns # Map.lookup id of
    Just links ->
      let
        { result: result, nextIds: nextIds } = traverseLink links
      in
        result <> (nextIds # foldMap (\i -> traverseConnections' i cxns))
    Nothing -> mempty

calcResourcesLink ::
  Array Connection ->
  { result :: ResourceResult, nextIds :: Array Int }
calcResourcesLink links = case (Array.uncons links) of
  Just { head: link, tail: t } ->
    let
      { result: result, nextIds: nextIds } = traverseLink f t
    in
      { result: f link <> result, nextIds: Array.cons link.to.id nextIds }
  Nothing -> { result: mempty, nextIds: [] }
-}

verifyLinksJs :: Connections -> VerifyResultR
verifyLinksJs cxns = un VerifyResult (verifyLinks cxns)

data Color
  = White
  | Blue
  | Red
  | Green
  | Yellow

type Resource =
  { color :: Color
  , amount :: Int
  }

bulbType1 :: BulbType
bulbType1 = ResourceBulb
  { gain:
    { growth: 0.0
    , white: 1
    , blue: 0
    , red: 0
    , green: 0
    , yellow: 0
    }
  , cost:
    { growth: 0.0
    , white: 0
    , blue: 0
    , red: 0
    , green: 0
    , yellow: 0
    }
  }

bulbType2 :: BulbType
bulbType2 = ResourceBulb
  { gain:
    { growth: 0.0
    , white: 0
    , blue: 1
    , red: 0
    , green: 0
    , yellow: 0
    }
  , cost:
    { growth: 0.0
    , white: 2
    , blue: 0
    , red: 0
    , green: 0
    , yellow: 0
    }
  }

data BulbType
  = ResourceBulb
      { gain :: Resources
      , cost :: Resources
      }

type ResourcesR =
  ( growth :: Number
  , white :: Int
  , blue :: Int
  , red :: Int
  , green :: Int
  , yellow :: Int
  )

type Resources = Record ResourcesR

minus :: Resources -> Resources -> Resources
minus
  { growth: growth0, white: white0, blue: blue0, red: red0, green: green0, yellow: yellow0 }
  { growth: growth1, white: white1, blue: blue1, red: red1, green: green1, yellow: yellow1 }
  =
  { growth: growth0 - growth1
  , white: white0 - white1
  , blue: blue0 - blue1
  , red: red0 - red1
  , green: green0 - green1
  , yellow: yellow0 - yellow1
  }

plus :: Resources -> Resources -> Resources
plus
  { growth: growth0, white: white0, blue: blue0, red: red0, green: green0, yellow: yellow0 }
  { growth: growth1, white: white1, blue: blue1, red: red1, green: green1, yellow: yellow1 }
  =
  { growth: growth0 + growth1
  , white: white0 + white1
  , blue: blue0 + blue1
  , red: red0 + red1
  , green: green0 + green1
  , yellow: yellow0 + yellow1
  }

isValid :: Resources -> Boolean
isValid
  { growth: growth, white: white, blue: blue, red: red, green: green, yellow: yellow }
  =
  growth > 0.0 && white >= 0 && blue >= 0 && red >= 0 && green >= 0 && yellow >= 0

verifyCost ::
  Resources -> BulbType -> { canBuy :: Boolean, newResources :: Resources }
verifyCost res (ResourceBulb { gain, cost }) =
  { canBuy: checkResources, newResources: newResources `plus` gain }
  where
  newResources = res `minus` cost
  checkResources = newResources # isValid

main :: Eff _ Unit
main = pure unit
