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
import Data.Foldable
import Data.Newtype

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

type Bulb =
  { x :: Int
  , y :: Int
  , id :: Int
  }

centerId :: Int
centerId = 0

type Connection =
  { to :: Bulb
  }

type Connections =
  Map Int (Array Connection)

initCxns :: Connections
initCxns = Map.empty

addLink :: { closest :: Bulb, furthest :: Bulb } -> Connections -> Connections
addLink { closest, furthest } cxns =
  cxns # Map.alter addLink' closest.id
  where
    newCxn = { to: furthest }
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

verifyLinksJs :: Connections -> VerifyResultR
verifyLinksJs cxns = un VerifyResult (verifyLinks cxns)

main :: Eff _ Unit
main = pure unit
