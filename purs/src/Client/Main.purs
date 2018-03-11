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

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (throw)

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
    --- 1 2 3 4 5 6 7 8 9
    [ "------------------"
    , "------------------"
    , "------------------"
    , "------------------"
    , "------1----1------"
    , "------------------"
    , "------------------"
    , "------------------"
    , "------------------"
    , "------------------"
    , "------------------"
    , "------------------"
    ]
  }

buttonMap :: Eff _ (StrMap Pos)
buttonMap = tupcMap buttonMapConfig

main :: Eff _ Unit
main = pure unit
