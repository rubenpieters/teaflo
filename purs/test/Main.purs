module Test.Main where

import Prelude

import Client.Main

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log, logShow)

import Data.Map (Map)
import Data.Map as Map
import Data.Tuple

import Test.Assert
import Test.QuickCheck

connections1 :: Connections
connections1 = Map.fromFoldable
  [ Tuple 0 [{ to: { x: 1, y: 1, id: 1 } }, { to: { x: 2, y: 2, id: 2 } }]
  , Tuple 1 [{ to: { x: 10, y: 10, id: 10 } }]
  , Tuple 2 [{ to: { x: 11, y: 11, id: 11 } }]
  ]

connections1_e :: VerifyResult
connections1_e = VerifyResult
  { invalids: []
  , valids: [1, 2, 10, 11]
  }

main :: Eff _ Unit
main = do
  assert $ (verifyLinks connections1) == connections1_e
