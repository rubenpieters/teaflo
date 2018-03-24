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



main :: Eff _ Unit
main = do
  pure unit
