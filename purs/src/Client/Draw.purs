module Client.Draw where

import Prelude

import Data.Traversable (for, for_)

import Shared.Node
import Shared.Board

import Control.Monad.Eff.Exception.Unsafe (unsafeThrow)

drawBoard :: forall f r.
  Monad f =>
  { drawNode :: Node -> f Unit
  | r } ->
  Board ->
  f Unit
drawBoard k (Board { nodes }) =
  for_ nodes k.drawNode

nodeColor :: NodeType -> Int
nodeColor Start = 0xFFFFFF
nodeColor (ResourceNode { name }) | name == "basic" = 0x666666
nodeColor (ResourceNode { name }) | name == "blue" = 0x1289A7
nodeColor (ResourceNode { name }) | name == "red" = 0xEA2027
nodeColor (ResourceNode { name }) | name == "green" = 0x009432
nodeColor (ResourceNode { name }) | name == "yellow" = 0xFFC312
nodeColor _ = 0x000000
