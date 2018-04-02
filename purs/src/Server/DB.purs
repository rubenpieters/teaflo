module Server.DB where

import Prelude

import Control.Monad.Eff

getCurrentTop ::
  Eff _ { top :: Array Int }
getCurrentTop = pure { top: [1,2,3] }
