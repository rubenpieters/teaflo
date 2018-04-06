module Server.ExperimentMain where

import Prelude

import Server.DB as DB

import Data.Foreign (Foreign)

import Control.Monad.Aff (launchAff_)
import Control.Monad.Eff (kind Effect, Eff)
import Control.Monad.Eff.Class (liftEff)

import Database.Postgres (DB, Query(..))
import Database.Postgres as PG

import Debug.Trace (traceAny)

main :: Eff _ Unit
main = launchAff_ $ do
  connectionInfo <- liftEff $ DB.connectionInfo
  pool <- liftEff $ PG.mkPool connectionInfo
  PG.withClient pool $ \c -> do
    tables <- PG.queryValue_ (Query "select * from pg_catalog.pg_tables" :: Query Foreign) c
    traceAny tables (\_ -> pure unit)
