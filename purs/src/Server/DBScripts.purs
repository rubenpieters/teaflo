module Server.DBScripts where

import Prelude

import Server.DB as DB

import Data.Foreign (Foreign)

import Control.Monad.Aff (launchAff_)
import Control.Monad.Eff (kind Effect, Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (log)

import Node.ReadLine (prompt, close, setLineHandler, setPrompt,  noCompletion, createConsoleInterface)

import Database.Postgres (DB, Query(..))
import Database.Postgres as PG

import Debug.Trace (traceAny)

main :: Eff _ Unit
main = do
  interface <- createConsoleInterface noCompletion
  setPrompt "> " 2 interface
  prompt interface
  setLineHandler interface $ \s ->
    if s == "quit"
      then close interface
      else do
        run s
        prompt interface

run :: String -> Eff _ Unit
run "setup" = setupDB
run cmd = log ("unknown command " <> cmd)

test :: Eff _ Unit
test = launchAff_ do
  connectionInfo <- liftEff $ DB.connectionInfo
  pool <- liftEff $ PG.mkPool connectionInfo
  PG.withClient pool $ \c -> do
    tables <- PG.queryValue_ (Query "select * from pg_catalog.pg_tables" :: Query Foreign) c
    traceAny tables (\_ -> pure unit)

setupDB :: Eff _ Unit
setupDB = launchAff_ do
  connectionInfo <- liftEff $ DB.connectionInfo
  pool <- liftEff $ PG.mkPool connectionInfo
  PG.withClient pool $ \c -> do
    PG.execute_ (Query setup) c

setup :: String
setup = """
 create table boards (
    id integer not null,
    boardJson jsonb
  )
  """
