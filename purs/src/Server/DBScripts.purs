module Server.DBScripts where

import Prelude

import Shared.Board
import Shared.Node
import Server.DB as DB

import Data.Foreign (Foreign)

import Control.Monad.Aff (Aff, launchAff_)
import Control.Monad.Eff (kind Effect, Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (log)
import Control.Monad.Eff.Exception (EXCEPTION)

import Data.Argonaut.Decode.Class (decodeJson)
import Data.Argonaut.Encode.Class (encodeJson)
import Data.Argonaut.Core (stringify)

import Node.ReadLine (prompt, close, setLineHandler, setPrompt,  noCompletion, createConsoleInterface)

import Node.FS (FS)
import Database.Postgres (DB, Client, Query(..))
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

testBoard :: Board
testBoard = Board
  { nodes:
    [ Node { id: 0, x: 0.0, y: 0.0, nodeType: Start }
    , Node { id: 1, x: 4.0, y: 4.0, nodeType: ResourceNode
        { gain: Resources
          { growth: 0.0
          , white: 0
          , blue: 0
          , red: 0
          , green: 1
          , yellow: 0
          }
        , cost: Resources
          { growth: 0.0
          , white: 0
          , blue: 0
          , red: 0
          , green: 0
          , yellow: 0
          }
        , name: "green"
        }}
    ]
  }

run :: String -> Eff _ Unit
run "setup" = setupDB
run "test1" = log (stringify (encodeJson testBoard))
run "test2" = onPostgres (\c -> c # DB.putBoard { id: 1, board: testBoard })
run "test3" = onPostgres (\c -> c # PG.query_ (Query ("select boardJson from boards where id = 1") :: Query Foreign) >>= \x -> traceAny x (\_ -> pure unit))
run "test4" = onPostgres (\c -> c # DB.getBoard { id: 1 } >>= \x -> liftEff $ log (show x))
run cmd = log ("unknown command " <> cmd)

onPostgres :: forall eff a.
  (Client -> Aff (db :: DB, exception :: EXCEPTION, fs :: FS | eff) Unit) ->
  Eff (db :: DB, exception :: EXCEPTION, fs :: FS | eff) Unit
onPostgres aff = launchAff_ do
  connectionInfo <- liftEff $ DB.connectionInfo
  pool <- liftEff $ PG.mkPool connectionInfo
  PG.withClient pool $ \c -> aff c

    --tables <- PG.queryValue_ (Query "select * from pg_catalog.pg_tables" :: Query Foreign) c
    --traceAny tables (\_ -> pure unit)

setupDB :: Eff _ Unit
setupDB = do
  onPostgres $ \c -> do
    PG.execute_ (Query setup) c

setup :: String
setup = """
 create table boards (
    id integer not null,
    boardJson jsonb
  )
  """
