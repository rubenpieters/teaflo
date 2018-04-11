module Server.DBScripts where

import Prelude

import Shared.Board
import Shared.Node
import Shared.Solution
import Server.DB as DB

import Data.Foreign (Foreign)
import Data.Map (Map(..))
import Data.Map (fromFoldable)
import Data.Tuple (Tuple(..))

import Control.Monad.Eff (kind Effect, Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (log)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Aff (Aff, launchAff_)

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

testSolution :: Number -> Solution
testSolution x = Solution $
  fromFoldable
    [ Tuple 0 [Connection { to: Node { id: 1, x: 4.0, y: 4.0, nodeType: Start }, distance: x }]
    ]

run :: String -> Eff _ Unit
run "setup" = setupDB
run "test1" = log (stringify (encodeJson testBoard))
run "test2" = onPostgres (\c -> c # DB.putBoard { id: 1, board: testBoard })
run "test3" = onPostgres (\c -> c # PG.query_ (Query ("select boardJson from boards where id = 1") :: Query Foreign) >>= \x -> traceAny x (\_ -> pure unit))
run "test4" = onPostgres (\c -> c # DB.getBoard { id: 1 } >>= \x -> liftEff $ log (show x))
run "test5" = onPostgres (\c -> c # DB.submitSolution { solutionId: 1, boardId: 1, vp: 1, solution: testSolution 1.0 })
run "test6" = onPostgres (\c -> c # DB.submitSolution { solutionId: 1, boardId: 1, vp: 2, solution: testSolution 2.0 })
run "test7" = onPostgres (\c -> c # DB.submitSolution { solutionId: 1, boardId: 1, vp: 3, solution: testSolution 3.0 })
run "test8" = onPostgres (\c -> c # DB.submitSolution { solutionId: 1, boardId: 1, vp: 4, solution: testSolution 4.0 })
run "test9" = onPostgres (\c -> c # PG.query_ (Query ("select vp, solutionJson from solutions where boardId = 1") :: Query Foreign) >>= \x -> traceAny x (\_ -> pure unit))
run "test10" = onPostgres (\c -> c # DB.getCurrentTop { boardId: 1 } >>= \x -> traceAny x (\_ -> pure unit))
run cmd = log ("unknown command " <> cmd)

onPostgres :: forall eff a.
  (Client -> Aff (db :: DB, exception :: EXCEPTION, fs :: FS | eff) Unit) ->
  Eff (db :: DB, exception :: EXCEPTION, fs :: FS | eff) Unit
onPostgres aff = launchAff_ do
  connectionInfo <- liftEff $ DB.connectionInfo "./credentials-heroku.json"
  pool <- liftEff $ PG.mkPool connectionInfo
  PG.withClient pool $ \c -> aff c

    --tables <- PG.queryValue_ (Query "select * from pg_catalog.pg_tables" :: Query Foreign) c
    --traceAny tables (\_ -> pure unit)

setupDB :: Eff _ Unit
setupDB = do
  onPostgres $ \c -> do
    -- PG.execute_ (Query setupBoardsTable) c
    PG.execute_ (Query setupSolutionsTable) c

setupBoardsTable :: String
setupBoardsTable = """
 create table boards (
    id integer not null,
    boardJson jsonb
  )
  """

setupSolutionsTable :: String
setupSolutionsTable = """
  create table solutions (
    id integer not null,
    boardId integer not null,
    vp integer not null,
    solutionJson jsonb
  )
  """
