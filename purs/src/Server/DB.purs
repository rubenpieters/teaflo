module Server.DB where

import Prelude

import Shared.Board
import Shared.Solution

import Data.Foreign (Foreign)

import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Data.Foreign.Class (class Decode, decode)
import Data.Foreign.Index (readProp)
import Data.Bifunctor (lmap)

import Control.Monad.Except (runExcept)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (throw)
import Control.Monad.Aff (Aff)

import Data.Argonaut.Decode.Class (decodeJson)
import Data.Argonaut.Encode.Class (encodeJson)
import Data.Argonaut.Core (Json, stringify)
import Data.Argonaut.Parser (jsonParser)

import Simple.JSON (readJSON)

import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)

import Database.Postgres (Client, ClientConfig, ConnectionInfo, Query(..))
import Database.Postgres as PG

import Debug.Trace (traceAny)

import Unsafe.Coerce (unsafeCoerce)

getCurrentTop ::
  Eff _ { top :: Array Int }
getCurrentTop = pure { top: [1,2,3] }

submitSolution ::
  { boardId :: Int
  , solution :: Solution Verified
  } ->
  Eff _ Unit
submitSolution { boardId, solution } = pure unit

newtype DBBoard = DBBoard
  { id :: Int
  , boardJson :: String
  }

instance decodeDBBoard :: Decode DBBoard where
  decode obj = do
    traceAny obj $ (\_ -> pure unit)
    id <- decode =<< readProp "id" obj
    boardJson <- decode =<< readProp "boardJson" obj
    pure $ DBBoard { id: id, boardJson: boardJson }

toBoard :: Foreign -> Either String Board
toBoard obj = do
  -- representation stored in DB is Argonaut Json
  let (boardJson :: Json) = unsafeCoerce obj
  decodedJson <- decodeJson boardJson
  pure decodedJson

--toBoard :: Foreign -> Either String Board
--toBoard obj =
{-  traceAny obj $ (\_ -> pure unit)
  boardJson <- lmap show $ runExcept $ decode obj
  traceAny boardJson $ (\_ -> pure unit)
  parsedJson <- jsonParser boardJson
  traceAny parsedJson $ (\_ -> pure unit)
  decodedJson <- decodeJson parsedJson
  traceAny decodedJson $ (\_ -> pure unit)
  pure decodedJson
-}

putBoard ::
  { id :: Int, board :: Board } ->
  Client ->
  Aff _ Unit
putBoard { id, board } c = do
  let query = Query $ "insert into boards values (" <> show id <> ", '" <> stringify (encodeJson board) <> "')"
  c # PG.execute_ query

{-
getBoards ::
  Client ->
  Aff _ (Array (Either String { id :: Int, board :: Board }))
getBoards c = do
  let (query :: Query Foreign) = Query $ "select id, boardJson from boards"
  dbBoards <- c # PG.query_ query
  pure (dbBoards <#> toBoard)
-}

getBoard ::
  { id :: Int } ->
  Client ->
  Aff _ (Either String Board)
getBoard { id } c = do
  let (query :: Query Foreign) = Query $ "select boardJson from boards where id = " <> show id
  dbBoard <- c # PG.queryValue_ query
  pure $ case dbBoard of
    Nothing -> Left $ "No board for id " <> show id
    Just x -> x # toBoard

--------------------------------------
-- Connection Info
--------------------------------------

parseClientConfig :: String -> Eff _ ClientConfig
parseClientConfig s = case readJSON s of
  Left err -> throw (show err)
  Right config -> pure config

connectionInfo :: Eff _ ConnectionInfo
connectionInfo = do
  credentialContents <- readTextFile UTF8 "./credentials-heroku.json"
  clientConfig <- parseClientConfig credentialContents
  pure $ PG.connectionInfoFromConfig clientConfig PG.defaultPoolConfig
