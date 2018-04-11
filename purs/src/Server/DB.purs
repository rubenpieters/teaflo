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
import Control.Monad.Eff.Console (log)
import Control.Monad.Aff (Aff)

import Data.Argonaut.Decode.Class (class DecodeJson, decodeJson)
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
  { boardId :: Int
  } ->
  Client ->
  Aff _ (Array { vp :: Int, solutionId :: Int })
getCurrentTop { boardId } c = do
  let (query :: Query Foreign) = Query $
    "select vp, id from solutions" <>
    " where boardId = " <> show boardId <>
    " order by vp desc limit 3"
--  dbTop <- c # PG.query_ query
  let (transform :: Foreign -> { vp :: Int, solutionId :: Int }) = unsafeCoerce
  dbResult <- c # PG.query_ query
  let transformed = dbResult <#> transform
  pure transformed

submitSolution ::
  { solutionId :: Int
  , boardId :: Int
  , vp :: Int
  , solution :: Solution
  } ->
  Client ->
  Aff _ Unit
submitSolution { solutionId, boardId, vp, solution } c = do
  let query = Query $
    "insert into solutions values (" <> show solutionId <>
    ", " <> show boardId <>
    ", " <> show vp <>
    ", '" <> stringify (encodeJson solution) <>
    "')"
  c # PG.execute_ query

jsonFromForeign :: forall a.
    (DecodeJson a) => Foreign -> Either String a
jsonFromForeign obj = do
  -- representation stored in DB is Argonaut Json
  let (json :: Json) = unsafeCoerce obj
  decodeJson json

putBoard ::
  { id :: Int, board :: Board } ->
  Client ->
  Aff _ Unit
putBoard { id, board } c = do
  let query = Query $
    "insert into boards values (" <> show id <>
    ", '" <> stringify (encodeJson board) <>
    "')"
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
    Just x -> x # jsonFromForeign

--------------------------------------
-- Connection Info
--------------------------------------

parseClientConfig :: String -> Eff _ ClientConfig
parseClientConfig s = case readJSON s of
  Left err -> throw (show err)
  Right config -> pure config

connectionInfo :: String -> Eff _ ConnectionInfo
connectionInfo credentialsPath = do
  credentialContents <- readTextFile UTF8 credentialsPath
  clientConfig <- parseClientConfig credentialContents
  pure $ PG.connectionInfoFromConfig clientConfig PG.defaultPoolConfig
