module Server.DB where

import Prelude

import Shared.Solution

import Data.Either (Either(..))

import Control.Monad.Eff
import Control.Monad.Eff.Exception (throw)

import Simple.JSON (readJSON)

import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)

import Database.Postgres (ClientConfig, ConnectionInfo)
import Database.Postgres as PG

getCurrentTop ::
  Eff _ { top :: Array Int }
getCurrentTop = pure { top: [1,2,3] }

submitSolution ::
  { boardId :: Int
  , solution :: Solution Verified
  } ->
  Eff _ Unit
submitSolution { boardId, solution } = pure unit

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
