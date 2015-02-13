{-# LANGUAGE TypeOperators, DataKinds, TemplateHaskell #-}

module Cloud.Azure.Storage.Table
    ( runStorageTable
    , tables
    , TableName(..)
    ) where

import Data.Aeson (FromJSON, Result(Success, Error))
import qualified Data.Aeson as Aeson
import Data.Aeson.TH (deriveFromJSON, defaultOptions)
import Data.ByteString (ByteString)
import Data.Monoid ((<>))
import qualified Data.UnixTime as UnixTime
import Network.Http.Client (Method(..))
import qualified Network.Http.Client as Http
import qualified System.IO.Streams.Attoparsec as SA

import Cloud.Azure.Storage.Aeson (azureOptions)
import Cloud.Azure.Storage.Core

data TableOperation a = TableOperation
    { method :: Method
    , resource :: ByteString
    }

newtype TableName = TableName { tableName :: String }
  deriving (Show)

deriveFromJSON azureOptions ''TableName

tables :: TableOperation TableName
tables = TableOperation
    GET
    "/Tables"

result :: (String -> r) -> (a -> r) -> Result a -> r
result f _ (Error msg) = f msg
result _ f (Success a) = f a

newtype Response a = Response { value :: [a] }

deriveFromJSON defaultOptions ''Response

runStorageTable :: FromJSON a
    => StorageAccount
    -> TableOperation a
    -> IO [a]
runStorageTable acc op = do
    let host = accountName acc <> ".table.core.windows.net"
    withConnection host 443 $ \conn -> do
        time <- UnixTime.getUnixTime
        req <- Http.buildRequest $ requestForTable
                (accountName acc)
                (accountKey acc)
                host
                (method op)
                Nothing
                (Just "application/json")
                time
                (resource op)

        Http.sendRequest conn req Http.emptyBody
        Http.receiveResponse conn $ \_res i -> do
            SA.parseFromStream Aeson.json i >>=
                result fail (return . value) . Aeson.fromJSON

