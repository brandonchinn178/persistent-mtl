{-# LANGUAGE OverloadedStrings #-}

module TestUtils.DB
  ( BackendType(..)
  , allBackendTypes
  , withTestDB
  ) where

import Control.Monad.Logger (runNoLoggingT)
import qualified Data.ByteString.Char8 as Char8
import Data.Maybe (fromMaybe)
import Data.Pool (Pool)
import qualified Data.Text as Text
import Database.Persist.Postgresql (withPostgresqlPool)
import Database.Persist.Sql (SqlBackend, rawExecute, runSqlPool)
import Database.Persist.Sqlite (withSqlitePool)
import System.Environment (lookupEnv)
import System.IO.Unsafe (unsafePerformIO)
import UnliftIO (liftIO, withSystemTempDirectory)

data BackendType = Sqlite | Postgresql
  deriving (Show)

allBackendTypes :: [BackendType]
allBackendTypes = Sqlite : [Postgresql | isEnabled "TEST_POSTGRESQL"]
  where
    isEnabled envVarName = unsafePerformIO (lookupEnv envVarName) == Just "1"

withTestDB :: BackendType -> (Pool SqlBackend -> IO a) -> IO a
withTestDB Sqlite = withTestDBSqlite
withTestDB Postgresql = withTestDBPostgresql

withTestDBSqlite :: (Pool SqlBackend -> IO a) -> IO a
withTestDBSqlite f =
  withSystemTempDirectory "persistent-mtl-testapp" $ \dir -> do
    let db = Text.pack $ dir ++ "/db.sqlite"
    runNoLoggingT $ withSqlitePool db 5 $ \pool -> liftIO $ f pool

-- Requires running database, with connection string specified in POSTGRESQL_URL
withTestDBPostgresql :: (Pool SqlBackend -> IO a) -> IO a
withTestDBPostgresql f = do
  url <- fromMaybe defaultUrl <$> lookupEnv "POSTGRESQL_URL"
  runNoLoggingT $ withPostgresqlPool (Char8.pack url) 5 $ \pool -> do
    (`runSqlPool` pool) $
      rawExecute "DROP SCHEMA public CASCADE; CREATE SCHEMA public;" []

    liftIO $ f pool
  where
    defaultUrl = "postgresql://postgres@localhost/persistent_mtl"
