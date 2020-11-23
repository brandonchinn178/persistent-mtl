{-# LANGUAGE GADTs #-}

module Database.Persist.Monad.Shim where

import Control.Monad.IO.Unlift (MonadUnliftIO)
import Data.Text (Text)
import Data.Typeable (Typeable)
import Database.Persist
import Database.Persist.Sql

import Database.Persist.Monad.Class (MonadSqlQuery(..))
import Database.Persist.Monad.SqlQueryRep (SqlQueryRep(..))

selectList :: (PersistRecordBackend record SqlBackend, Typeable record, MonadSqlQuery m) => [Filter record] -> [SelectOpt record] -> m [Entity record]
selectList a b = runQueryRep $ SelectList a b

insert :: (PersistRecordBackend record SqlBackend, Typeable record, MonadSqlQuery m) => record -> m (Key record)
insert a = runQueryRep $ Insert a

insert_ :: (PersistRecordBackend record SqlBackend, Typeable record, MonadSqlQuery m) => record -> m ()
insert_ a = runQueryRep $ Insert_ a

runMigrationSilent :: (MonadUnliftIO m, MonadSqlQuery m) => Migration -> m [Text]
runMigrationSilent a = runQueryRep $ RunMigrationsSilent a
