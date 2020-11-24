{-# LANGUAGE GADTs #-}

module Database.Persist.Monad.Shim where

import Control.Monad.IO.Unlift (MonadUnliftIO)
import Data.Map (Map)
import Data.Text (Text)
import Data.Typeable (Typeable)
import Database.Persist
import Database.Persist.Sql

import Database.Persist.Monad.Class (MonadSqlQuery(..))
import Database.Persist.Monad.SqlQueryRep (SqlQueryRep(..))

{- PersistStoreRead -}

get :: (PersistRecordBackend record SqlBackend, Typeable record, MonadSqlQuery m) => Key record -> m (Maybe record)
get a = runQueryRep $ Get a

getMany :: (PersistRecordBackend record SqlBackend, Typeable record, MonadSqlQuery m) => [Key record] -> m (Map (Key record) record)
getMany a = runQueryRep $ GetMany a

getJust :: (PersistRecordBackend record SqlBackend, Typeable record, MonadSqlQuery m) => Key record -> m record
getJust a = runQueryRep $ GetJust a

getJustEntity :: (PersistRecordBackend record SqlBackend, Typeable record, MonadSqlQuery m) => Key record -> m (Entity record)
getJustEntity a = runQueryRep $ GetJustEntity a

getEntity :: (PersistRecordBackend record SqlBackend, Typeable record, MonadSqlQuery m) => Key record -> m (Maybe (Entity record))
getEntity a = runQueryRep $ GetEntity a

belongsTo :: (PersistEntity record1, PersistRecordBackend record2 SqlBackend, Typeable record1, Typeable record2, MonadSqlQuery m) => (record1 -> Maybe (Key record2)) -> record1 -> m (Maybe record2)
belongsTo a b = runQueryRep $ BelongsTo a b

belongsToJust :: (PersistEntity record1, PersistRecordBackend record2 SqlBackend, Typeable record1, Typeable record2, MonadSqlQuery m) => (record1 -> Key record2) -> record1 -> m record2
belongsToJust a b = runQueryRep $ BelongsToJust a b

{- Other -}

selectList :: (PersistRecordBackend record SqlBackend, Typeable record, MonadSqlQuery m) => [Filter record] -> [SelectOpt record] -> m [Entity record]
selectList a b = runQueryRep $ SelectList a b

insert :: (PersistRecordBackend record SqlBackend, Typeable record, MonadSqlQuery m) => record -> m (Key record)
insert a = runQueryRep $ Insert a

insert_ :: (PersistRecordBackend record SqlBackend, Typeable record, MonadSqlQuery m) => record -> m ()
insert_ a = runQueryRep $ Insert_ a

runMigrationSilent :: (MonadUnliftIO m, MonadSqlQuery m) => Migration -> m [Text]
runMigrationSilent a = runQueryRep $ RunMigrationsSilent a
