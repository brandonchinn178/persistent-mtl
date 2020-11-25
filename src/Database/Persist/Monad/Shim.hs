{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}

module Database.Persist.Monad.Shim where

import Data.Conduit (ConduitM)
import Data.Int (Int64)
import Data.Map (Map)
import Data.Text (Text)
import Data.Typeable (Typeable)
import Data.Void (Void)
import Database.Persist.Sql hiding (pattern Update)
import GHC.Stack (HasCallStack)

import Database.Persist.Monad.Class (MonadSqlQuery(..))
import Database.Persist.Monad.SqlQueryRep (SqlQueryRep(..))

{-# ANN module "HLint: ignore" #-}

get
  :: (PersistRecordBackend record SqlBackend, Typeable record, MonadSqlQuery m)
  => Key record -> m (Maybe record)
get a1 = runQueryRep $ Get a1

getMany
  :: (PersistRecordBackend record SqlBackend, Typeable record, MonadSqlQuery m)
  => [Key record] -> m (Map (Key record) record)
getMany a1 = runQueryRep $ GetMany a1

getJust
  :: (PersistRecordBackend record SqlBackend, Typeable record, MonadSqlQuery m)
  => Key record -> m record
getJust a1 = runQueryRep $ GetJust a1

getJustEntity
  :: (PersistRecordBackend record SqlBackend, Typeable record, MonadSqlQuery m)
  => Key record -> m (Entity record)
getJustEntity a1 = runQueryRep $ GetJustEntity a1

getEntity
  :: (PersistRecordBackend record SqlBackend, Typeable record, MonadSqlQuery m)
  => Key record -> m (Maybe (Entity record))
getEntity a1 = runQueryRep $ GetEntity a1

belongsTo
  :: (PersistEntity record1, PersistRecordBackend record2 SqlBackend, Typeable record1, Typeable record2, MonadSqlQuery m)
  => (record1 -> Maybe (Key record2)) -> record1 -> m (Maybe record2)
belongsTo a1 a2 = runQueryRep $ BelongsTo a1 a2

belongsToJust
  :: (PersistEntity record1, PersistRecordBackend record2 SqlBackend, Typeable record1, Typeable record2, MonadSqlQuery m)
  => (record1 -> Key record2) -> record1 -> m record2
belongsToJust a1 a2 = runQueryRep $ BelongsToJust a1 a2

insert
  :: (PersistRecordBackend record SqlBackend, Typeable record, MonadSqlQuery m)
  => record -> m (Key record)
insert a1 = runQueryRep $ Insert a1

insert_
  :: (PersistRecordBackend record SqlBackend, Typeable record, MonadSqlQuery m)
  => record -> m ()
insert_ a1 = runQueryRep $ Insert_ a1

insertMany
  :: (PersistRecordBackend record SqlBackend, Typeable record, MonadSqlQuery m)
  => [record] -> m [Key record]
insertMany a1 = runQueryRep $ InsertMany a1

insertMany_
  :: (PersistRecordBackend record SqlBackend, Typeable record, MonadSqlQuery m)
  => [record] -> m ()
insertMany_ a1 = runQueryRep $ InsertMany_ a1

insertEntityMany
  :: (PersistRecordBackend record SqlBackend, Typeable record, MonadSqlQuery m)
  => [Entity record] -> m ()
insertEntityMany a1 = runQueryRep $ InsertEntityMany a1

insertKey
  :: (PersistRecordBackend record SqlBackend, Typeable record, MonadSqlQuery m)
  => Key record -> record -> m ()
insertKey a1 a2 = runQueryRep $ InsertKey a1 a2

repsert
  :: (PersistRecordBackend record SqlBackend, Typeable record, MonadSqlQuery m)
  => Key record -> record -> m ()
repsert a1 a2 = runQueryRep $ Repsert a1 a2

repsertMany
  :: (PersistRecordBackend record SqlBackend, Typeable record, MonadSqlQuery m)
  => [(Key record, record)] -> m ()
repsertMany a1 = runQueryRep $ RepsertMany a1

replace
  :: (PersistRecordBackend record SqlBackend, Typeable record, MonadSqlQuery m)
  => Key record -> record -> m ()
replace a1 a2 = runQueryRep $ Replace a1 a2

delete
  :: (PersistRecordBackend record SqlBackend, Typeable record, MonadSqlQuery m)
  => Key record -> m ()
delete a1 = runQueryRep $ Delete a1

update
  :: (PersistRecordBackend record SqlBackend, Typeable record, MonadSqlQuery m)
  => Key record -> [Update record] -> m ()
update a1 a2 = runQueryRep $ Update a1 a2

updateGet
  :: (PersistRecordBackend record SqlBackend, Typeable record, MonadSqlQuery m)
  => Key record -> [Update record] -> m record
updateGet a1 a2 = runQueryRep $ UpdateGet a1 a2

insertEntity
  :: (PersistRecordBackend record SqlBackend, Typeable record, MonadSqlQuery m)
  => record -> m (Entity record)
insertEntity a1 = runQueryRep $ InsertEntity a1

insertRecord
  :: (PersistRecordBackend record SqlBackend, Typeable record, MonadSqlQuery m)
  => record -> m record
insertRecord a1 = runQueryRep $ InsertRecord a1

getBy
  :: (PersistRecordBackend record SqlBackend, Typeable record, MonadSqlQuery m)
  => Unique record -> m (Maybe (Entity record))
getBy a1 = runQueryRep $ GetBy a1

getByValue
  :: (PersistRecordBackend record SqlBackend, AtLeastOneUniqueKey record, Typeable record, MonadSqlQuery m)
  => record -> m (Maybe (Entity record))
getByValue a1 = runQueryRep $ GetByValue a1

checkUnique
  :: (PersistRecordBackend record SqlBackend, Typeable record, MonadSqlQuery m)
  => record -> m (Maybe (Unique record))
checkUnique a1 = runQueryRep $ CheckUnique a1

#if MIN_VERSION_persistent(2,11,0)
checkUniqueUpdateable
  :: (PersistRecordBackend record SqlBackend, Typeable record, MonadSqlQuery m)
  => Entity record -> m (Maybe (Unique record))
checkUniqueUpdateable a1 = runQueryRep $ CheckUniqueUpdateable a1
#endif

deleteBy
  :: (PersistRecordBackend record SqlBackend, Typeable record, MonadSqlQuery m)
  => Unique record -> m ()
deleteBy a1 = runQueryRep $ DeleteBy a1

insertUnique
  :: (PersistRecordBackend record SqlBackend, Typeable record, MonadSqlQuery m)
  => record -> m (Maybe (Key record))
insertUnique a1 = runQueryRep $ InsertUnique a1

upsert
  :: (PersistRecordBackend record SqlBackend, OnlyOneUniqueKey record, Typeable record, MonadSqlQuery m)
  => record -> [Update record] -> m (Entity record)
upsert a1 a2 = runQueryRep $ Upsert a1 a2

upsertBy
  :: (PersistRecordBackend record SqlBackend, Typeable record, MonadSqlQuery m)
  => Unique record -> record -> [Update record] -> m (Entity record)
upsertBy a1 a2 a3 = runQueryRep $ UpsertBy a1 a2 a3

putMany
  :: (PersistRecordBackend record SqlBackend, Typeable record, MonadSqlQuery m)
  => [record] -> m ()
putMany a1 = runQueryRep $ PutMany a1

insertBy
  :: (PersistRecordBackend record SqlBackend, AtLeastOneUniqueKey record, Typeable record, MonadSqlQuery m)
  => record -> m (Either (Entity record) (Key record))
insertBy a1 = runQueryRep $ InsertBy a1

insertUniqueEntity
  :: (PersistRecordBackend record SqlBackend, Typeable record, MonadSqlQuery m)
  => record -> m (Maybe (Entity record))
insertUniqueEntity a1 = runQueryRep $ InsertUniqueEntity a1

replaceUnique
  :: (PersistRecordBackend record SqlBackend, Eq (Unique record), Typeable record, MonadSqlQuery m)
  => Key record -> record -> m (Maybe (Unique record))
replaceUnique a1 a2 = runQueryRep $ ReplaceUnique a1 a2

onlyUnique
  :: (PersistRecordBackend record SqlBackend, OnlyOneUniqueKey record, Typeable record, MonadSqlQuery m)
  => record -> m (Unique record)
onlyUnique a1 = runQueryRep $ OnlyUnique a1

selectFirst
  :: (PersistRecordBackend record SqlBackend, Typeable record, MonadSqlQuery m)
  => [Filter record] -> [SelectOpt record] -> m (Maybe (Entity record))
selectFirst a1 a2 = runQueryRep $ SelectFirst a1 a2

count
  :: (PersistRecordBackend record SqlBackend, Typeable record, MonadSqlQuery m)
  => [Filter record] -> m Int
count a1 = runQueryRep $ Count a1

#if MIN_VERSION_persistent(2,11,0)
exists
  :: (PersistRecordBackend record SqlBackend, Typeable record, MonadSqlQuery m)
  => [Filter record] -> m Bool
exists a1 = runQueryRep $ Exists a1
#endif

selectList
  :: (PersistRecordBackend record SqlBackend, Typeable record, MonadSqlQuery m)
  => [Filter record] -> [SelectOpt record] -> m [Entity record]
selectList a1 a2 = runQueryRep $ SelectList a1 a2

selectKeysList
  :: (PersistRecordBackend record SqlBackend, Typeable record, MonadSqlQuery m)
  => [Filter record] -> [SelectOpt record] -> m [Key record]
selectKeysList a1 a2 = runQueryRep $ SelectKeysList a1 a2

updateWhere
  :: (PersistRecordBackend record SqlBackend, Typeable record, MonadSqlQuery m)
  => [Filter record] -> [Update record] -> m ()
updateWhere a1 a2 = runQueryRep $ UpdateWhere a1 a2

deleteWhere
  :: (PersistRecordBackend record SqlBackend, Typeable record, MonadSqlQuery m)
  => [Filter record] -> m ()
deleteWhere a1 = runQueryRep $ DeleteWhere a1

deleteWhereCount
  :: (PersistRecordBackend record SqlBackend, Typeable record, MonadSqlQuery m)
  => [Filter record] -> m Int64
deleteWhereCount a1 = runQueryRep $ DeleteWhereCount a1

updateWhereCount
  :: (PersistRecordBackend record SqlBackend, Typeable record, MonadSqlQuery m)
  => [Filter record] -> [Update record] -> m Int64
updateWhereCount a1 a2 = runQueryRep $ UpdateWhereCount a1 a2

deleteCascade
  :: (DeleteCascade record SqlBackend, Typeable record, MonadSqlQuery m)
  => Key record -> m ()
deleteCascade a1 = runQueryRep $ DeleteCascade a1

deleteCascadeWhere
  :: (DeleteCascade record SqlBackend, Typeable record, MonadSqlQuery m)
  => [Filter record] -> m ()
deleteCascadeWhere a1 = runQueryRep $ DeleteCascadeWhere a1

parseMigration
  :: (HasCallStack, MonadSqlQuery m)
  => Migration -> m (Either [Text] CautiousMigration)
parseMigration a1 = runQueryRep $ ParseMigration a1

parseMigration'
  :: (HasCallStack, MonadSqlQuery m)
  => Migration -> m CautiousMigration
parseMigration' a1 = runQueryRep $ ParseMigration' a1

printMigration
  :: (HasCallStack, MonadSqlQuery m)
  => Migration -> m ()
printMigration a1 = runQueryRep $ PrintMigration a1

showMigration
  :: (HasCallStack, MonadSqlQuery m)
  => Migration -> m [Text]
showMigration a1 = runQueryRep $ ShowMigration a1

getMigration
  :: (HasCallStack, MonadSqlQuery m)
  => Migration -> m [Sql]
getMigration a1 = runQueryRep $ GetMigration a1

runMigration
  :: (MonadSqlQuery m)
  => Migration -> m ()
runMigration a1 = runQueryRep $ RunMigration a1

runMigrationQuiet
  :: (MonadSqlQuery m)
  => Migration -> m [Text]
runMigrationQuiet a1 = runQueryRep $ RunMigrationQuiet a1

runMigrationSilent
  :: (MonadSqlQuery m)
  => Migration -> m [Text]
runMigrationSilent a1 = runQueryRep $ RunMigrationSilent a1

runMigrationUnsafe
  :: (MonadSqlQuery m)
  => Migration -> m ()
runMigrationUnsafe a1 = runQueryRep $ RunMigrationUnsafe a1

runMigrationUnsafeQuiet
  :: (HasCallStack, MonadSqlQuery m)
  => Migration -> m [Text]
runMigrationUnsafeQuiet a1 = runQueryRep $ RunMigrationUnsafeQuiet a1

getFieldName
  :: (PersistRecordBackend record SqlBackend, Typeable record, MonadSqlQuery m)
  => EntityField record typ -> m Text
getFieldName a1 = runQueryRep $ GetFieldName a1

getTableName
  :: (PersistEntity record, Typeable record, MonadSqlQuery m)
  => record -> m Text
getTableName a1 = runQueryRep $ GetTableName a1

withRawQuery
  :: (MonadSqlQuery m)
  => Text -> [PersistValue] -> ConduitM [PersistValue] Void IO a -> m a
withRawQuery a1 a2 a3 = runQueryRep $ WithRawQuery a1 a2 a3

rawExecute
  :: (MonadSqlQuery m)
  => Text -> [PersistValue] -> m ()
rawExecute a1 a2 = runQueryRep $ RawExecute a1 a2

rawExecuteCount
  :: (MonadSqlQuery m)
  => Text -> [PersistValue] -> m Int64
rawExecuteCount a1 a2 = runQueryRep $ RawExecuteCount a1 a2

rawSql
  :: (RawSql a, MonadSqlQuery m)
  => Text -> [PersistValue] -> m [a]
rawSql a1 a2 = runQueryRep $ RawSql a1 a2

transactionSave
  :: (MonadSqlQuery m)
  => m ()
transactionSave = runQueryRep $ TransactionSave

transactionSaveWithIsolation
  :: (MonadSqlQuery m)
  => IsolationLevel -> m ()
transactionSaveWithIsolation a1 = runQueryRep $ TransactionSaveWithIsolation a1

transactionUndo
  :: (MonadSqlQuery m)
  => m ()
transactionUndo = runQueryRep $ TransactionUndo

transactionUndoWithIsolation
  :: (MonadSqlQuery m)
  => IsolationLevel -> m ()
transactionUndoWithIsolation a1 = runQueryRep $ TransactionUndoWithIsolation a1
