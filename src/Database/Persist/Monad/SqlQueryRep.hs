{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Database.Persist.Monad.SqlQueryRep
  ( SqlQueryRep(..)
  , runSqlQueryRep
  ) where

import Control.Monad.IO.Unlift (MonadUnliftIO)
import Data.Conduit (ConduitM)
import Data.Int (Int64)
import Data.Map (Map)
import Data.Proxy (Proxy(..))
import Data.Text (Text)
import Data.Typeable (Typeable, eqT, typeRep, (:~:)(..))
import Data.Void (Void)
import Database.Persist.Sql as Persist hiding (pattern Update)
import GHC.Stack (HasCallStack)

{-# ANN module "HLint: ignore" #-}

data SqlQueryRep record a where
  Get
    :: (PersistRecordBackend record SqlBackend)
    => Key record -> SqlQueryRep record (Maybe record)

  GetMany
    :: (PersistRecordBackend record SqlBackend)
    => [Key record] -> SqlQueryRep record (Map (Key record) record)

  GetJust
    :: (PersistRecordBackend record SqlBackend)
    => Key record -> SqlQueryRep record record

  GetJustEntity
    :: (PersistRecordBackend record SqlBackend)
    => Key record -> SqlQueryRep record (Entity record)

  GetEntity
    :: (PersistRecordBackend record SqlBackend)
    => Key record -> SqlQueryRep record (Maybe (Entity record))

  BelongsTo
    :: (PersistEntity record1, PersistRecordBackend record2 SqlBackend)
    => (record1 -> Maybe (Key record2)) -> record1 -> SqlQueryRep (record1, record2) (Maybe record2)

  BelongsToJust
    :: (PersistEntity record1, PersistRecordBackend record2 SqlBackend)
    => (record1 -> Key record2) -> record1 -> SqlQueryRep (record1, record2) record2

  Insert
    :: (PersistRecordBackend record SqlBackend)
    => record -> SqlQueryRep record (Key record)

  Insert_
    :: (PersistRecordBackend record SqlBackend)
    => record -> SqlQueryRep record ()

  InsertMany
    :: (PersistRecordBackend record SqlBackend)
    => [record] -> SqlQueryRep record [Key record]

  InsertMany_
    :: (PersistRecordBackend record SqlBackend)
    => [record] -> SqlQueryRep record ()

  InsertEntityMany
    :: (PersistRecordBackend record SqlBackend)
    => [Entity record] -> SqlQueryRep record ()

  InsertKey
    :: (PersistRecordBackend record SqlBackend)
    => Key record -> record -> SqlQueryRep record ()

  Repsert
    :: (PersistRecordBackend record SqlBackend)
    => Key record -> record -> SqlQueryRep record ()

  RepsertMany
    :: (PersistRecordBackend record SqlBackend)
    => [(Key record, record)] -> SqlQueryRep record ()

  Replace
    :: (PersistRecordBackend record SqlBackend)
    => Key record -> record -> SqlQueryRep record ()

  Delete
    :: (PersistRecordBackend record SqlBackend)
    => Key record -> SqlQueryRep record ()

  Update
    :: (PersistRecordBackend record SqlBackend)
    => Key record -> [Update record] -> SqlQueryRep record ()

  UpdateGet
    :: (PersistRecordBackend record SqlBackend)
    => Key record -> [Update record] -> SqlQueryRep record record

  InsertEntity
    :: (PersistRecordBackend record SqlBackend)
    => record -> SqlQueryRep record (Entity record)

  InsertRecord
    :: (PersistRecordBackend record SqlBackend)
    => record -> SqlQueryRep record record

  GetBy
    :: (PersistRecordBackend record SqlBackend)
    => Unique record -> SqlQueryRep record (Maybe (Entity record))

  GetByValue
    :: (PersistRecordBackend record SqlBackend, AtLeastOneUniqueKey record)
    => record -> SqlQueryRep record (Maybe (Entity record))

  CheckUnique
    :: (PersistRecordBackend record SqlBackend)
    => record -> SqlQueryRep record (Maybe (Unique record))

#if MIN_VERSION_persistent(2,11,0)
  CheckUniqueUpdateable
    :: (PersistRecordBackend record SqlBackend)
    => Entity record -> SqlQueryRep record (Maybe (Unique record))
#endif

  DeleteBy
    :: (PersistRecordBackend record SqlBackend)
    => Unique record -> SqlQueryRep record ()

  InsertUnique
    :: (PersistRecordBackend record SqlBackend)
    => record -> SqlQueryRep record (Maybe (Key record))

  Upsert
    :: (PersistRecordBackend record SqlBackend, OnlyOneUniqueKey record)
    => record -> [Update record] -> SqlQueryRep record (Entity record)

  UpsertBy
    :: (PersistRecordBackend record SqlBackend)
    => Unique record -> record -> [Update record] -> SqlQueryRep record (Entity record)

  PutMany
    :: (PersistRecordBackend record SqlBackend)
    => [record] -> SqlQueryRep record ()

  InsertBy
    :: (PersistRecordBackend record SqlBackend, AtLeastOneUniqueKey record)
    => record -> SqlQueryRep record (Either (Entity record) (Key record))

  InsertUniqueEntity
    :: (PersistRecordBackend record SqlBackend)
    => record -> SqlQueryRep record (Maybe (Entity record))

  ReplaceUnique
    :: (PersistRecordBackend record SqlBackend, Eq (Unique record))
    => Key record -> record -> SqlQueryRep record (Maybe (Unique record))

  OnlyUnique
    :: (PersistRecordBackend record SqlBackend, OnlyOneUniqueKey record)
    => record -> SqlQueryRep record (Unique record)

  SelectFirst
    :: (PersistRecordBackend record SqlBackend)
    => [Filter record] -> [SelectOpt record] -> SqlQueryRep record (Maybe (Entity record))

  Count
    :: (PersistRecordBackend record SqlBackend)
    => [Filter record] -> SqlQueryRep record Int

#if MIN_VERSION_persistent(2,11,0)
  Exists
    :: (PersistRecordBackend record SqlBackend)
    => [Filter record] -> SqlQueryRep record Bool
#endif

  SelectList
    :: (PersistRecordBackend record SqlBackend)
    => [Filter record] -> [SelectOpt record] -> SqlQueryRep record [Entity record]

  SelectKeysList
    :: (PersistRecordBackend record SqlBackend)
    => [Filter record] -> [SelectOpt record] -> SqlQueryRep record [Key record]

  UpdateWhere
    :: (PersistRecordBackend record SqlBackend)
    => [Filter record] -> [Update record] -> SqlQueryRep record ()

  DeleteWhere
    :: (PersistRecordBackend record SqlBackend)
    => [Filter record] -> SqlQueryRep record ()

  DeleteWhereCount
    :: (PersistRecordBackend record SqlBackend)
    => [Filter record] -> SqlQueryRep record Int64

  UpdateWhereCount
    :: (PersistRecordBackend record SqlBackend)
    => [Filter record] -> [Update record] -> SqlQueryRep record Int64

  DeleteCascade
    :: (DeleteCascade record SqlBackend)
    => Key record -> SqlQueryRep record ()

  DeleteCascadeWhere
    :: (DeleteCascade record SqlBackend)
    => [Filter record] -> SqlQueryRep record ()

  ParseMigration
    :: (HasCallStack)
    => Migration -> SqlQueryRep Void (Either [Text] CautiousMigration)

  ParseMigration'
    :: (HasCallStack)
    => Migration -> SqlQueryRep Void CautiousMigration

  PrintMigration
    :: (HasCallStack)
    => Migration -> SqlQueryRep Void ()

  ShowMigration
    :: (HasCallStack)
    => Migration -> SqlQueryRep Void [Text]

  GetMigration
    :: (HasCallStack)
    => Migration -> SqlQueryRep Void [Sql]

  RunMigration
    :: ()
    => Migration -> SqlQueryRep Void ()

  RunMigrationQuiet
    :: ()
    => Migration -> SqlQueryRep Void [Text]

  RunMigrationSilent
    :: ()
    => Migration -> SqlQueryRep Void [Text]

  RunMigrationUnsafe
    :: ()
    => Migration -> SqlQueryRep Void ()

  RunMigrationUnsafeQuiet
    :: (HasCallStack)
    => Migration -> SqlQueryRep Void [Text]

  GetFieldName
    :: (PersistRecordBackend record SqlBackend)
    => EntityField record typ -> SqlQueryRep record Text

  GetTableName
    :: (PersistEntity record)
    => record -> SqlQueryRep record Text

  WithRawQuery
    :: ()
    => Text -> [PersistValue] -> ConduitM [PersistValue] Void IO a -> SqlQueryRep Void a

  RawExecute
    :: ()
    => Text -> [PersistValue] -> SqlQueryRep Void ()

  RawExecuteCount
    :: ()
    => Text -> [PersistValue] -> SqlQueryRep Void Int64

  RawSql
    :: (RawSql a)
    => Text -> [PersistValue] -> SqlQueryRep Void [a]

  TransactionSave
    :: ()
    => SqlQueryRep Void ()

  TransactionSaveWithIsolation
    :: ()
    => IsolationLevel -> SqlQueryRep Void ()

  TransactionUndo
    :: ()
    => SqlQueryRep Void ()

  TransactionUndoWithIsolation
    :: ()
    => IsolationLevel -> SqlQueryRep Void ()

instance Typeable record => Show (SqlQueryRep record a) where
  show = \case
    Get{} -> "Get{..}" ++ record
    GetMany{} -> "GetMany{..}" ++ record
    GetJust{} -> "GetJust{..}" ++ record
    GetJustEntity{} -> "GetJustEntity{..}" ++ record
    GetEntity{} -> "GetEntity{..}" ++ record
    BelongsTo{} -> "BelongsTo{..}" ++ record
    BelongsToJust{} -> "BelongsToJust{..}" ++ record
    Insert{} -> "Insert{..}" ++ record
    Insert_{} -> "Insert_{..}" ++ record
    InsertMany{} -> "InsertMany{..}" ++ record
    InsertMany_{} -> "InsertMany_{..}" ++ record
    InsertEntityMany{} -> "InsertEntityMany{..}" ++ record
    InsertKey{} -> "InsertKey{..}" ++ record
    Repsert{} -> "Repsert{..}" ++ record
    RepsertMany{} -> "RepsertMany{..}" ++ record
    Replace{} -> "Replace{..}" ++ record
    Delete{} -> "Delete{..}" ++ record
    Update{} -> "Update{..}" ++ record
    UpdateGet{} -> "UpdateGet{..}" ++ record
    InsertEntity{} -> "InsertEntity{..}" ++ record
    InsertRecord{} -> "InsertRecord{..}" ++ record
    GetBy{} -> "GetBy{..}" ++ record
    GetByValue{} -> "GetByValue{..}" ++ record
    CheckUnique{} -> "CheckUnique{..}" ++ record
#if MIN_VERSION_persistent(2,11,0)
    CheckUniqueUpdateable{} -> "CheckUniqueUpdateable{..}" ++ record
#endif
    DeleteBy{} -> "DeleteBy{..}" ++ record
    InsertUnique{} -> "InsertUnique{..}" ++ record
    Upsert{} -> "Upsert{..}" ++ record
    UpsertBy{} -> "UpsertBy{..}" ++ record
    PutMany{} -> "PutMany{..}" ++ record
    InsertBy{} -> "InsertBy{..}" ++ record
    InsertUniqueEntity{} -> "InsertUniqueEntity{..}" ++ record
    ReplaceUnique{} -> "ReplaceUnique{..}" ++ record
    OnlyUnique{} -> "OnlyUnique{..}" ++ record
    SelectFirst{} -> "SelectFirst{..}" ++ record
    Count{} -> "Count{..}" ++ record
#if MIN_VERSION_persistent(2,11,0)
    Exists{} -> "Exists{..}" ++ record
#endif
    SelectList{} -> "SelectList{..}" ++ record
    SelectKeysList{} -> "SelectKeysList{..}" ++ record
    UpdateWhere{} -> "UpdateWhere{..}" ++ record
    DeleteWhere{} -> "DeleteWhere{..}" ++ record
    DeleteWhereCount{} -> "DeleteWhereCount{..}" ++ record
    UpdateWhereCount{} -> "UpdateWhereCount{..}" ++ record
    DeleteCascade{} -> "DeleteCascade{..}" ++ record
    DeleteCascadeWhere{} -> "DeleteCascadeWhere{..}" ++ record
    ParseMigration{} -> "ParseMigration{..}" ++ record
    ParseMigration'{} -> "ParseMigration'{..}" ++ record
    PrintMigration{} -> "PrintMigration{..}" ++ record
    ShowMigration{} -> "ShowMigration{..}" ++ record
    GetMigration{} -> "GetMigration{..}" ++ record
    RunMigration{} -> "RunMigration{..}" ++ record
    RunMigrationQuiet{} -> "RunMigrationQuiet{..}" ++ record
    RunMigrationSilent{} -> "RunMigrationSilent{..}" ++ record
    RunMigrationUnsafe{} -> "RunMigrationUnsafe{..}" ++ record
    RunMigrationUnsafeQuiet{} -> "RunMigrationUnsafeQuiet{..}" ++ record
    GetFieldName{} -> "GetFieldName{..}" ++ record
    GetTableName{} -> "GetTableName{..}" ++ record
    WithRawQuery{} -> "WithRawQuery{..}" ++ record
    RawExecute{} -> "RawExecute{..}" ++ record
    RawExecuteCount{} -> "RawExecuteCount{..}" ++ record
    RawSql{} -> "RawSql{..}" ++ record
    TransactionSave{} -> "TransactionSave{..}" ++ record
    TransactionSaveWithIsolation{} -> "TransactionSaveWithIsolation{..}" ++ record
    TransactionUndo{} -> "TransactionUndo{..}" ++ record
    TransactionUndoWithIsolation{} -> "TransactionUndoWithIsolation{..}" ++ record
    where
      record = case recordTypeRep of
        Just recordType -> "<" ++ show recordType ++ ">"
        Nothing -> ""
      recordTypeRep = case eqT @record @Void of
        Just Refl -> Nothing
        Nothing -> Just $ typeRep $ Proxy @record

runSqlQueryRep :: MonadUnliftIO m => SqlQueryRep record a -> Persist.SqlPersistT m a
runSqlQueryRep = \case
  Get a1 -> Persist.get a1
  GetMany a1 -> Persist.getMany a1
  GetJust a1 -> Persist.getJust a1
  GetJustEntity a1 -> Persist.getJustEntity a1
  GetEntity a1 -> Persist.getEntity a1
  BelongsTo a1 a2 -> Persist.belongsTo a1 a2
  BelongsToJust a1 a2 -> Persist.belongsToJust a1 a2
  Insert a1 -> Persist.insert a1
  Insert_ a1 -> Persist.insert_ a1
  InsertMany a1 -> Persist.insertMany a1
  InsertMany_ a1 -> Persist.insertMany_ a1
  InsertEntityMany a1 -> Persist.insertEntityMany a1
  InsertKey a1 a2 -> Persist.insertKey a1 a2
  Repsert a1 a2 -> Persist.repsert a1 a2
  RepsertMany a1 -> Persist.repsertMany a1
  Replace a1 a2 -> Persist.replace a1 a2
  Delete a1 -> Persist.delete a1
  Update a1 a2 -> Persist.update a1 a2
  UpdateGet a1 a2 -> Persist.updateGet a1 a2
  InsertEntity a1 -> Persist.insertEntity a1
  InsertRecord a1 -> Persist.insertRecord a1
  GetBy a1 -> Persist.getBy a1
  GetByValue a1 -> Persist.getByValue a1
  CheckUnique a1 -> Persist.checkUnique a1
#if MIN_VERSION_persistent(2,11,0)
  CheckUniqueUpdateable a1 -> Persist.checkUniqueUpdateable a1
#endif
  DeleteBy a1 -> Persist.deleteBy a1
  InsertUnique a1 -> Persist.insertUnique a1
  Upsert a1 a2 -> Persist.upsert a1 a2
  UpsertBy a1 a2 a3 -> Persist.upsertBy a1 a2 a3
  PutMany a1 -> Persist.putMany a1
  InsertBy a1 -> Persist.insertBy a1
  InsertUniqueEntity a1 -> Persist.insertUniqueEntity a1
  ReplaceUnique a1 a2 -> Persist.replaceUnique a1 a2
  OnlyUnique a1 -> Persist.onlyUnique a1
  SelectFirst a1 a2 -> Persist.selectFirst a1 a2
  Count a1 -> Persist.count a1
#if MIN_VERSION_persistent(2,11,0)
  Exists a1 -> Persist.exists a1
#endif
  SelectList a1 a2 -> Persist.selectList a1 a2
  SelectKeysList a1 a2 -> Persist.selectKeysList a1 a2
  UpdateWhere a1 a2 -> Persist.updateWhere a1 a2
  DeleteWhere a1 -> Persist.deleteWhere a1
  DeleteWhereCount a1 -> Persist.deleteWhereCount a1
  UpdateWhereCount a1 a2 -> Persist.updateWhereCount a1 a2
  DeleteCascade a1 -> Persist.deleteCascade a1
  DeleteCascadeWhere a1 -> Persist.deleteCascadeWhere a1
  ParseMigration a1 -> Persist.parseMigration a1
  ParseMigration' a1 -> Persist.parseMigration' a1
  PrintMigration a1 -> Persist.printMigration a1
  ShowMigration a1 -> Persist.showMigration a1
  GetMigration a1 -> Persist.getMigration a1
  RunMigration a1 -> Persist.runMigration a1
  RunMigrationQuiet a1 -> Persist.runMigrationQuiet a1
  RunMigrationSilent a1 -> Persist.runMigrationSilent a1
  RunMigrationUnsafe a1 -> Persist.runMigrationUnsafe a1
  RunMigrationUnsafeQuiet a1 -> Persist.runMigrationUnsafeQuiet a1
  GetFieldName a1 -> Persist.getFieldName a1
  GetTableName a1 -> Persist.getTableName a1
  WithRawQuery a1 a2 a3 -> Persist.withRawQuery a1 a2 a3
  RawExecute a1 a2 -> Persist.rawExecute a1 a2
  RawExecuteCount a1 a2 -> Persist.rawExecuteCount a1 a2
  RawSql a1 a2 -> Persist.rawSql a1 a2
  TransactionSave -> Persist.transactionSave
  TransactionSaveWithIsolation a1 -> Persist.transactionSaveWithIsolation a1
  TransactionUndo -> Persist.transactionUndo
  TransactionUndoWithIsolation a1 -> Persist.transactionUndoWithIsolation a1
