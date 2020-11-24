{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Database.Persist.Monad.SqlQueryRep
  ( SqlQueryRep(..)
  , runSqlQueryRep
  ) where

import Control.Monad.IO.Unlift (MonadUnliftIO)
import Data.Map (Map)
import Data.Proxy (Proxy(..))
import Data.Text (Text)
import Data.Typeable (Typeable, eqT, typeRep, (:~:)(..))
import Data.Void (Void)
import Database.Persist.Sql as Persist

data SqlQueryRep record a where
  {- PersistStoreRead -}

  Get
    :: PersistRecordBackend record SqlBackend
    => Key record -> SqlQueryRep record (Maybe record)

  GetMany
    :: PersistRecordBackend record SqlBackend
    => [Key record] -> SqlQueryRep record (Map (Key record) record)

  GetJust
    :: PersistRecordBackend record SqlBackend
    => Key record -> SqlQueryRep record record

  GetJustEntity
    :: PersistRecordBackend record SqlBackend
    => Key record -> SqlQueryRep record (Entity record)

  GetEntity
    :: PersistRecordBackend record SqlBackend
    => Key record -> SqlQueryRep record (Maybe (Entity record))

  BelongsTo
    :: (PersistEntity record1, PersistRecordBackend record2 SqlBackend)
    => (record1 -> Maybe (Key record2)) -> record1 -> SqlQueryRep (record1, record2) (Maybe record2)

  BelongsToJust
    :: (PersistEntity record1, PersistRecordBackend record2 SqlBackend)
    => (record1 -> Key record2) -> record1 -> SqlQueryRep (record1, record2) record2

  {- Other -}

  SelectList
    :: PersistRecordBackend record SqlBackend
    => [Filter record] -> [SelectOpt record] -> SqlQueryRep record [Entity record]

  Insert
    :: PersistRecordBackend record SqlBackend
    => record -> SqlQueryRep record (Key record)

  Insert_
    :: PersistRecordBackend record SqlBackend
    => record -> SqlQueryRep record ()

  RunMigrationsSilent
    :: Migration -> SqlQueryRep Void [Text]

instance Typeable record => Show (SqlQueryRep record a) where
  show = \case
    Get{} -> "Get{..}" ++ record
    GetMany{} -> "GetMany{..}" ++ record
    GetJust{} -> "GetJust{..}" ++ record
    GetJustEntity{} -> "GetJustEntity{..}" ++ record
    GetEntity{} -> "GetEntity{..}" ++ record
    BelongsTo{} -> "BelongsTo{..}" ++ record
    BelongsToJust{} -> "BelongsToJust{..}" ++ record

    SelectList{} -> "SelectList{..}" ++ record
    Insert{} -> "Insert{..}" ++ record
    Insert_{} -> "Insert_{..}" ++ record
    RunMigrationsSilent{} -> "RunMigrationsSilent{..}" ++ record
    where
      record = case recordTypeRep of
        Just recordType -> "<" ++ show recordType ++ ">"
        Nothing -> ""
      recordTypeRep = case eqT @record @Void of
        Just Refl -> Nothing
        Nothing -> Just $ typeRep $ Proxy @record

runSqlQueryRep :: MonadUnliftIO m => SqlQueryRep record a -> Persist.SqlPersistT m a
runSqlQueryRep = \case
  Get a -> Persist.get a
  GetMany a -> Persist.getMany a
  GetJust a -> Persist.getJust a
  GetJustEntity a -> Persist.getJustEntity a
  GetEntity a -> Persist.getEntity a
  BelongsTo a b -> Persist.belongsTo a b
  BelongsToJust a b -> Persist.belongsToJust a b

  SelectList a b -> Persist.selectList a b
  Insert a -> Persist.insert a
  Insert_ a -> Persist.insert_ a
  RunMigrationsSilent a -> Persist.runMigrationSilent a
