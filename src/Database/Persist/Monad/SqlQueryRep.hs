{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Database.Persist.Monad.SqlQueryRep
  ( SqlQueryRep(..)
  , runSqlQueryRep
  ) where

import Control.Monad.IO.Unlift (MonadUnliftIO)
import Data.Proxy (Proxy(..))
import Data.Text (Text)
import Data.Typeable (Typeable, eqT, typeRep, (:~:)(..))
import Data.Void (Void)
import Database.Persist (Entity, Filter, Key, PersistRecordBackend, SelectOpt)
import Database.Persist.Sql (Migration, SqlBackend)
import qualified Database.Persist.Sql as Persist

-- TODO: generate this
data SqlQueryRep record a where
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
  SelectList a b -> Persist.selectList a b
  Insert a -> Persist.insert a
  Insert_ a -> Persist.insert_ a
  RunMigrationsSilent a -> Persist.runMigrationSilent a
