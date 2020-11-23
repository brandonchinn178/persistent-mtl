{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Database.Persist.Monad
  ( MonadSqlQuery(..)

    -- * SqlQueryT monad transformer
  , SqlQueryT
  , SqlQueryBackend(..)
  , runSqlQueryT

    -- * Test utility
  , MockSqlQueryT
  , runMockSqlQueryT
  , withRecord

    -- * Coerced functions
  , SqlQueryRep(..)
  , selectList
  , insert
  , insert_
  , runMigrationSilent
  ) where

import Control.Monad (msum)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Reader (ReaderT, ask, local, runReaderT)
import Data.Pool (Pool)
import qualified Data.Pool as Pool
import Data.Proxy (Proxy(..))
import Data.Text (Text)
import Data.Typeable ((:~:)(..), Typeable, eqT, typeRep)
import Database.Persist (Entity, Filter, Key, PersistRecordBackend, SelectOpt)
import Database.Persist.Sql (Migration, SqlBackend)
import qualified Database.Persist.Sql as Persist
import UnliftIO (MonadUnliftIO, mask, onException, withRunInIO, wrappedWithRunInIO)

class MonadSqlQuery m where
  runQueryRep :: Typeable record => SqlQueryRep record a -> m a
  runRawQuery :: Persist.SqlPersistT m a -> m a
  withTransaction :: m a -> m a

{- SqlQueryT -}

data SqlQueryEnv = SqlQueryEnv
  { backend     :: SqlQueryBackend
  , currentConn :: Maybe SqlBackend
  }

newtype SqlQueryT m a = SqlQueryT
  { unSqlQueryT :: ReaderT SqlQueryEnv m a
  } deriving
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    )

instance MonadUnliftIO m => MonadUnliftIO (SqlQueryT m) where
  withRunInIO = wrappedWithRunInIO SqlQueryT unSqlQueryT

data SqlQueryBackend
  = BackendSingle SqlBackend
  | BackendPool (Pool SqlBackend)

runSqlQueryT :: SqlQueryBackend -> SqlQueryT m a -> m a
runSqlQueryT backend = (`runReaderT` env) . unSqlQueryT
  where
    env = SqlQueryEnv { currentConn = Nothing, .. }

withCurrentConnection :: MonadUnliftIO m => (SqlBackend -> SqlQueryT m a) -> SqlQueryT m a
withCurrentConnection f = SqlQueryT ask >>= \case
  -- Currently in a transaction; use the transaction connection
  SqlQueryEnv { currentConn = Just conn } -> f conn
  -- Otherwise, get a new connection
  SqlQueryEnv { backend = BackendSingle conn } -> f conn
  SqlQueryEnv { backend = BackendPool pool } -> withResource pool f

instance MonadUnliftIO m => MonadSqlQuery (SqlQueryT m) where
  runQueryRep = runRawQuery . runSqlQueryRep

  runRawQuery m = withCurrentConnection (Persist.runSqlConn m)

  withTransaction action =
    withCurrentConnection $ \conn ->
      SqlQueryT . local (\env -> env { currentConn = Just conn }) . unSqlQueryT $ action

{- SqlQueryRep
   TODO: generate this with TH
-}

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

instance Typeable record => Show (SqlQueryRep record a) where
  show = \case
    SelectList{} -> "SelectList{..}" ++ record
    Insert{} -> "Insert{..}" ++ record
    Insert_{} -> "Insert_{..}" ++ record
    where
      record = "<" ++ show (typeRep $ Proxy @record) ++ ">"

runSqlQueryRep :: MonadIO m => SqlQueryRep record a -> Persist.SqlPersistT m a
runSqlQueryRep = \case
  SelectList a b -> Persist.selectList a b
  Insert a -> Persist.insert a
  Insert_ a -> Persist.insert_ a

selectList :: (PersistRecordBackend record SqlBackend, Typeable record, MonadSqlQuery m) => [Filter record] -> [SelectOpt record] -> m [Entity record]
selectList a b = runQueryRep $ SelectList a b

insert :: (PersistRecordBackend record SqlBackend, Typeable record, MonadSqlQuery m) => record -> m (Key record)
insert a = runQueryRep $ Insert a

insert_ :: (PersistRecordBackend record SqlBackend, Typeable record, MonadSqlQuery m) => record -> m ()
insert_ a = runQueryRep $ Insert_ a

runMigrationSilent :: (MonadUnliftIO m, MonadSqlQuery m) => Migration -> m [Text]
runMigrationSilent a = runRawQuery $ Persist.runMigrationSilent a

{- MockSqlQueryT -}

data MockQuery = MockQuery (forall record a. Typeable record => SqlQueryRep record a -> Maybe a)

withRecord :: forall record. Typeable record => (forall a. SqlQueryRep record a -> Maybe a) -> MockQuery
withRecord f = MockQuery $ \(rep :: SqlQueryRep someRecord result) ->
  case eqT @record @someRecord of
    Just Refl -> f rep
    Nothing -> Nothing

newtype MockSqlQueryT m a = MockSqlQueryT
  { unMockSqlQueryT :: ReaderT [MockQuery] m a
  } deriving
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    )

runMockSqlQueryT :: MockSqlQueryT m a -> [MockQuery] -> m a
runMockSqlQueryT action mockQueries = (`runReaderT` mockQueries) . unMockSqlQueryT $ action

instance Monad m => MonadSqlQuery (MockSqlQueryT m) where
  runQueryRep rep = do
    mockQueries <- MockSqlQueryT ask
    maybe (error $ "Could not find mock for query: " ++ show rep) return
      $ msum $ map tryMockQuery mockQueries
    where
      tryMockQuery (MockQuery f) = f rep

  runRawQuery _ = error "Can't run raw queries with MockSqlQueryT"

  withTransaction = id

-- | Upstream 'withResource' modified to use MonadUnliftIO
-- https://github.com/bos/pool/issues/31
withResource :: MonadUnliftIO m => Pool a -> (a -> m b) -> m b
withResource pool act = withRunInIO $ \runInIO -> mask $ \restore -> do
  (resource, local') <- Pool.takeResource pool
  ret <- restore (runInIO (act resource)) `onException`
            Pool.destroyResource pool local' resource
  Pool.putResource local' resource
  return ret
