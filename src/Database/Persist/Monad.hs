{-|
Module: Database.Persist.Monad

Defines the 'SqlQueryT' monad transformer, which has a 'MonadSqlQuery' instance
to execute @persistent@ database operations, and the `SqlTransaction` monad,
which tracks transaction state to ensure @persistent@ database operations are
run in a single transaction.

Usage:

@
myFunction :: (MonadSqlQuery m, MonadIO m) => m ()
myFunction = do
  insert_ $ Person { name = \"Alice\", age = Just 25 }
  insert_ $ Person { name = \"Bob\", age = Nothing }

  -- some other business logic

  personList <- selectList [] []
  liftIO $ print (personList :: [Person])

  -- everything in here will run in a transaction
  withTransaction $
    selectFirst [PersonAge >. 30] [] >>= \\case
      Nothing -> insert_ $ Person { name = \"Claire\", age = Just 50 }
      Just (Entity key person) -> replace key person{ age = Just (age person - 10) }

  -- some more business logic

  return ()
@
-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Database.Persist.Monad
  (
  -- * Type class for executing database queries
    MonadSqlQuery
  , withTransaction
  , SqlQueryRep(..)

  -- * SqlQueryT monad transformer
  , SqlQueryT
  , runSqlQueryT
  , SqlTransaction

  -- * Lifted functions
  , module Database.Persist.Monad.Shim
  ) where

import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.IO.Unlift (MonadUnliftIO(..), wrappedWithRunInIO)
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Trans.Resource (MonadResource)
import Data.Acquire (withAcquire)
import Data.Pool (Pool)
import Data.Pool.Acquire (poolToAcquire)
import Database.Persist.Sql (SqlBackend, SqlPersistT, runSqlConn)
import qualified GHC.TypeLits as GHC

import Control.Monad.IO.Rerunnable (MonadRerunnableIO)
import Database.Persist.Monad.Class
import Database.Persist.Monad.Shim
import Database.Persist.Monad.SqlQueryRep

{- SqlTransaction -}

-- | The monad that tracks transaction state.
--
-- Conceptually equivalent to 'Database.Persist.Sql.SqlPersistT', but restricts
-- IO operations, for two reasons:
--   1. Forking a thread that uses the same 'SqlBackend' as the current thread
--      causes Bad Things to happen.
--   2. Transactions may need to be retried, in which case IO operations in
--      a transaction are required to be rerunnable.
--
-- You shouldn't need to explicitly use this type; your functions should only
-- declare the 'MonadSqlQuery' constraint.
newtype SqlTransaction m a = SqlTransaction
  { unSqlTransaction :: SqlPersistT m a
  }
  deriving (Functor, Applicative, Monad, MonadRerunnableIO)

instance
  ( GHC.TypeError ('GHC.Text "Cannot run arbitrary IO actions within a transaction. If the IO action is rerunnable, use rerunnableIO")
  , Monad m
  )
  => MonadIO (SqlTransaction m) where
  liftIO = undefined

instance (MonadSqlQuery m, MonadUnliftIO m) => MonadSqlQuery (SqlTransaction m) where
  type TransactionM (SqlTransaction m) = TransactionM m

  runQueryRep = SqlTransaction . runSqlQueryRep

  -- Delegate to 'm', since 'm' is in charge of starting/stopping transactions.
  -- 'SqlTransaction' is ONLY in charge of executing queries.
  withTransaction = SqlTransaction . withTransaction

runSqlTransaction :: MonadUnliftIO m => SqlBackend -> SqlTransaction m a -> m a
runSqlTransaction conn = (`runSqlConn` conn) . unSqlTransaction

{- SqlQueryT monad -}

data SqlQueryEnv = SqlQueryEnv
  { backendPool :: Pool SqlBackend
  }

-- | The monad transformer that implements 'MonadSqlQuery'.
newtype SqlQueryT m a = SqlQueryT
  { unSqlQueryT :: ReaderT SqlQueryEnv m a
  } deriving
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadTrans
    , MonadResource
    , MonadRerunnableIO
    )

instance MonadUnliftIO m => MonadSqlQuery (SqlQueryT m) where
  type TransactionM (SqlQueryT m) = SqlTransaction (SqlQueryT m)

  -- Running a query directly in SqlQueryT will create a one-off transaction.
  runQueryRep = withTransaction . runQueryRep

  -- Start a new transaction and run the given 'SqlTransaction'
  withTransaction m = do
    SqlQueryEnv{backendPool} <- SqlQueryT ask
    withAcquire (poolToAcquire backendPool) $ \conn ->
      runSqlTransaction conn m

instance MonadUnliftIO m => MonadUnliftIO (SqlQueryT m) where
  withRunInIO = wrappedWithRunInIO SqlQueryT unSqlQueryT

{- Running SqlQueryT -}

-- | Run the 'SqlQueryT' monad transformer with the given backend.
runSqlQueryT :: Pool SqlBackend -> SqlQueryT m a -> m a
runSqlQueryT backendPool = (`runReaderT` env) . unSqlQueryT
  where
    env = SqlQueryEnv{..}
