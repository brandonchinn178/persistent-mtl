{-|
Module: Database.Persist.Monad

Defines the 'SqlQueryT' monad transformer that has a 'MonadSqlQuery' instance
to execute @persistent@ database operations.

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

{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Database.Persist.Monad
  (
  -- * Type class for executing database queries
    MonadSqlQuery
  , withTransaction
  , SqlQueryRep(..)

  -- * SqlQueryT monad transformer
  , SqlQueryT
  , runSqlQueryT

  -- * Lifted functions
  , module Database.Persist.Monad.Shim
  ) where

import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.IO.Unlift (MonadUnliftIO(..), wrappedWithRunInIO)
import Control.Monad.Reader (ReaderT, ask, local, runReaderT)
import Control.Monad.Trans.Class (MonadTrans(..))
import Data.Acquire (withAcquire)
import Data.Pool (Pool)
import Data.Pool.Acquire (poolToAcquire)
import Database.Persist.Sql (SqlBackend, runSqlConn)

import Database.Persist.Monad.Class
import Database.Persist.Monad.Shim
import Database.Persist.Monad.SqlQueryRep

{- SqlQueryT monad -}

data SqlQueryEnv = SqlQueryEnv
  { backendPool :: Pool SqlBackend
  , currentConn :: Maybe SqlBackend
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
    )

instance MonadUnliftIO m => MonadSqlQuery (SqlQueryT m) where
  runQueryRep queryRep = do
    SqlQueryEnv{currentConn} <- SqlQueryT ask
    case currentConn of
      Just conn -> runWithConn conn
      Nothing -> withTransactionConn runWithConn
    where
      runWithConn = runReaderT (runSqlQueryRep queryRep)

  withTransaction action = withTransactionConn $ \_ -> action

instance MonadUnliftIO m => MonadUnliftIO (SqlQueryT m) where
  withRunInIO = wrappedWithRunInIO SqlQueryT unSqlQueryT

{- Running SqlQueryT -}

-- | Run the 'SqlQueryT' monad transformer with the given backend.
runSqlQueryT :: Pool SqlBackend -> SqlQueryT m a -> m a
runSqlQueryT backendPool = (`runReaderT` env) . unSqlQueryT
  where
    env = SqlQueryEnv { currentConn = Nothing, .. }

-- | Start a new transaction and get the connection.
withTransactionConn :: MonadUnliftIO m => (SqlBackend -> SqlQueryT m a) -> SqlQueryT m a
withTransactionConn f = do
  SqlQueryEnv{backendPool} <- SqlQueryT ask
  withAcquire (poolToAcquire backendPool) $ \conn ->
    SqlQueryT . local (setCurrentConn conn) . unSqlQueryT $
      runSqlConn (lift $ f conn) conn
  where
    setCurrentConn conn env = env { currentConn = Just conn }
