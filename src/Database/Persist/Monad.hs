{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
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
  , SqlQueryBackend(..)

  -- * Lifted functions
  , module Database.Persist.Monad.Shim
  ) where

import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.IO.Unlift (MonadUnliftIO(..), wrappedWithRunInIO)
import Control.Monad.Reader (ReaderT, ask, local, runReaderT)
import Control.Monad.Trans.Class (MonadTrans(..))
import Data.Pool (Pool)
import Database.Persist.Sql (SqlBackend, runSqlConn, runSqlPool)

import Database.Persist.Monad.Class
import Database.Persist.Monad.Shim
import Database.Persist.Monad.SqlQueryRep

{- SqlQueryT monad -}

data SqlQueryEnv = SqlQueryEnv
  { backend     :: SqlQueryBackend
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
  runQueryRep queryRep =
    withCurrentConnection $ \conn ->
      runSqlConn (runSqlQueryRep queryRep) conn

  withTransaction action =
    withCurrentConnection $ \conn ->
      SqlQueryT . local (\env -> env { currentConn = Just conn }) . unSqlQueryT $ action

instance MonadUnliftIO m => MonadUnliftIO (SqlQueryT m) where
  withRunInIO = wrappedWithRunInIO SqlQueryT unSqlQueryT

{- Running SqlQueryT -}

-- | The backend to use to run 'SqlQueryT'.
--
-- You can get these from the database-specific persistent library, e.g.
-- 'Database.Persist.Postgresql.withPostgresqlConn' or
-- 'Database.Persist.Postgresql.withPostgresqlPool' from @persistent-postgresql@
data SqlQueryBackend
  = BackendSingle SqlBackend
  | BackendPool (Pool SqlBackend)

-- | Run the 'SqlQueryT' monad transformer with the given backend.
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
  SqlQueryEnv { backend = BackendPool pool } -> runSqlPool (lift . f =<< ask) pool
