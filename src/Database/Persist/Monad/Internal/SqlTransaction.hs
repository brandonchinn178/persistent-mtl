{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Database.Persist.Monad.Internal.SqlTransaction (
  SqlTransaction (..),
  SqlTransactionEnv (..),
  runSqlTransaction,
  catchSqlTransaction,
) where

import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.IO.Unlift (MonadUnliftIO (..))
import Control.Monad.Reader (ReaderT, ask, withReaderT)
import Database.Persist.Sql (SqlBackend, runSqlConn)
import qualified GHC.TypeLits as GHC
import UnliftIO.Exception (Exception, SomeException, catchJust, fromException)

import Control.Monad.IO.Rerunnable (MonadRerunnableIO)
import Control.Monad.Trans.Rerunnable (MonadRerunnableTrans)
import Database.Persist.Monad.Class
import Database.Persist.Monad.SqlQueryRep

{-| The monad that tracks transaction state.

 Conceptually equivalent to 'Database.Persist.Sql.SqlPersistT', but restricts
 IO operations, for two reasons:

   1. Forking a thread that uses the same 'SqlBackend' as the current thread
      causes Bad Things to happen.
   2. Transactions may need to be retried, in which case IO operations in
      a transaction are required to be rerunnable.

 You shouldn't need to explicitly use this type; your functions should only
 declare the 'MonadSqlQuery' constraint.
-}
newtype SqlTransaction m a = UnsafeSqlTransaction
  { unSqlTransaction :: ReaderT SqlTransactionEnv m a
  }
  deriving (Functor, Applicative, Monad, MonadFix, MonadRerunnableIO, MonadRerunnableTrans)

instance
  ( GHC.TypeError ('GHC.Text "Cannot run arbitrary IO actions within a transaction. If the IO action is rerunnable, use rerunnableIO")
  , Monad m
  ) =>
  MonadIO (SqlTransaction m)
  where
  liftIO = undefined

instance (MonadSqlQuery m, MonadUnliftIO m) => MonadSqlQuery (SqlTransaction m) where
  type TransactionM (SqlTransaction m) = TransactionM m

  runQueryRep = UnsafeSqlTransaction . withReaderT sqlBackend . runSqlQueryRep

  -- Delegate to 'm', since 'm' is in charge of starting/stopping transactions.
  -- 'SqlTransaction' is ONLY in charge of executing queries.
  withTransaction = UnsafeSqlTransaction . withTransaction

data SqlTransactionEnv = SqlTransactionEnv
  { sqlBackend :: SqlBackend
  , ignoreCatch :: SomeException -> Bool
  }

runSqlTransaction ::
  MonadUnliftIO m =>
  SqlTransactionEnv ->
  SqlTransaction m a ->
  m a
runSqlTransaction opts =
  (`runSqlConn` sqlBackend opts)
    . withReaderT (\conn -> opts{sqlBackend = conn})
    . unSqlTransaction

-- | Like normal 'catch', except ignores errors specified by 'ignoreCatch'.
catchSqlTransaction ::
  (MonadUnliftIO m, Exception e) =>
  SqlTransaction m a ->
  (e -> SqlTransaction m a) ->
  SqlTransaction m a
catchSqlTransaction (UnsafeSqlTransaction m) handler =
  UnsafeSqlTransaction $ m `catch` (unSqlTransaction . handler)
  where
    catch a b = do
      SqlTransactionEnv{ignoreCatch} <- ask
      catchJust
        (\e -> if ignoreCatch e then Nothing else fromException e)
        a
        b
