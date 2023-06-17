{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableSuperClasses #-}

{-|
Module: Database.Persist.Monad.Class

Defines the 'MonadSqlQuery' type class that a monad can make an instance of
in order to interpret how to run a
'Database.Persist.Monad.SqlQueryRep.SqlQueryRep' sent by a lifted function from
@Database.Persist.Monad.Shim@.
-}
module Database.Persist.Monad.Class (
  MonadSqlQuery (..),
) where

import Control.Monad.Trans.Class (lift)
import qualified Control.Monad.Trans.Except as Except
import qualified Control.Monad.Trans.Identity as Identity
import qualified Control.Monad.Trans.Maybe as Maybe
import qualified Control.Monad.Trans.RWS.Lazy as RWS.Lazy
import qualified Control.Monad.Trans.RWS.Strict as RWS.Strict
import qualified Control.Monad.Trans.Reader as Reader
import qualified Control.Monad.Trans.State.Lazy as State.Lazy
import qualified Control.Monad.Trans.State.Strict as State.Strict
import qualified Control.Monad.Trans.Writer.Lazy as Writer.Lazy
import qualified Control.Monad.Trans.Writer.Strict as Writer.Strict
import Data.Kind (Type)
import Data.Typeable (Typeable)

import Database.Persist.Monad.SqlQueryRep (SqlQueryRep)

-- | The type-class for monads that can run persistent database queries.
class (Monad m, MonadSqlQuery (TransactionM m)) => MonadSqlQuery m where
  type TransactionM m :: Type -> Type

  -- | Interpret the given SQL query operation.
  runQueryRep :: (Typeable record) => SqlQueryRep record a -> m a

  -- | Run all queries in the given action using the same database connection.
  withTransaction :: TransactionM m a -> m a

{- Instances for common monad transformers -}

instance (MonadSqlQuery m) => MonadSqlQuery (Reader.ReaderT r m) where
  type TransactionM (Reader.ReaderT r m) = TransactionM m
  runQueryRep = lift . runQueryRep
  withTransaction = lift . withTransaction

instance (MonadSqlQuery m) => MonadSqlQuery (Except.ExceptT e m) where
  type TransactionM (Except.ExceptT e m) = TransactionM m
  runQueryRep = lift . runQueryRep
  withTransaction = lift . withTransaction

instance (MonadSqlQuery m) => MonadSqlQuery (Identity.IdentityT m) where
  type TransactionM (Identity.IdentityT m) = TransactionM m
  runQueryRep = lift . runQueryRep
  withTransaction = lift . withTransaction

instance (MonadSqlQuery m) => MonadSqlQuery (Maybe.MaybeT m) where
  type TransactionM (Maybe.MaybeT m) = TransactionM m
  runQueryRep = lift . runQueryRep
  withTransaction = lift . withTransaction

instance (Monoid w, MonadSqlQuery m) => MonadSqlQuery (RWS.Lazy.RWST r w s m) where
  type TransactionM (RWS.Lazy.RWST r w s m) = TransactionM m
  runQueryRep = lift . runQueryRep
  withTransaction = lift . withTransaction

instance (Monoid w, MonadSqlQuery m) => MonadSqlQuery (RWS.Strict.RWST r w s m) where
  type TransactionM (RWS.Strict.RWST r w s m) = TransactionM m
  runQueryRep = lift . runQueryRep
  withTransaction = lift . withTransaction

instance (MonadSqlQuery m) => MonadSqlQuery (State.Lazy.StateT s m) where
  type TransactionM (State.Lazy.StateT s m) = TransactionM m
  runQueryRep = lift . runQueryRep
  withTransaction = lift . withTransaction

instance (MonadSqlQuery m) => MonadSqlQuery (State.Strict.StateT s m) where
  type TransactionM (State.Strict.StateT s m) = TransactionM m
  runQueryRep = lift . runQueryRep
  withTransaction = lift . withTransaction

instance (Monoid w, MonadSqlQuery m) => MonadSqlQuery (Writer.Lazy.WriterT w m) where
  type TransactionM (Writer.Lazy.WriterT w m) = TransactionM m
  runQueryRep = lift . runQueryRep
  withTransaction = lift . withTransaction

instance (Monoid w, MonadSqlQuery m) => MonadSqlQuery (Writer.Strict.WriterT w m) where
  type TransactionM (Writer.Strict.WriterT w m) = TransactionM m
  runQueryRep = lift . runQueryRep
  withTransaction = lift . withTransaction
