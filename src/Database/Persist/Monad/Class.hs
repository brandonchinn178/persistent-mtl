module Database.Persist.Monad.Class
  ( MonadSqlQuery(..)
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
import Data.Typeable (Typeable)

import Database.Persist.Monad.SqlQueryRep (SqlQueryRep)

-- | The type-class for monads that can run persistent database queries.
class Monad m => MonadSqlQuery m where
  -- | The main function that interprets a SQL query operation and runs it
  -- in the monadic context.
  runQueryRep :: Typeable record => SqlQueryRep record a -> m a

  -- | Run all queries in the given action using the same database connection.
  --
  -- You should make sure to not fork any threads within this action. This
  -- will almost certainly cause problems.
  -- https://github.com/brandonchinn178/persistent-mtl/issues/7
  withTransaction :: m a -> m a

{- Instances for common monad transformers -}

instance MonadSqlQuery m => MonadSqlQuery (Reader.ReaderT r m) where
  runQueryRep = lift . runQueryRep
  withTransaction = Reader.mapReaderT withTransaction

instance MonadSqlQuery m => MonadSqlQuery (Except.ExceptT e m) where
  runQueryRep = lift . runQueryRep
  withTransaction = Except.mapExceptT withTransaction

instance MonadSqlQuery m => MonadSqlQuery (Identity.IdentityT m) where
  runQueryRep = lift . runQueryRep
  withTransaction = Identity.mapIdentityT withTransaction

instance MonadSqlQuery m => MonadSqlQuery (Maybe.MaybeT m) where
  runQueryRep = lift . runQueryRep
  withTransaction = Maybe.mapMaybeT withTransaction

instance (Monoid w, MonadSqlQuery m) => MonadSqlQuery (RWS.Lazy.RWST r w s m) where
  runQueryRep = lift . runQueryRep
  withTransaction = RWS.Lazy.mapRWST withTransaction

instance (Monoid w, MonadSqlQuery m) => MonadSqlQuery (RWS.Strict.RWST r w s m) where
  runQueryRep = lift . runQueryRep
  withTransaction = RWS.Strict.mapRWST withTransaction

instance MonadSqlQuery m => MonadSqlQuery (State.Lazy.StateT s m) where
  runQueryRep = lift . runQueryRep
  withTransaction = State.Lazy.mapStateT withTransaction

instance MonadSqlQuery m => MonadSqlQuery (State.Strict.StateT s m) where
  runQueryRep = lift . runQueryRep
  withTransaction = State.Strict.mapStateT withTransaction

instance (Monoid w, MonadSqlQuery m) => MonadSqlQuery (Writer.Lazy.WriterT w m) where
  runQueryRep = lift . runQueryRep
  withTransaction = Writer.Lazy.mapWriterT withTransaction

instance (Monoid w, MonadSqlQuery m) => MonadSqlQuery (Writer.Strict.WriterT w m) where
  runQueryRep = lift . runQueryRep
  withTransaction = Writer.Strict.mapWriterT withTransaction
