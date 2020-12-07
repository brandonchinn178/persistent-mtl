{-|
Module: Control.Monad.IO.Rerunnable

Defines the 'MonadRerunnableIO' type class that is functionally equivalent
to 'Control.Monad.IO.Class.MonadIO', but use of it requires the user to
explicitly acknowledge that the given IO operation can be rerun.
-}

module Control.Monad.IO.Rerunnable
  ( MonadRerunnableIO(..)
  ) where

import Control.Monad.Trans.Class (lift)
import qualified Control.Monad.Trans.Except as Except
import qualified Control.Monad.Trans.Identity as Identity
import qualified Control.Monad.Trans.Maybe as Maybe
import qualified Control.Monad.Trans.RWS.Lazy as RWS.Lazy
import qualified Control.Monad.Trans.RWS.Strict as RWS.Strict
import qualified Control.Monad.Trans.Reader as Reader
import qualified Control.Monad.Trans.Resource as Resource
import qualified Control.Monad.Trans.State.Lazy as State.Lazy
import qualified Control.Monad.Trans.State.Strict as State.Strict
import qualified Control.Monad.Trans.Writer.Lazy as Writer.Lazy
import qualified Control.Monad.Trans.Writer.Strict as Writer.Strict

-- | A copy of 'Control.Monad.IO.Class.MonadIO' to explicitly allow only IO
-- operations that are rerunnable, e.g. in the context of a SQL transaction.
class Monad m => MonadRerunnableIO m where
  -- | Lift the given IO operation to @m@.
  --
  -- The given IO operation may be rerun, so use of this function requires
  -- manually verifying that the given IO operation is rerunnable.
  rerunnableIO :: IO a -> m a

instance MonadRerunnableIO IO where
  rerunnableIO = id

{- Instances for common monad transformers -}

instance MonadRerunnableIO m => MonadRerunnableIO (Reader.ReaderT r m) where
  rerunnableIO = lift . rerunnableIO

instance MonadRerunnableIO m => MonadRerunnableIO (Except.ExceptT e m) where
  rerunnableIO = lift . rerunnableIO

instance MonadRerunnableIO m => MonadRerunnableIO (Identity.IdentityT m) where
  rerunnableIO = lift . rerunnableIO

instance MonadRerunnableIO m => MonadRerunnableIO (Maybe.MaybeT m) where
  rerunnableIO = lift . rerunnableIO

instance (Monoid w, MonadRerunnableIO m) => MonadRerunnableIO (RWS.Lazy.RWST r w s m) where
  rerunnableIO = lift . rerunnableIO

instance (Monoid w, MonadRerunnableIO m) => MonadRerunnableIO (RWS.Strict.RWST r w s m) where
  rerunnableIO = lift . rerunnableIO

instance MonadRerunnableIO m => MonadRerunnableIO (State.Lazy.StateT s m) where
  rerunnableIO = lift . rerunnableIO

instance MonadRerunnableIO m => MonadRerunnableIO (State.Strict.StateT s m) where
  rerunnableIO = lift . rerunnableIO

instance (Monoid w, MonadRerunnableIO m) => MonadRerunnableIO (Writer.Lazy.WriterT w m) where
  rerunnableIO = lift . rerunnableIO

instance (Monoid w, MonadRerunnableIO m) => MonadRerunnableIO (Writer.Strict.WriterT w m) where
  rerunnableIO = lift . rerunnableIO

instance MonadRerunnableIO m => MonadRerunnableIO (Resource.ResourceT m) where
  rerunnableIO = lift . rerunnableIO
