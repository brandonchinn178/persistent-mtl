{-|
Module: Control.Monad.Trans.Rerunnable

Defines the 'MonadRerunnableTrans' type class that is functionally equivalent
to 'Control.Monad.Trans.Class.MonadTrans', but use of it requires the user to
explicitly acknowledge that lifting the given action can be rerun.
-}
module Control.Monad.Trans.Rerunnable (
  MonadRerunnableTrans (..),
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

{-| A copy of 'Control.Monad.Trans.Class.MonadTrans' to explicitly allow only
lifting actions that are rerunnable, e.g. in the context of a SQL transaction.
-}
class MonadRerunnableTrans t where
  -- | Lift the given action.
  --
  -- The given action may be rerun, so use of this function requires
  -- manually verifying that the given action is rerunnable.
  rerunnableLift :: Monad m => m a -> t m a

{- Instances for common monad transformers -}

instance MonadRerunnableTrans (Reader.ReaderT r) where
  rerunnableLift = lift

instance MonadRerunnableTrans (Except.ExceptT e) where
  rerunnableLift = lift

instance MonadRerunnableTrans Identity.IdentityT where
  rerunnableLift = lift

instance MonadRerunnableTrans Maybe.MaybeT where
  rerunnableLift = lift

instance Monoid w => MonadRerunnableTrans (RWS.Lazy.RWST r w s) where
  rerunnableLift = lift

instance Monoid w => MonadRerunnableTrans (RWS.Strict.RWST r w s) where
  rerunnableLift = lift

instance MonadRerunnableTrans (State.Lazy.StateT s) where
  rerunnableLift = lift

instance MonadRerunnableTrans (State.Strict.StateT s) where
  rerunnableLift = lift

instance Monoid w => MonadRerunnableTrans (Writer.Lazy.WriterT w) where
  rerunnableLift = lift

instance Monoid w => MonadRerunnableTrans (Writer.Strict.WriterT w) where
  rerunnableLift = lift

instance MonadRerunnableTrans Resource.ResourceT where
  rerunnableLift = lift
