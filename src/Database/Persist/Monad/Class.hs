module Database.Persist.Monad.Class
  ( MonadSqlQuery(..)
  ) where

import Data.Typeable (Typeable)

import Database.Persist.Monad.SqlQueryRep (SqlQueryRep)

class MonadSqlQuery m where
  runQueryRep :: Typeable record => SqlQueryRep record a -> m a
  withTransaction :: m a -> m a
