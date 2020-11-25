{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Database.Persist.Monad.TestUtils
  ( MockSqlQueryT
  , runMockSqlQueryT
  , withRecord
  ) where

import Control.Monad (msum)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Data.Typeable (Typeable, eqT, (:~:)(..))

import Database.Persist.Monad.Class (MonadSqlQuery(..))
import Database.Persist.Monad.SqlQueryRep (SqlQueryRep)

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

  withTransaction = id

data MockQuery = MockQuery (forall record a. Typeable record => SqlQueryRep record a -> Maybe a)

withRecord :: forall record. Typeable record => (forall a. SqlQueryRep record a -> Maybe a) -> MockQuery
withRecord f = MockQuery $ \(rep :: SqlQueryRep someRecord result) ->
  case eqT @record @someRecord of
    Just Refl -> f rep
    Nothing -> Nothing
