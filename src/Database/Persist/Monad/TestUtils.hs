{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

{-|
Module: Database.Persist.Monad.TestUtils

Defines 'MockSqlQueryT', which one can use in tests in order to mock out
@persistent@ database queries called in production code.
-}
module Database.Persist.Monad.TestUtils (
  MockSqlQueryT,
  runMockSqlQueryT,
  withRecord,
  mockQuery,
  MockQuery,

  -- * Specialized helpers
  mockSelectSource,
  mockSelectKeys,
  mockWithRawQuery,
  mockRawQuery,
  mockRawSql,

  -- * Re-exports
  SqlQueryRep (..),
) where

import Conduit ((.|))
import qualified Conduit
import Control.Monad (msum)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Control.Monad.Trans.Resource (MonadResource)
import qualified Data.Acquire as Acquire
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Typeable (Typeable, eqT, (:~:) (..))
import Database.Persist.Sql (
  Entity,
  Filter,
  Key,
  PersistValue,
  SelectOpt,
  rawSqlProcessRow,
 )

import Database.Persist.Monad.Class (MonadSqlQuery (..))
import Database.Persist.Monad.SqlQueryRep (SqlQueryRep (..))

-- | A monad transformer for testing functions that use 'MonadSqlQuery'.
newtype MockSqlQueryT m a = MockSqlQueryT
  { unMockSqlQueryT :: ReaderT [MockQuery] m a
  }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadResource
    )

-- | Runs a 'MockSqlQueryT' monad transformer using the given mocks.
--
--  When a database query is executed, the first mock that returns a 'Just' is
--  returned. If no mocks match the query, an error is thrown. See 'SqlQueryRep'
--  for the constructors available to match against. Most of the time, you'll
--  want to use 'withRecord' to only match queries against a specific @record@
--  type (e.g. only match 'Database.Persist.Monad.Shim.selectList' calls for
--  the @Person@ entity).
--
--  Usage:
--
--  @
--  myFunction :: MonadSqlQuery m => m [String]
--  myFunction = map personName <$> selectList [PersonAge >. 25] []
--
--  let persons = [Person ...]
--  result <- runMockSqlQueryT myFunction
--    [ withRecord \@Person $ \\case
--        SelectList _ _ -> Just persons
--        _ -> Nothing
--    , withRecord \@Post $ \\case
--        Insert Post{ name = "post1" } -> Just $ toSqlKey 1
--        _ -> Nothing
--    , mockQuery $ \\case
--        RawExecuteCount "DELETE FROM person WHERE name = \'Alice\'" [] -> Just 1
--        _ -> Nothing
--    ]
--  @
runMockSqlQueryT :: MockSqlQueryT m a -> [MockQuery] -> m a
runMockSqlQueryT action mockQueries = (`runReaderT` mockQueries) . unMockSqlQueryT $ action

instance (MonadIO m) => MonadSqlQuery (MockSqlQueryT m) where
  type TransactionM (MockSqlQueryT m) = MockSqlQueryT m

  runQueryRep rep = do
    mockQueries <- MockSqlQueryT ask
    maybe (error $ "Could not find mock for query: " ++ show rep) liftIO $
      msum $
        map tryMockQuery mockQueries
    where
      tryMockQuery (MockQuery f) = f rep

  withTransaction = id

-- | A mocked query to use in 'runMockSqlQueryT'.
--
--  Use 'withRecord' or another helper to create a 'MockQuery'.
data MockQuery = MockQuery (forall record a. (Typeable record) => SqlQueryRep record a -> Maybe (IO a))

-- | A helper for defining a mocked database query against a specific @record@
--  type. Designed to be used with TypeApplications.
--
--  Most 'SqlQueryRep' constructors are in the context of a specific @record@
--  type, like @Person@. This helper only matches mocked database queries that
--  are querying the record you specify.
--
--  Some constructors reference multiple @record@ types, like
--  'Database.Persist.Monad.BelongsTo'. Look at the type to see the record you
--  need to match against. For example,
--
--  @
--  withRecord \@(Person, Post) $ \\case
--    BelongsTo _ _ -> ...
--  @
--
--  would match the function call
--
--  @
--  belongsTo :: (Person -> Maybe (Key Post)) -> Person -> SqlQueryRep (Person, Post) (Maybe Post)
--  @
withRecord :: forall record. (Typeable record) => (forall a. SqlQueryRep record a -> Maybe a) -> MockQuery
withRecord f = MockQuery $ \(rep :: SqlQueryRep someRecord result) ->
  case eqT @record @someRecord of
    Just Refl -> pure <$> f rep
    Nothing -> Nothing

-- | A helper for defining a mocked database query.
--
--  This does not do any matching on the @record@ type, so it is mostly useful
--  for queries that don't use the @record@ type, like
--  'Database.Persist.Monad.Shim.rawExecute'.
mockQuery :: (forall record a. (Typeable record) => SqlQueryRep record a -> Maybe a) -> MockQuery
mockQuery f = MockQuery (fmap pure . f)

-- | A helper for mocking a 'Database.Persist.Monad.Shim.selectSource' or
--  'Database.Persist.Monad.Shim.selectSourceRes' call.
--
--  Usage:
--
--  @
--  mockSelectSource $ \\filters opts ->
--    if null filters && null opts
--      then
--        let person1 = [Entity (toSqlKey 1) $ Person \"Alice\"]
--            person2 = [Entity (toSqlKey 2) $ Person \"Bob\"]
--        in Just [person1, person2]
--      else Nothing
--  @
mockSelectSource :: forall record. (Typeable record) => ([Filter record] -> [SelectOpt record] -> Maybe [Entity record]) -> MockQuery
mockSelectSource f = withRecord @record $ \case
  SelectSourceRes filters opts ->
    let toAcquire entities = Acquire.mkAcquire (pure $ Conduit.yieldMany entities) (\_ -> pure ())
     in toAcquire <$> f filters opts
  _ -> Nothing

-- | A helper for mocking a 'Database.Persist.Monad.Shim.selectKeys' or
--  'Database.Persist.Monad.Shim.selectKeysRes' call.
--
--  Usage:
--
--  @
--  mockSelectKeys $ \\filters opts ->
--    if null filters && null opts
--      then Just $ map toSqlKey [1, 2]
--      else Nothing
--  @
mockSelectKeys :: forall record. (Typeable record) => ([Filter record] -> [SelectOpt record] -> Maybe [Key record]) -> MockQuery
mockSelectKeys f = withRecord @record $ \case
  SelectKeysRes filters opts ->
    let toAcquire keys = Acquire.mkAcquire (pure $ Conduit.yieldMany keys) (\_ -> pure ())
     in toAcquire <$> f filters opts
  _ -> Nothing

-- | A helper for mocking a 'Database.Persist.Monad.Shim.withRawQuery' call.
--
--  Usage:
--
--  @
--  mockWithRawQuery $ \\sql vals ->
--    if sql == "SELECT id, name FROM person"
--      then
--        let row1 = [toPersistValue 1, toPersistValue \"Alice\"]
--            row2 = [toPersistValue 2, toPersistValue \"Bob\"]
--        in Just [row1, row2]
--      else Nothing
--  @
mockWithRawQuery :: (Text -> [PersistValue] -> Maybe [[PersistValue]]) -> MockQuery
mockWithRawQuery f = MockQuery $ \case
  WithRawQuery sql vals conduit ->
    let outputRows rows = Conduit.runConduit $ Conduit.yieldMany rows .| conduit
     in outputRows <$> f sql vals
  _ -> Nothing

-- | A helper for mocking a 'Database.Persist.Monad.Shim.rawQuery' or
--  'Database.Persist.Monad.Shim.rawQueryRes' call.
--
--  Usage:
--
--  @
--  mockRawQuery $ \\sql vals ->
--    if sql == "SELECT id, name FROM person"
--      then
--        let row1 = [toPersistValue 1, toPersistValue \"Alice\"]
--            row2 = [toPersistValue 2, toPersistValue \"Bob\"]
--        in Just [row1, row2]
--      else Nothing
--  @
mockRawQuery :: (Text -> [PersistValue] -> Maybe [[PersistValue]]) -> MockQuery
mockRawQuery f = MockQuery $ \case
  RawQueryRes sql vals ->
    let toAcquire rows = Acquire.mkAcquire (pure $ Conduit.yieldMany rows) (\_ -> pure ())
     in pure . toAcquire <$> f sql vals
  _ -> Nothing

-- | A helper for mocking a 'Database.Persist.Monad.Shim.rawSql' call.
--
--  Usage:
--
--  @
--  mockRawSql $ \\sql vals ->
--    if sql == "SELECT id, name FROM person"
--      then
--        let row1 = [toPersistValue 1, toPersistValue \"Alice\"]
--            row2 = [toPersistValue 2, toPersistValue \"Bob\"]
--        in Just [row1, row2]
--      else Nothing
--  @
mockRawSql :: (Text -> [PersistValue] -> Maybe [[PersistValue]]) -> MockQuery
mockRawSql f = MockQuery $ \case
  RawSql sql vals ->
    let fromRow = either (error . Text.unpack) id . rawSqlProcessRow
     in pure . map fromRow <$> f sql vals
  _ -> Nothing
