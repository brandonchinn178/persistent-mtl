{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

module Mocked where

import Database.Persist (Entity(..))
import Database.Persist.Sql (toSqlKey)
import Test.Tasty
import Test.Tasty.HUnit
import UnliftIO (SomeException, try)

import Database.Persist.Monad
import Database.Persist.Monad.TestUtils
import Example

tests :: TestTree
tests = testGroup "Mocked tests"
  [ testCase "withTransaction doesn't error" $
      runMockSqlQueryT (withTransaction $ insert_ $ Person "Alice" 10)
        [ withRecord @Person $ \case
            Insert_ _ -> Just ()
            _ -> Nothing
        ]
  , testCase "MockSqlQueryT errors if it could not find a mock" $ do
      result <- try $ runMockSqlQueryT getPeopleNames []
      case result of
        Right _ -> assertFailure "runMockSqlQueryT did not fail"
        Left e -> do
          let msg = head $ lines $ show (e :: SomeException)
          msg @?= "Could not find mock for query: SelectList{..}<Person>"
  , testCase "MockSqlQueryT continues after a mock doesn't match" $ do
      result <- runMockSqlQueryT getPeopleNames
        [ withRecord @Post $ \_ -> error "getPeopleNames matched Post record"
        , mockQuery $ \_ -> Nothing
        , withRecord @Person $ \case
            SelectList _ _ -> Just
              [ Entity (toSqlKey 1) (Person "Alice" 10)
              , Entity (toSqlKey 2) (Person "Bob" 20)
              ]
            _ -> Nothing
        ]

      result @?= ["Alice", "Bob"]
  , testPersistentAPI
  ]

testPersistentAPI :: TestTree
testPersistentAPI = testGroup "Persistent API"
  [ testCase "selectList" $ do
      result <- runMockSqlQueryT (selectList [] [])
        [ withRecord @Person $ \case
            SelectList _ _ -> Just
              [ Entity (toSqlKey 1) (Person "Alice" 10)
              , Entity (toSqlKey 2) (Person "Bob" 20)
              ]
            _ -> Nothing
        ]

      map getName result @?= ["Alice", "Bob"]
  ]
