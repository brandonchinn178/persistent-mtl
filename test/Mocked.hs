{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

module Mocked where

import qualified Data.Map.Strict as Map
import Database.Persist (Entity(..))
import Test.Tasty
import Test.Tasty.HUnit

import Database.Persist.Monad
import Database.Persist.Monad.TestUtils
import Example

tests :: TestTree
tests = testGroup "Mocked tests"
  [ testWithTransaction
  , testPersistentAPI
  ]

testWithTransaction :: TestTree
testWithTransaction = testGroup "withTransaction"
  [ testCase "withTransaction doesn't error" $
      runMockSqlQueryT (withTransaction $ insert_ $ person "Alice")
        [ withRecord @Person $ \case
            Insert_ _ -> Just ()
            _ -> Nothing
        ]
  ]

testPersistentAPI :: TestTree
testPersistentAPI = testGroup "Persistent API"
  [ testCase "get" $ do
      result <- runMockSqlQueryT (mapM get [1, 2])
        [ withRecord @Person $ \case
            Get n
              | n == 1 -> Just $ Just $ person "Alice"
              | n == 2 -> Just Nothing
            _ -> Nothing
        ]
      map (fmap personName) result @?= [Just "Alice", Nothing]

  , testCase "getMany" $ do
      result <- runMockSqlQueryT (getMany [1])
        [ withRecord @Person $ \case
            GetMany _ -> Just $ Map.fromList [(1, person "Alice")]
            _ -> Nothing
        ]
      personName <$> Map.lookup 1 result @?= Just "Alice"

  , testCase "getJust" $ do
      result <- runMockSqlQueryT (getJust 1)
        [ withRecord @Person $ \case
            GetJust _ -> Just $ person "Alice"
            _ -> Nothing
        ]
      personName result @?= "Alice"

  , testCase "getJustEntity" $ do
      result <- runMockSqlQueryT (getJustEntity 1)
        [ withRecord @Person $ \case
            GetJustEntity _ -> Just $ Entity 1 $ person "Alice"
            _ -> Nothing
        ]
      getName result @?= "Alice"

  , testCase "getEntity" $ do
      result <- runMockSqlQueryT (mapM getEntity [1, 2])
        [ withRecord @Person $ \case
            GetEntity n
              | n == 1 -> Just $ Just $ Entity 1 $ person "Alice"
              | n == 2 -> Just Nothing
            _ -> Nothing
        ]
      map (fmap getName) result @?= [Just "Alice", Nothing]

  , testCase "selectList" $ do
      result <- runMockSqlQueryT (selectList [] [])
        [ withRecord @Person $ \case
            SelectList _ _ -> Just
              [ Entity 1 (person "Alice")
              , Entity 2 (person "Bob")
              ]
            _ -> Nothing
        ]
      map getName result @?= ["Alice", "Bob"]
  ]
