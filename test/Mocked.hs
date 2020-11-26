{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

module Mocked where

import Database.Persist (Entity(..))
import Database.Persist.Sql (toSqlKey)
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
      runMockSqlQueryT (withTransaction $ insert_ $ Person "Alice" 10)
        [ withRecord @Person $ \case
            Insert_ _ -> Just ()
            _ -> Nothing
        ]
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
