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
  [ exampleFunctions
  ]

-- | Each test in list should correspond with Mocked.exampleFunctions
exampleFunctions :: TestTree
exampleFunctions = testGroup "Functions from example"
  [ testCase "getPeopleNames" $ do
      result <- runMockSqlQueryT getPeopleNames
        [ withRecord @Person $ \case
            SelectList _ _ -> Just
              [ Entity (toSqlKey 1) (Person "Alice" 10)
              , Entity (toSqlKey 2) (Person "Bob" 20)
              ]
            _ -> Nothing
        ]

      result @?= ["Alice", "Bob"]
  ]
