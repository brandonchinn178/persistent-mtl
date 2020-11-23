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
  [ testCase "getPeople" $ do
      let persons =
            [ Entity (toSqlKey 1) (Person "Alice" Nothing)
            , Entity (toSqlKey 2) (Person "Bob" (Just 20))
            ]

      result <- runMockSqlQueryT getPeople
        [ withRecord @Person $ \case
            SelectList _ _ -> Just persons
            _ -> Nothing
        ]

      result @?= persons
  ]
