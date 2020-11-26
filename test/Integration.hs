module Integration where

import Test.Tasty
import Test.Tasty.HUnit

import Database.Persist.Monad
import Example

tests :: TestTree
tests = testGroup "Integration tests"
  [ exampleFunctions
  ]

-- | Each test in list should correspond with Mocked.exampleFunctions
exampleFunctions :: TestTree
exampleFunctions = testGroup "Functions from example"
  [ testCase "getPeopleNames" $ do
      result <- runTestApp $ do
        insert_ $ Person "Alice" 10
        insert_ $ Person "Bob" 20
        getPeopleNames

      result @?= ["Alice", "Bob"]
  ]
