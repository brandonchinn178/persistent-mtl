module Integration where

import Database.Persist (Entity(..))
import Test.Tasty
import Test.Tasty.HUnit

import Database.Persist.Monad
import Example

tests :: TestTree
tests = testGroup "Integration tests"
  [ testCase "getPeople" $ do
      let alice = Person "Alice" Nothing
          bob = Person "Bob" (Just 20)

      result <- runTestApp $ do
        insert_ alice
        insert_ bob

        getPeople

      map entityVal result @?= [alice, bob]
  ]
