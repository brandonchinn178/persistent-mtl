module Integration where

import Test.Tasty
import Test.Tasty.HUnit
import UnliftIO (Exception, liftIO, throwIO, try)

import Database.Persist.Monad
import Example

data TestError = TestError
  deriving (Show, Eq)

instance Exception TestError

tests :: TestTree
tests = testGroup "Integration tests"
  [ testCase "withTransaction uses the same transaction" $ do
      let catchTestError m = do
            result <- try m
            liftIO $ result @?= Left TestError

          insertAndFail :: TestApp ()
          insertAndFail = insert_ (Person "Alice" 0) >> throwIO TestError

      -- without transactions, the INSERT shouldn't be rolled back
      runTestApp $ do
        catchTestError insertAndFail
        result <- getPeopleNames
        liftIO $ result @?= ["Alice"]

      -- with transactions, the INSERT should be rolled back
      runTestApp $ do
        catchTestError $ withTransaction insertAndFail
        result <- getPeopleNames
        liftIO $ result @?= []

  , exampleFunctions
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
