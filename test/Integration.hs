module Integration where

import qualified Data.Map.Strict as Map
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
  [ testWithTransaction
  , testPersistentAPI
  ]

testWithTransaction :: TestTree
testWithTransaction = testGroup "withTransaction"
  [ testCase "withTransaction uses the same transaction" $ do
      let catchTestError m = do
            result <- try m
            liftIO $ result @?= Left TestError

          insertAndFail :: TestApp ()
          insertAndFail = insert_ (person "Alice") >> throwIO TestError

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
  ]

testPersistentAPI :: TestTree
testPersistentAPI = testGroup "Persistent API"
  [ testCase "get" $ do
      result <- runTestApp $ do
        insert_ $ person "Alice"
        mapM get [1, 2]
      map (fmap personName) result @?= [Just "Alice", Nothing]

  , testCase "getMany" $ do
      result <- runTestApp $ do
        insert_ $ person "Alice"
        getMany [1]
      personName <$> Map.lookup 1 result @?= Just "Alice"

  , testCase "getJust" $ do
      result <- runTestApp $ do
        insert_ $ person "Alice"
        getJust 1
      personName result @?= "Alice"

  , testCase "getJustEntity" $ do
      result <- runTestApp $ do
        insert_ $ person "Alice"
        getJustEntity 1
      getName result @?= "Alice"

  , testCase "getEntity" $ do
      result <- runTestApp $ do
        insert_ $ person "Alice"
        mapM getEntity [1, 2]
      map (fmap getName) result @?= [Just "Alice", Nothing]

  , testCase "belongsTo" $ do
      result <- runTestApp $ do
        aliceKey <- insert $ person "Alice"
        let post1 = Post "Post #1" aliceKey (Just aliceKey)
            post2 = Post "Post #2" aliceKey Nothing
        insertMany_ [post1, post2]
        mapM (belongsTo postEditor) [post1, post2]
      map (fmap personName) result @?= [Just "Alice", Nothing]

  , testCase "belongsToJust" $ do
      result <- runTestApp $ do
        aliceKey <- insert $ person "Alice"
        let post1 = Post "Post #1" aliceKey Nothing
        insert_ post1
        belongsToJust postAuthor post1
      personName result @?= "Alice"

  , testCase "selectList" $ do
      result <- runTestApp $ do
        insert_ $ person "Alice"
        insert_ $ person "Bob"
        selectList [] []
      map getName result @?= ["Alice", "Bob"]
  ]
