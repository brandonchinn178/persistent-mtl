module Integration where

import Control.Arrow ((&&&))
import qualified Data.Map.Strict as Map
import Database.Persist (Entity(..), (=.))
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
  [ testCase "it uses the same transaction" $ do
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

  , testCase "insert" $ do
      result <- runTestApp $ do
        aliceKey <- insert $ person "Alice"
        people <- getPeopleNames
        return (aliceKey, people)
      result @?= (1, ["Alice"])

  , testCase "insert_" $ do
      result <- runTestApp $ do
        result <- insert_ $ person "Alice"
        people <- getPeopleNames
        return (result, people)
      result @?= ((), ["Alice"])

  , testCase "insertMany" $ do
      result <- runTestApp $ do
        keys <- insertMany [person "Alice", person "Bob"]
        people <- getPeopleNames
        return (keys, people)
      result @?= ([1, 2], ["Alice", "Bob"])

  , testCase "insertMany_" $ do
      result <- runTestApp $ do
        result <- insertMany_ [person "Alice", person "Bob"]
        people <- getPeopleNames
        return (result, people)
      result @?= ((), ["Alice", "Bob"])

  , testCase "insertEntityMany" $ do
      result <- runTestApp $ do
        result <- insertEntityMany
          [ Entity 1 $ person "Alice"
          , Entity 2 $ person "Bob"
          ]
        people <- getPeopleNames
        return (result, people)
      result @?= ((), ["Alice", "Bob"])

  , testCase "insertKey" $ do
      result <- runTestApp $ do
        result <- insertKey 1 $ person "Alice"
        people <- getPeopleNames
        return (result, people)
      result @?= ((), ["Alice"])

  , testCase "repsert" $ do
      result <- runTestApp $ do
        let alice = person "Alice"
        insert_ alice
        repsert 1 $ alice { personAge = 100 }
        repsert 2 $ person "Bob"
        getPeople
      map (personName &&& personAge) result @?=
        [ ("Alice", 100)
        , ("Bob", 0)
        ]

  , testCase "repsertMany" $ do
      result <- runTestApp $ do
        let alice = person "Alice"
        insert_ alice
        repsertMany
          [ (1, alice { personAge = 100 })
          , (2, person "Bob")
          ]
        getPeople
      map (personName &&& personAge) result @?=
        [ ("Alice", 100)
        , ("Bob", 0)
        ]

  , testCase "replace" $ do
      result <- runTestApp $ do
        let alice = person "Alice"
        insert_ alice
        replace 1 $ alice { personAge = 100 }
        getJust 1
      personAge result @?= 100

  , testCase "delete" $ do
      result <- runTestApp $ do
        aliceKey <- insert $ person "Alice"
        delete aliceKey
        getPeople
      result @?= []

  , testCase "update" $ do
      result <- runTestApp $ do
        key <- insert $ person "Alice"
        update key [PersonName =. "Alicia"]
        getPeopleNames
      result @?= ["Alicia"]

  , testCase "updateGet" $ do
      (updateResult, getResult) <- runTestApp $ do
        key <- insert $ person "Alice"
        updateResult <- updateGet key [PersonName =. "Alicia"]
        getResult <- getJust key
        return (updateResult, getResult)
      updateResult @?= getResult

  , testCase "insertEntity" $ do
      (insertResult, getResult) <- runTestApp $ do
        insertResult <- insertEntity $ person "Alice"
        getResult <- getJust $ entityKey insertResult
        return (insertResult, getResult)
      entityVal insertResult @?= getResult

  , testCase "insertRecord" $ do
      (insertResult, getResult) <- runTestApp $ do
        insertResult <- insertRecord $ person "Alice"
        getResult <- getJust 1
        return (insertResult, getResult)
      insertResult @?= getResult

  , testCase "selectList" $ do
      result <- runTestApp $ do
        insert_ $ person "Alice"
        insert_ $ person "Bob"
        selectList [] []
      map getName result @?= ["Alice", "Bob"]
  ]
