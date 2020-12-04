{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeApplications #-}

module Integration where

import Conduit (runConduit, (.|))
import qualified Conduit
import Control.Arrow ((&&&))
import qualified Data.Acquire as Acquire
import Data.Bifunctor (first)
import qualified Data.Map.Strict as Map
import Database.Persist (Entity(..), (=.), (==.))
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
      map nameAndAge result @?=
        [ ("Alice", 100)
        , ("Bob", 0)
        ]

  , testCase "repsertMany" $ do
      result <- runTestApp $ do
        let alice = person "Alice"
-- https://github.com/yesodweb/persistent/issues/832
#if MIN_VERSION_persistent(2,9,0)
        insert_ alice
        repsertMany
          [ (1, alice { personAge = 100 })
          , (2, person "Bob")
          ]
#else
        repsertMany [(1, alice { personAge = 100 })]
        repsertMany [(2, person "Bob")]
#endif
        getPeople
      map nameAndAge result @?=
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

  , testCase "getBy" $ do
      result <- runTestApp $ do
        insert_ $ person "Alice"
        mapM getBy [UniqueName "Alice", UniqueName "Bob"]
      map (fmap getName) result @?= [Just "Alice", Nothing]

  , testCase "getByValue" $ do
      result <- runTestApp $ do
        let alice = person "Alice"
        insert_ alice
        mapM getByValue [alice, person "Bob"]
      map (fmap getName) result @?= [Just "Alice", Nothing]

  , testCase "checkUnique" $ do
      result <- runTestApp $ do
        let alice = person "Alice"
        insert_ alice
        mapM checkUnique
          [ alice
          , person "Bob"
          , (person "Alice"){ personAge = 100 }
          ]
      result @?= [Just (UniqueName "Alice"), Nothing, Just (UniqueName "Alice")]

#if MIN_VERSION_persistent(2,11,0)
  , testCase "checkUniqueUpdateable" $ do
      result <- runTestApp $ do
        let alice = person "Alice"
        insert_ alice
        mapM checkUniqueUpdateable
          [ Entity 1 alice
          , Entity 2 $ person "Bob"
          , Entity 3 $ (person "Alice"){ personAge = 100 }
          ]
      result @?= [Nothing, Nothing, Just (UniqueName "Alice")]
#endif

  , testCase "deleteBy" $ do
      result <- runTestApp $ do
        insert_ $ person "Alice"
        deleteBy $ UniqueName "Alice"
        getPeople
      result @?= []

  , testCase "insertUnique" $ do
      (result1, result2, people) <- runTestApp $ do
        result1 <- insertUnique $ person "Alice"
        result2 <- insertUnique $ person "Alice"
        people <- getPeopleNames
        return (result1, result2, people)
      result1 @?= Just 1
      result2 @?= Nothing
      people @?= ["Alice"]

  , testCase "upsert" $ do
      (result1, result2, people) <- runTestApp $ do
        result1 <- upsert (person "Alice") [PersonAge =. 0]
        result2 <- upsert (person "Alice") [PersonAge =. 100]
        people <- getPeople
        return (result1, result2, people)
      entityKey result1 @?= entityKey result2
      nameAndAge (entityVal result1) @?= ("Alice", 0)
      nameAndAge (entityVal result2) @?= ("Alice", 100)
      map nameAndAge people @?= [("Alice", 100)]

  , testCase "upsertBy" $ do
      (result1, result2, people) <- runTestApp $ do
        result1 <- upsertBy (UniqueName "Alice") (person "Alice") [PersonAge =. 0]
        result2 <- upsertBy (UniqueName "Alice") (person "Alice") [PersonAge =. 100]
        people <- getPeople
        return (result1, result2, people)
      entityKey result1 @?= entityKey result2
      nameAndAge (entityVal result1) @?= ("Alice", 0)
      nameAndAge (entityVal result2) @?= ("Alice", 100)
      map nameAndAge people @?= [("Alice", 100)]

  , testCase "putMany" $ do
      result <- runTestApp $ do
        let alice = person "Alice"
        insert_ alice
        putMany
          [ alice { personAge = 100 }
          , person "Bob"
          ]
        getPeople
      map nameAndAge result @?=
        [ ("Alice", 100)
        , ("Bob", 0)
        ]

  , testCase "insertBy" $ do
      (result1, result2, people) <- runTestApp $ do
        let alice = person "Alice"
        result1 <- insertBy alice
        result2 <- insertBy $ alice { personAge = 100 }
        people <- getPeople
        return (result1, result2, people)
      result1 @?= Right 1
      first (entityKey &&& getName) result2 @?= Left (1, "Alice")
      map nameAndAge people @?= [("Alice", 0)]

  , testCase "insertUniqueEntity" $ do
      (result1, result2, people) <- runTestApp $ do
        let alice = person "Alice"
        result1 <- insertUniqueEntity alice
        result2 <- insertUniqueEntity $ alice { personAge = 100 }
        people <- getPeople
        return (result1, result2, people)
      (entityKey &&& getName) <$> result1 @?= Just (1, "Alice")
      result2 @?= Nothing
      map nameAndAge people @?= [("Alice", 0)]

  , testCase "replaceUnique" $ do
      (result1, result2, people) <- runTestApp $ do
        let alice = person "Alice"
            bob = person "Bob"
        insertMany_ [alice, bob]
        result1 <- replaceUnique 1 $ alice { personName = "Bob" }
        result2 <- replaceUnique 2 $ bob { personAge = 100 }
        people <- getPeople
        return (result1, result2, people)
      result1 @?= Just (UniqueName "Bob")
      result2 @?= Nothing
      map nameAndAge people @?= [("Alice", 0), ("Bob", 100)]

  , testCase "onlyUnique" $ do
      result <- runTestApp $ onlyUnique $ person "Alice"
      result @?= UniqueName "Alice"

  , testCase "selectSourceRes" $ do
      result <- runTestApp $ do
        insertMany_ [person "Alice", person "Bob"]
        acquire <- selectSourceRes [] []
        Acquire.with acquire $ \conduit ->
          runConduit $ conduit .| Conduit.mapC getName .| Conduit.sinkList
      result @?= ["Alice", "Bob"]

  , testCase "selectFirst" $ do
      result <- runTestApp $ do
        insert_ $ person "Alice"
        sequence
          [ selectFirst [PersonName ==. "Alice"] []
          , selectFirst [PersonName ==. "Bob"] []
          ]
      map (fmap getName) result @?= [Just "Alice", Nothing]

  , testCase "selectKeysRes" $ do
      result <- runTestApp $ do
        insertMany_ [person "Alice", person "Bob"]
        acquire <- selectKeysRes @_ @Person [] []
        Acquire.with acquire $ \conduit ->
          runConduit $ conduit .| Conduit.sinkList
      result @?= [1, 2]

  , testCase "count" $ do
      result <- runTestApp $ do
        insertMany_ $ map (\p -> p{personAge = 100}) [person "Alice", person "Bob"]
        count [PersonAge ==. 100]
      result @?= 2

#if MIN_VERSION_persistent(2,11,0)
  , testCase "exists" $ do
      result <- runTestApp $ do
        insertMany_ [person "Alice", person "Bob"]
        exists [PersonName ==. "Alice"]
      result @?= True
#endif

  , testCase "selectSource" $ do
      result <- runTestApp $ do
        insertMany_ [person "Alice", person "Bob"]
        runConduit $ selectSource [] [] .| Conduit.mapC getName .| Conduit.sinkList
      result @?= ["Alice", "Bob"]

  , testCase "selectKeys" $ do
      result <- runTestApp $ do
        insertMany_ [person "Alice", person "Bob"]
        runConduit $ selectKeys @Person [] [] .| Conduit.sinkList
      result @?= [1, 2]

  , testCase "selectList" $ do
      result <- runTestApp $ do
        insert_ $ person "Alice"
        insert_ $ person "Bob"
        selectList [] []
      map getName result @?= ["Alice", "Bob"]

  , testCase "selectKeysList" $ do
      result <- runTestApp $ do
        insert_ $ person "Alice"
        insert_ $ person "Bob"
        selectKeysList @Person [] []
      result @?= [1, 2]

  , testCase "updateWhere" $ do
      result <- runTestApp $ do
        insertMany_ [person "Alice", person "Bob"]
        updateWhere [PersonName ==. "Alice"] [PersonAge =. 100]
        getPeople
      map nameAndAge result @?= [("Alice", 100), ("Bob", 0)]

  , testCase "deleteWhere" $ do
      result <- runTestApp $ do
        insertMany_ [person "Alice", person "Bob"]
        deleteWhere [PersonName ==. "Alice"]
        getPeopleNames
      result @?= ["Bob"]
  ]
