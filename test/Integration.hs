{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Integration where

import Conduit (runConduit, (.|))
import qualified Conduit
import Control.Arrow ((&&&))
import qualified Data.Acquire as Acquire
import Data.Bifunctor (first)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Typeable (Typeable)
import Database.Persist.Sql
    ( Entity(..)
    , Migration
    , PersistField
    , PersistRecordBackend
    , PersistValue
    , Single(..)
    , SqlBackend
    , fromPersistValue
    , (=.)
    , (==.)
    )
#if MIN_VERSION_persistent(2,9,0)
import Database.Persist.Sql (IsolationLevel(..))
#endif
import Test.Tasty
import Test.Tasty.HUnit
import UnliftIO (Exception, MonadUnliftIO, liftIO, throwIO, try)

import Control.Monad.IO.Rerunnable (MonadRerunnableIO, rerunnableIO)
import Database.Persist.Monad
import Example
import TestUtils.DB (BackendType(..), allBackendTypes)
import TestUtils.Match (Match(..), (@?~))

tests :: TestTree
tests = testGroup "Integration tests" $
  map testsWithBackend allBackendTypes

testsWithBackend :: BackendType -> TestTree
testsWithBackend backendType = testGroup (show backendType)
  [ testWithTransaction backendType
  , testPersistentAPI backendType
  ]

testWithTransaction :: BackendType -> TestTree
testWithTransaction backendType = testGroup "withTransaction"
  [ testCase "it uses the same transaction" $ do
      -- without transactions, the INSERT shouldn't be rolled back
      runTestApp backendType $ do
        catchTestError $ insertAndFail $ person "Alice"
        result <- getPeopleNames
        liftIO $ result @?= ["Alice"]

      -- with transactions, the INSERT should be rolled back
      runTestApp backendType $ do
        catchTestError $ withTransaction $ insertAndFail $ person "Alice"
        result <- getPeopleNames
        liftIO $ result @?= []
  ]

testPersistentAPI :: BackendType -> TestTree
testPersistentAPI backendType = testGroup "Persistent API"
  [ testCase "get" $ do
      result <- runTestApp backendType $ do
        insert_ $ person "Alice"
        mapM get [1, 2]
      map (fmap personName) result @?= [Just "Alice", Nothing]

  , testCase "getMany" $ do
      result <- runTestApp backendType $ do
        insert_ $ person "Alice"
        getMany [1]
      personName <$> Map.lookup 1 result @?= Just "Alice"

  , testCase "getJust" $ do
      result <- runTestApp backendType $ do
        insert_ $ person "Alice"
        getJust 1
      personName result @?= "Alice"

  , testCase "getJustEntity" $ do
      result <- runTestApp backendType $ do
        insert_ $ person "Alice"
        getJustEntity 1
      getName result @?= "Alice"

  , testCase "getEntity" $ do
      result <- runTestApp backendType $ do
        insert_ $ person "Alice"
        mapM getEntity [1, 2]
      map (fmap getName) result @?= [Just "Alice", Nothing]

  , testCase "belongsTo" $ do
      result <- runTestApp backendType $ do
        aliceKey <- insert $ person "Alice"
        let post1 = Post "Post #1" aliceKey (Just aliceKey)
            post2 = Post "Post #2" aliceKey Nothing
        insertMany_ [post1, post2]
        mapM (belongsTo postEditor) [post1, post2]
      map (fmap personName) result @?= [Just "Alice", Nothing]

  , testCase "belongsToJust" $ do
      result <- runTestApp backendType $ do
        aliceKey <- insert $ person "Alice"
        let post1 = Post "Post #1" aliceKey Nothing
        insert_ post1
        belongsToJust postAuthor post1
      personName result @?= "Alice"

  , testCase "insert" $ do
      result <- runTestApp backendType $ do
        aliceKey <- insert $ person "Alice"
        people <- getPeopleNames
        return (aliceKey, people)
      result @?= (1, ["Alice"])

  , testCase "insert_" $ do
      result <- runTestApp backendType $ do
        result <- insert_ $ person "Alice"
        people <- getPeopleNames
        return (result, people)
      result @?= ((), ["Alice"])

  , testCase "insertMany" $ do
      result <- runTestApp backendType $ do
        keys <- insertMany [person "Alice", person "Bob"]
        people <- getPeopleNames
        return (keys, people)
      result @?= ([1, 2], ["Alice", "Bob"])

  , testCase "insertMany_" $ do
      result <- runTestApp backendType $ do
        result <- insertMany_ [person "Alice", person "Bob"]
        people <- getPeopleNames
        return (result, people)
      result @?= ((), ["Alice", "Bob"])

  , testCase "insertEntityMany" $ do
      result <- runTestApp backendType $ do
        result <- insertEntityMany
          [ Entity 1 $ person "Alice"
          , Entity 2 $ person "Bob"
          ]
        people <- getPeopleNames
        return (result, people)
      result @?= ((), ["Alice", "Bob"])

  , testCase "insertKey" $ do
      result <- runTestApp backendType $ do
        result <- insertKey 1 $ person "Alice"
        people <- getPeopleNames
        return (result, people)
      result @?= ((), ["Alice"])

  , testCase "repsert" $ do
      result <- runTestApp backendType $ do
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
      result <- runTestApp backendType $ do
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
      result <- runTestApp backendType $ do
        let alice = person "Alice"
        insert_ alice
        replace 1 $ alice { personAge = 100 }
        getJust 1
      personAge result @?= 100

  , testCase "delete" $ do
      result <- runTestApp backendType $ do
        aliceKey <- insert $ person "Alice"
        delete aliceKey
        getPeople
      result @?= []

  , testCase "update" $ do
      result <- runTestApp backendType $ do
        key <- insert $ person "Alice"
        update key [PersonName =. "Alicia"]
        getPeopleNames
      result @?= ["Alicia"]

  , testCase "updateGet" $ do
      (updateResult, getResult) <- runTestApp backendType $ do
        key <- insert $ person "Alice"
        updateResult <- updateGet key [PersonName =. "Alicia"]
        getResult <- getJust key
        return (updateResult, getResult)
      updateResult @?= getResult

  , testCase "insertEntity" $ do
      (insertResult, getResult) <- runTestApp backendType $ do
        insertResult <- insertEntity $ person "Alice"
        getResult <- getJust $ entityKey insertResult
        return (insertResult, getResult)
      entityVal insertResult @?= getResult

  , testCase "insertRecord" $ do
      (insertResult, getResult) <- runTestApp backendType $ do
        insertResult <- insertRecord $ person "Alice"
        getResult <- getJust 1
        return (insertResult, getResult)
      insertResult @?= getResult

  , testCase "getBy" $ do
      result <- runTestApp backendType $ do
        insert_ $ person "Alice"
        mapM getBy [UniqueName "Alice", UniqueName "Bob"]
      map (fmap getName) result @?= [Just "Alice", Nothing]

  , testCase "getByValue" $ do
      result <- runTestApp backendType $ do
        let alice = person "Alice"
        insert_ alice
        mapM getByValue [alice, person "Bob"]
      map (fmap getName) result @?= [Just "Alice", Nothing]

  , testCase "checkUnique" $ do
      result <- runTestApp backendType $ do
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
      result <- runTestApp backendType $ do
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
      result <- runTestApp backendType $ do
        insert_ $ person "Alice"
        deleteBy $ UniqueName "Alice"
        getPeople
      result @?= []

  , testCase "insertUnique" $ do
      (result1, result2, people) <- runTestApp backendType $ do
        result1 <- insertUnique $ person "Alice"
        result2 <- insertUnique $ person "Alice"
        people <- getPeopleNames
        return (result1, result2, people)
      result1 @?= Just 1
      result2 @?= Nothing
      people @?= ["Alice"]

  , testCase "upsert" $ do
      (result1, result2, people) <- runTestApp backendType $ do
        result1 <- upsert (person "Alice") [PersonAge =. 0]
        result2 <- upsert (person "Alice") [PersonAge =. 100]
        people <- getPeople
        return (result1, result2, people)
      entityKey result1 @?= entityKey result2
      nameAndAge (entityVal result1) @?= ("Alice", 0)
      nameAndAge (entityVal result2) @?= ("Alice", 100)
      map nameAndAge people @?= [("Alice", 100)]

  , testCase "upsertBy" $ do
      (result1, result2, people) <- runTestApp backendType $ do
        result1 <- upsertBy (UniqueName "Alice") (person "Alice") [PersonAge =. 0]
        result2 <- upsertBy (UniqueName "Alice") (person "Alice") [PersonAge =. 100]
        people <- getPeople
        return (result1, result2, people)
      entityKey result1 @?= entityKey result2
      nameAndAge (entityVal result1) @?= ("Alice", 0)
      nameAndAge (entityVal result2) @?= ("Alice", 100)
      map nameAndAge people @?= [("Alice", 100)]

  , testCase "putMany" $ do
      result <- runTestApp backendType $ do
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
      (result1, result2, people) <- runTestApp backendType $ do
        let alice = person "Alice"
        result1 <- insertBy alice
        result2 <- insertBy $ alice { personAge = 100 }
        people <- getPeople
        return (result1, result2, people)
      result1 @?= Right 1
      first (entityKey &&& getName) result2 @?= Left (1, "Alice")
      map nameAndAge people @?= [("Alice", 0)]

  , testCase "insertUniqueEntity" $ do
      (result1, result2, people) <- runTestApp backendType $ do
        let alice = person "Alice"
        result1 <- insertUniqueEntity alice
        result2 <- insertUniqueEntity $ alice { personAge = 100 }
        people <- getPeople
        return (result1, result2, people)
      (entityKey &&& getName) <$> result1 @?= Just (1, "Alice")
      result2 @?= Nothing
      map nameAndAge people @?= [("Alice", 0)]

  , testCase "replaceUnique" $ do
      (result1, result2, people) <- runTestApp backendType $ do
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
      result <- runTestApp backendType $ onlyUnique $ person "Alice"
      result @?= UniqueName "Alice"

  , testCase "selectSourceRes" $ do
      result <- runTestApp backendType $ do
        insertMany_ [person "Alice", person "Bob"]
        acquire <- selectSourceRes [] []
        Acquire.with acquire $ \conduit ->
          runConduit $ conduit .| Conduit.mapC getName .| Conduit.sinkList
      result @?= ["Alice", "Bob"]

  , testCase "selectFirst" $ do
      result <- runTestApp backendType $ do
        insert_ $ person "Alice"
        sequence
          [ selectFirst [PersonName ==. "Alice"] []
          , selectFirst [PersonName ==. "Bob"] []
          ]
      map (fmap getName) result @?= [Just "Alice", Nothing]

  , testCase "selectKeysRes" $ do
      result <- runTestApp backendType $ do
        insertMany_ [person "Alice", person "Bob"]
        acquire <- selectKeysRes @_ @Person [] []
        Acquire.with acquire $ \conduit ->
          runConduit $ conduit .| Conduit.sinkList
      result @?= [1, 2]

  , testCase "count" $ do
      result <- runTestApp backendType $ do
        insertMany_ $ map (\p -> p{personAge = 100}) [person "Alice", person "Bob"]
        count [PersonAge ==. 100]
      result @?= 2

#if MIN_VERSION_persistent(2,11,0)
  , testCase "exists" $ do
      result <- runTestApp backendType $ do
        insertMany_ [person "Alice", person "Bob"]
        exists [PersonName ==. "Alice"]
      result @?= True
#endif

  , testCase "selectSource" $ do
      result <- runTestApp backendType $ do
        insertMany_ [person "Alice", person "Bob"]
        runConduit $ selectSource [] [] .| Conduit.mapC getName .| Conduit.sinkList
      result @?= ["Alice", "Bob"]

  , testCase "selectKeys" $ do
      result <- runTestApp backendType $ do
        insertMany_ [person "Alice", person "Bob"]
        runConduit $ selectKeys @Person [] [] .| Conduit.sinkList
      result @?= [1, 2]

  , testCase "selectList" $ do
      result <- runTestApp backendType $ do
        insert_ $ person "Alice"
        insert_ $ person "Bob"
        selectList [] []
      map getName result @?= ["Alice", "Bob"]

  , testCase "selectKeysList" $ do
      result <- runTestApp backendType $ do
        insert_ $ person "Alice"
        insert_ $ person "Bob"
        selectKeysList @Person [] []
      result @?= [1, 2]

  , testCase "updateWhere" $ do
      result <- runTestApp backendType $ do
        insertMany_ [person "Alice", person "Bob"]
        updateWhere [PersonName ==. "Alice"] [PersonAge =. 100]
        getPeople
      map nameAndAge result @?= [("Alice", 100), ("Bob", 0)]

  , testCase "deleteWhere" $ do
      result <- runTestApp backendType $ do
        insertMany_ [person "Alice", person "Bob"]
        deleteWhere [PersonName ==. "Alice"]
        getPeopleNames
      result @?= ["Bob"]

  , testCase "updateWhereCount" $ do
      (rowsUpdated, people) <- runTestApp backendType $ do
        insertMany_ [person "Alice", person "Bob"]
        rowsUpdated <- updateWhereCount [PersonName ==. "Alice"] [PersonAge =. 100]
        people <- getPeople
        return (rowsUpdated, people)
      rowsUpdated @?= 1
      map nameAndAge people @?= [("Alice", 100), ("Bob", 0)]

  , testCase "deleteWhereCount" $ do
      (rowsDeleted, names) <- runTestApp backendType $ do
        insertMany_ [person "Alice", person "Bob"]
        rowsDeleted <- deleteWhereCount [PersonName ==. "Alice"]
        names <- getPeopleNames
        return (rowsDeleted, names)
      rowsDeleted @?= 1
      names @?= ["Bob"]

  , testCase "deleteCascade" $ do
      (people, posts) <- runTestApp backendType $ do
        aliceKey <- insert $ person "Alice"
        bobKey <- insert $ person "Bob"
        insertMany_
          [ post "Post #1" aliceKey
          , post "Post #2" bobKey
          ]
        deleteCascade aliceKey
        people <- getPeopleNames
        posts <- getPostTitles
        return (people, posts)
      people @?= ["Bob"]
      posts @?= ["Post #2"]

  , testCase "deleteCascadeWhere" $ do
      (people, posts) <- runTestApp backendType $ do
        aliceKey <- insert $ person "Alice"
        bobKey <- insert $ person "Bob"
        insertMany_
          [ post "Post #1" aliceKey
          , post "Post #2" bobKey
          ]
        deleteCascadeWhere [PersonName ==. "Alice"]
        people <- getPeopleNames
        posts <- getPostTitles
        return (people, posts)
      people @?= ["Bob"]
      posts @?= ["Post #2"]

  , testCase "parseMigration" $ do
      result <- runTestApp backendType $ do
        setupUnsafeMigration
        parseMigration migration

      let sql = case backendType of
            Sqlite ->
              [ Match
                  ( False
                  , Text.concat
                      [ "CREATE TEMP TABLE \"person_backup\"("
                      , "\"id\" INTEGER PRIMARY KEY,"
                      , "\"name\" VARCHAR NOT NULL,"
                      , "\"age\" INTEGER NOT NULL,"
                      , "CONSTRAINT \"unique_name\" UNIQUE (\"name\"))"
                      ]
                  )
              , Anything
              , Match (True, "DROP TABLE \"person\"")
              , Anything
              , Anything
              , Match (False, "DROP TABLE \"person_backup\"")
              ]
            Postgresql ->
              [ Match (True, "ALTER TABLE \"person\" DROP COLUMN \"foo\"")
              ]

      result @?~ Right @[Text] sql

  , testCase "parseMigration'" $ do
      let action :: (Migration -> TestApp a) -> IO a
          action f = runTestApp backendType $ do
            setupUnsafeMigration
            f migration

      result <- action parseMigration
      result' <- action parseMigration'
      Right result' @?= result

  , testCase "printMigration" $
      runTestApp backendType $ do
        setupUnsafeMigration
        printMigration migration

  , testCase "showMigration" $ do
      result <- runTestApp backendType $ do
        setupUnsafeMigration
        showMigration migration

      let sql = case backendType of
            Sqlite ->
              [ Match $ Text.concat
                  [ "CREATE TEMP TABLE \"person_backup\"("
                  , "\"id\" INTEGER PRIMARY KEY,"
                  , "\"name\" VARCHAR NOT NULL,"
                  , "\"age\" INTEGER NOT NULL,"
                  , "CONSTRAINT \"unique_name\" UNIQUE (\"name\"));"
                  ]
              , Anything
              , Match "DROP TABLE \"person\";"
              , Anything
              , Anything
              , Match "DROP TABLE \"person_backup\";"
              ]
            Postgresql ->
              [ Match "ALTER TABLE \"person\" DROP COLUMN \"foo\";"
              ]

      result @?~ sql

  , testCase "getMigration" $ do
      result <- runTestApp backendType $ do
        setupUnsafeMigration
        getMigration migration

      let sql = case backendType of
            Sqlite ->
              [ Match $ Text.concat
                  [ "CREATE TEMP TABLE \"person_backup\"("
                  , "\"id\" INTEGER PRIMARY KEY,"
                  , "\"name\" VARCHAR NOT NULL,"
                  , "\"age\" INTEGER NOT NULL,"
                  , "CONSTRAINT \"unique_name\" UNIQUE (\"name\"))"
                  ]
              , Anything
              , Match "DROP TABLE \"person\""
              , Anything
              , Anything
              , Match "DROP TABLE \"person_backup\""
              ]
            Postgresql ->
              [ Match "ALTER TABLE \"person\" DROP COLUMN \"foo\""
              ]

      result @?~ sql

  , testCase "runMigration" $ do
      result <- runTestApp backendType $ do
        setupSafeMigration
        runMigration migration
        getSchemaColumnNames backendType "person"
      assertNotIn "removed_column" result

#if MIN_VERSION_persistent(2,10,2)
  , testCase "runMigrationQuiet" $ do
      (withQuiet, cols) <- runTestApp backendType $ do
        setupSafeMigration
        sql <- runMigrationQuiet migration
        cols <- getSchemaColumnNames backendType "person"
        return (sql, cols)
      withSilent <- runTestApp backendType $ do
        setupSafeMigration
        runMigrationSilent migration
      assertNotIn "removed_column" cols
      withQuiet @?= withSilent
#endif

  , testCase "runMigrationSilent" $ do
      (sqlPlanned, sqlExecuted, cols) <- runTestApp backendType $ do
        setupSafeMigration
        sqlPlanned <- getMigration migration
        sqlExecuted <- runMigrationSilent migration
        cols <- getSchemaColumnNames backendType "person"
        return (sqlPlanned, sqlExecuted, cols)
      assertNotIn "removed_column" cols
      sqlExecuted @?= sqlPlanned

  , testCase "runMigrationUnsafe" $ do
      result <- runTestApp backendType $ do
        setupUnsafeMigration
        runMigrationUnsafe migration
        getSchemaColumnNames backendType "person"
      assertNotIn "removed_column" result

#if MIN_VERSION_persistent(2,10,2)
  , testCase "runMigrationUnsafeQuiet" $ do
      (sqlPlanned, sqlExecuted, cols) <- runTestApp backendType $ do
        setupUnsafeMigration
        sqlPlanned <- getMigration migration
        sqlExecuted <- runMigrationUnsafeQuiet migration
        cols <- getSchemaColumnNames backendType "person"
        return (sqlPlanned, sqlExecuted, cols)
      assertNotIn "removed_column" cols
      sqlExecuted @?= sqlPlanned
#endif

  , testCase "getFieldName" $ do
      result <- runTestApp backendType $
        getFieldName PersonName
      result @?= "\"name\""

  , testCase "getTableName" $ do
      result <- runTestApp backendType $
        getTableName $ person "Alice"
      result @?= "\"person\""

  , testCase "withRawQuery" $ do
      result <- runTestApp backendType $ do
        insertMany_ [person "Alice", person "Bob"]
        withRawQuery "SELECT name FROM person" [] $
          Conduit.mapC (fromPersistValue' @Text . head) .| Conduit.sinkList

      result @?= ["Alice", "Bob"]

  , testCase "rawQueryRes" $ do
      result <- runTestApp backendType $ do
        insertMany_ [person "Alice", person "Bob"]
        acquire <- rawQueryRes "SELECT name FROM person" []
        Acquire.with acquire $ \conduit ->
          runConduit $ conduit .| Conduit.mapC (fromPersistValue' @Text . head) .| Conduit.sinkList
      result @?= ["Alice", "Bob"]

  , testCase "rawQuery" $ do
      result <- runTestApp backendType $ do
        insertMany_ [person "Alice", person "Bob"]
        runConduit $ rawQuery "SELECT name FROM person" [] .| Conduit.mapC (fromPersistValue' @Text . head) .| Conduit.sinkList
      result @?= ["Alice", "Bob"]

  , testCase "rawExecute" $ do
      result <- runTestApp backendType $ do
        insertMany_ [person "Alice", person "Bob"]
        rawExecute "UPDATE person SET age = 100 WHERE name = 'Alice'" []
        getPeople
      map nameAndAge result @?= [("Alice", 100), ("Bob", 0)]

  , testCase "rawExecuteCount" $ do
      (rowsUpdated, people) <- runTestApp backendType $ do
        insertMany_ [person "Alice", person "Bob"]
        rowsUpdated <- rawExecuteCount "UPDATE person SET age = 100 WHERE name = 'Alice'" []
        people <- getPeople
        return (rowsUpdated, people)
      rowsUpdated @?= 1
      map nameAndAge people @?= [("Alice", 100), ("Bob", 0)]

  , testCase "rawSql" $ do
      result <- runTestApp backendType $ do
        insertMany_ [person "Alice", person "Bob"]
        rawSql @(Single String) "SELECT name FROM person" []
      map unSingle result @?= ["Alice", "Bob"]

  , testCase "transactionSave" $ do
      result1 <- runTestApp backendType $ do
        catchTestError $ withTransaction $ do
          insert_ $ person "Alice"
          insertAndFail $ person "Bob"
        getPeopleNames
      result1 @?= []

      result2 <- runTestApp backendType $ do
        catchTestError $ withTransaction $ do
          insert_ $ person "Alice"
          transactionSave
          insertAndFail $ person "Bob"
        getPeopleNames
      result2 @?= ["Alice"]

#if MIN_VERSION_persistent(2,9,0)
  , testCase "transactionSaveWithIsolation" $ do
      result1 <- runTestApp backendType $ do
        catchTestError $ withTransaction $ do
          insert_ $ person "Alice"
          insertAndFail $ person "Bob"
        getPeopleNames
      result1 @?= []

      result2 <- runTestApp backendType $ do
        catchTestError $ withTransaction $ do
          insert_ $ person "Alice"
          transactionSaveWithIsolation Serializable
          insertAndFail $ person "Bob"
        getPeopleNames
      result2 @?= ["Alice"]
#endif

  , testCase "transactionUndo" $ do
      result <- runTestApp backendType $ withTransaction $ do
        insert_ $ person "Alice"
        transactionUndo
        getPeopleNames
      result @?= []

#if MIN_VERSION_persistent(2,9,0)
  , testCase "transactionUndoWithIsolation" $ do
      result <- runTestApp backendType $ withTransaction $ do
        insert_ $ person "Alice"
        transactionUndoWithIsolation Serializable
        getPeopleNames
      result @?= []
#endif
  ]

{- Persistent helpers -}

fromPersistValue' :: PersistField a => PersistValue -> a
fromPersistValue' = either (error . Text.unpack) id . fromPersistValue

{- Meta SQL helpers -}

-- | Put the database in a state where running a migration is safe.
setupSafeMigration :: MonadSqlQuery m => m ()
setupSafeMigration = rawExecute "ALTER TABLE person ADD COLUMN removed_column VARCHAR" []

-- | Put the database in a state where running a migration is unsafe.
setupUnsafeMigration :: MonadSqlQuery m => m ()
setupUnsafeMigration = rawExecute "ALTER TABLE person ADD COLUMN foo VARCHAR" []

-- | Get the names of all columns in the given table.
getSchemaColumnNames :: MonadSqlQuery m => BackendType -> String -> m [String]
getSchemaColumnNames backendType tableName = map unSingle <$> rawSql sql []
  where
    sql = Text.pack $ case backendType of
      Sqlite -> "SELECT name FROM pragma_table_info('" ++ tableName ++ "')"
      Postgresql -> unlines
        [ "SELECT column_name FROM information_schema.columns"
        , "WHERE table_schema = 'public' AND table_name = '" ++ tableName ++ "'"
        ]

{- Test helpers -}

data TestError = TestError
  deriving (Show, Eq)

instance Exception TestError

catchTestError :: (MonadUnliftIO m, Eq a, Show a) => m a -> m ()
catchTestError m = do
  result <- try m
  liftIO $ result @?= Left TestError

insertAndFail ::
  ( MonadRerunnableIO m
  , MonadSqlQuery m
  , PersistRecordBackend record SqlBackend
  , Typeable record
  )
  => record -> m ()
insertAndFail record = do
  insert_ record
  rerunnableIO $ throwIO TestError

assertNotIn :: (Eq a, Show a) => a -> [a] -> Assertion
assertNotIn a as = as @?= filter (/= a) as
