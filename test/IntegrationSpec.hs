{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module IntegrationSpec (spec) where

import Conduit (runConduit, (.|))
import qualified Conduit
import Control.Arrow ((&&&))
import Control.Monad (forM_)
import qualified Data.Acquire as Acquire
import Data.Bifunctor (first)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Typeable (Typeable)
import qualified Database.Esqueleto.Experimental as E
import Database.Persist.Sql (
  Entity (..),
  IsolationLevel (..),
  Migration,
  PersistField,
  PersistRecordBackend,
  PersistValue,
  Single (..),
  SqlBackend,
  fromPersistValue,
  (=.),
  (==.),
 )
import Skeletest
import qualified Skeletest.Predicate as P
import UnliftIO (MonadIO, MonadUnliftIO, liftIO)
import UnliftIO.Exception (
  Exception,
  SomeException,
  StringException (..),
  fromException,
  throwIO,
  throwString,
  try,
 )
import UnliftIO.IORef (atomicModifyIORef, newIORef, readIORef, writeIORef)

import Control.Monad.IO.Rerunnable (MonadRerunnableIO, rerunnableIO)
import Database.Persist.Monad
import Database.Persist.Monad.Internal.PersistentShim (SafeToInsert)
import Example
import TestUtils.DB (BackendType (..), allBackendTypes)
import TestUtils.Esqueleto (esqueletoSelect)

spec :: Spec
spec = do
  forM_ allBackendTypes $ \backendType ->
    describe (show backendType) $ do
      describe "withTransaction" $ do
        it "uses the same transaction" $ do
          -- without transactions, the INSERT shouldn't be rolled back
          resultWithoutTransactions <-
            runTestApp backendType $ do
              catchTestError $ insertAndFail $ person "Alice"
              getPeopleNames
          resultWithoutTransactions `shouldBe` ["Alice"]

          -- with transactions, the INSERT should be rolled back
          resultWithTransactions <-
            runTestApp backendType $ do
              catchTestError $ withTransaction $ insertAndFail $ person "Alice"
              getPeopleNames
          resultWithTransactions `shouldBe` []

        it "retries transactions" $ do
          let retryIf e = case fromException e of
                Just (StringException "retry me" _) -> True
                _ -> False
              setRetry env = env{retryIf, retryLimit = 5}

          counter <- newIORef (0 :: Int)

          (`shouldNotSatisfy` P.throws (P.anything @_ @SomeException)) $
            runTestAppWith backendType setRetry $
              withTransaction $
                rerunnableIO $ do
                  x <- atomicModifyIORef counter $ \x -> (x + 1, x)
                  if x > 2
                    then return ()
                    else throwString "retry me"

        it "throws error when retry hits limit" $ do
          let setRetry env = env{retryIf = const True, retryLimit = 2}

          result <-
            try @_ @TransactionError @() $
              runTestAppWith backendType setRetry $
                withTransaction $
                  rerunnableIO $
                    throwString "retry me"

          result `shouldBe` Left RetryLimitExceeded

        it "runs retryCallback" $ do
          callbackRef <- newIORef Nothing

          let setRetry env =
                env
                  { retryIf = const True
                  , retryLimit = 2
                  , retryCallback = writeIORef callbackRef . Just
                  }
          _ <-
            try @_ @TransactionError @() $
              runTestAppWith backendType setRetry . withTransaction $
                rerunnableIO (throwIO TestError)

          mError <- readIORef callbackRef
          (mError >>= fromException) `shouldBe` Just TestError

      describe "catchSqlTransaction" $ do
        let newCatchRef = do
              wasCaughtRef <- newIORef False
              let markCaught (_ :: SomeException) =
                    rerunnableIO $ writeIORef wasCaughtRef True
                  getWasCaught = readIORef wasCaughtRef
              pure (markCaught, getWasCaught)

        it "catches errors" $ do
          (markCaught, getWasCaught) <- newCatchRef
          runTestApp backendType . withTransaction $
            (`catchSqlTransaction` markCaught) $
              rerunnableIO (throwString "error")
          getWasCaught `shouldSatisfy` P.returns (P.eq True)

        it "does not catch retry errors" $ do
          let retryIf e = case fromException e of
                Just (StringException "retry me" _) -> True
                _ -> False
              setRetry env = env{retryIf, retryLimit = 2}

          (markCaught, getWasCaught) <- newCatchRef
          _ <-
            try @_ @SomeException $
              runTestAppWith backendType setRetry . withTransaction $
                (`catchSqlTransaction` markCaught) $
                  rerunnableIO (throwString "retry me")
          getWasCaught `shouldSatisfy` P.returns (P.eq False)

      -- should compile
      it "can compose operations" $ do
        let onlySql :: (MonadSqlQuery m) => m ()
            onlySql = do
              _ <- getPeople
              return ()

            sqlAndRerunnableIO :: (MonadSqlQuery m, MonadRerunnableIO m) => m ()
            sqlAndRerunnableIO = do
              _ <- getPeopleNames
              _ <- rerunnableIO $ newIORef True
              return ()

            onlyRerunnableIO :: (MonadRerunnableIO m) => m ()
            onlyRerunnableIO = do
              _ <- rerunnableIO $ newIORef True
              return ()

            arbitraryIO :: (MonadIO m) => m ()
            arbitraryIO = do
              _ <- liftIO $ newIORef True
              return ()

        -- everything should compose naturally by default
        runTestApp backendType $ do
          onlySql
          sqlAndRerunnableIO
          onlyRerunnableIO
          arbitraryIO

        -- in a transaction, you can compose everything except arbitrary IO
        runTestApp backendType $ withTransaction $ do
          onlySql
          sqlAndRerunnableIO
          onlyRerunnableIO
          -- uncomment this to get compile error
          -- arbitraryIO
          pure ()

      describe "Persistent API" $ do
        it "get" $ do
          result <- runTestApp backendType $ do
            insert_ $ person "Alice"
            mapM get [1, 2]
          map (fmap personName) result `shouldBe` [Just "Alice", Nothing]

        it "getMany" $ do
          result <- runTestApp backendType $ do
            insert_ $ person "Alice"
            getMany [1]
          personName <$> Map.lookup 1 result `shouldBe` Just "Alice"

        it "getJust" $ do
          result <- runTestApp backendType $ do
            insert_ $ person "Alice"
            getJust 1
          personName result `shouldBe` "Alice"

        it "getJustEntity" $ do
          result <- runTestApp backendType $ do
            insert_ $ person "Alice"
            getJustEntity 1
          getName result `shouldBe` "Alice"

        it "getEntity" $ do
          result <- runTestApp backendType $ do
            insert_ $ person "Alice"
            mapM getEntity [1, 2]
          map (fmap getName) result `shouldBe` [Just "Alice", Nothing]

        it "belongsTo" $ do
          result <- runTestApp backendType $ do
            aliceKey <- insert $ person "Alice"
            let post1 = Post "Post #1" aliceKey (Just aliceKey)
                post2 = Post "Post #2" aliceKey Nothing
            insertMany_ [post1, post2]
            mapM (belongsTo postEditor) [post1, post2]
          map (fmap personName) result `shouldBe` [Just "Alice", Nothing]

        it "belongsToJust" $ do
          result <- runTestApp backendType $ do
            aliceKey <- insert $ person "Alice"
            let post1 = Post "Post #1" aliceKey Nothing
            insert_ post1
            belongsToJust postAuthor post1
          personName result `shouldBe` "Alice"

        it "insert" $ do
          result <- runTestApp backendType $ do
            aliceKey <- insert $ person "Alice"
            people <- getPeopleNames
            return (aliceKey, people)
          result `shouldBe` (1, ["Alice"])

        it "insert_" $ do
          result <- runTestApp backendType $ do
            result <- insert_ $ person "Alice"
            people <- getPeopleNames
            return (result, people)
          result `shouldBe` ((), ["Alice"])

        it "insertMany" $ do
          result <- runTestApp backendType $ do
            keys <- insertMany [person "Alice", person "Bob"]
            people <- getPeopleNames
            return (keys, people)
          result `shouldBe` ([1, 2], ["Alice", "Bob"])

        it "insertMany_" $ do
          result <- runTestApp backendType $ do
            result <- insertMany_ [person "Alice", person "Bob"]
            people <- getPeopleNames
            return (result, people)
          result `shouldBe` ((), ["Alice", "Bob"])

        it "insertEntityMany" $ do
          result <- runTestApp backendType $ do
            result <-
              insertEntityMany
                [ Entity 1 $ person "Alice"
                , Entity 2 $ person "Bob"
                ]
            people <- getPeopleNames
            return (result, people)
          result `shouldBe` ((), ["Alice", "Bob"])

        it "insertKey" $ do
          result <- runTestApp backendType $ do
            result <- insertKey 1 $ person "Alice"
            people <- getPeopleNames
            return (result, people)
          result `shouldBe` ((), ["Alice"])

        it "repsert" $ do
          result <- runTestApp backendType $ do
            let alice = person "Alice"
            insert_ alice
            repsert 1 $ alice{personAge = 100}
            repsert 2 $ person "Bob"
            getPeople
          map nameAndAge result
            `shouldBe` [ ("Alice", 100)
                       , ("Bob", 0)
                       ]

        it "repsertMany" $ do
          result <- runTestApp backendType $ do
            let alice = person "Alice"
            -- https://github.com/yesodweb/persistent/issues/832
            insert_ alice
            repsertMany
              [ (1, alice{personAge = 100})
              , (2, person "Bob")
              ]
            getPeople
          map nameAndAge result
            `shouldBe` [ ("Alice", 100)
                       , ("Bob", 0)
                       ]

        it "replace" $ do
          result <- runTestApp backendType $ do
            let alice = person "Alice"
            insert_ alice
            replace 1 $ alice{personAge = 100}
            getJust 1
          personAge result `shouldBe` 100

        it "delete" $ do
          result <- runTestApp backendType $ do
            aliceKey <- insert $ person "Alice"
            delete aliceKey
            getPeople
          result `shouldBe` []

        it "update" $ do
          result <- runTestApp backendType $ do
            key <- insert $ person "Alice"
            update key [PersonName =. "Alicia"]
            getPeopleNames
          result `shouldBe` ["Alicia"]

        it "updateGet" $ do
          (updateResult, getResult) <- runTestApp backendType $ do
            key <- insert $ person "Alice"
            updateResult <- updateGet key [PersonName =. "Alicia"]
            getResult <- getJust key
            return (updateResult, getResult)
          updateResult `shouldBe` getResult

        it "insertEntity" $ do
          (insertResult, getResult) <- runTestApp backendType $ do
            insertResult <- insertEntity $ person "Alice"
            getResult <- getJust $ entityKey insertResult
            return (insertResult, getResult)
          entityVal insertResult `shouldBe` getResult

        it "insertRecord" $ do
          (insertResult, getResult) <- runTestApp backendType $ do
            insertResult <- insertRecord $ person "Alice"
            getResult <- getJust 1
            return (insertResult, getResult)
          insertResult `shouldBe` getResult

        it "getBy" $ do
          result <- runTestApp backendType $ do
            insert_ $ person "Alice"
            mapM getBy [UniqueName "Alice", UniqueName "Bob"]
          map (fmap getName) result `shouldBe` [Just "Alice", Nothing]

        it "getByValue" $ do
          result <- runTestApp backendType $ do
            let alice = person "Alice"
            insert_ alice
            mapM getByValue [alice, person "Bob"]
          map (fmap getName) result `shouldBe` [Just "Alice", Nothing]

        it "checkUnique" $ do
          result <- runTestApp backendType $ do
            let alice = person "Alice"
            insert_ alice
            mapM
              checkUnique
              [ alice
              , person "Bob"
              , (person "Alice"){personAge = 100}
              ]
          result `shouldBe` [Just (UniqueName "Alice"), Nothing, Just (UniqueName "Alice")]

        it "checkUniqueUpdateable" $ do
          result <- runTestApp backendType $ do
            let alice = person "Alice"
            insert_ alice
            mapM
              checkUniqueUpdateable
              [ Entity 1 alice
              , Entity 2 $ person "Bob"
              , Entity 3 $ (person "Alice"){personAge = 100}
              ]
          result `shouldBe` [Nothing, Nothing, Just (UniqueName "Alice")]

        it "deleteBy" $ do
          result <- runTestApp backendType $ do
            insert_ $ person "Alice"
            deleteBy $ UniqueName "Alice"
            getPeople
          result `shouldBe` []

        it "insertUnique" $ do
          (result1, result2, people) <- runTestApp backendType $ do
            result1 <- insertUnique $ person "Alice"
            result2 <- insertUnique $ person "Alice"
            people <- getPeopleNames
            return (result1, result2, people)
          result1 `shouldBe` Just 1
          result2 `shouldBe` Nothing
          people `shouldBe` ["Alice"]

        it "upsert" $ do
          (result1, result2, people) <- runTestApp backendType $ do
            result1 <- upsert (person "Alice") [PersonAge =. 0]
            result2 <- upsert (person "Alice") [PersonAge =. 100]
            people <- getPeople
            return (result1, result2, people)
          entityKey result1 `shouldBe` entityKey result2
          nameAndAge (entityVal result1) `shouldBe` ("Alice", 0)
          nameAndAge (entityVal result2) `shouldBe` ("Alice", 100)
          map nameAndAge people `shouldBe` [("Alice", 100)]

        it "upsertBy" $ do
          (result1, result2, people) <- runTestApp backendType $ do
            result1 <- upsertBy (UniqueName "Alice") (person "Alice") [PersonAge =. 0]
            result2 <- upsertBy (UniqueName "Alice") (person "Alice") [PersonAge =. 100]
            people <- getPeople
            return (result1, result2, people)
          entityKey result1 `shouldBe` entityKey result2
          nameAndAge (entityVal result1) `shouldBe` ("Alice", 0)
          nameAndAge (entityVal result2) `shouldBe` ("Alice", 100)
          map nameAndAge people `shouldBe` [("Alice", 100)]

        it "putMany" $ do
          result <- runTestApp backendType $ do
            let alice = person "Alice"
            insert_ alice
            putMany
              [ alice{personAge = 100}
              , person "Bob"
              ]
            getPeople
          map nameAndAge result
            `shouldBe` [ ("Alice", 100)
                       , ("Bob", 0)
                       ]

        it "insertBy" $ do
          (result1, result2, people) <- runTestApp backendType $ do
            let alice = person "Alice"
            result1 <- insertBy alice
            result2 <- insertBy $ alice{personAge = 100}
            people <- getPeople
            return (result1, result2, people)
          result1 `shouldBe` Right 1
          first (entityKey &&& getName) result2 `shouldBe` Left (1, "Alice")
          map nameAndAge people `shouldBe` [("Alice", 0)]

        it "insertUniqueEntity" $ do
          (result1, result2, people) <- runTestApp backendType $ do
            let alice = person "Alice"
            result1 <- insertUniqueEntity alice
            result2 <- insertUniqueEntity $ alice{personAge = 100}
            people <- getPeople
            return (result1, result2, people)
          (entityKey &&& getName) <$> result1 `shouldBe` Just (1, "Alice")
          result2 `shouldBe` Nothing
          map nameAndAge people `shouldBe` [("Alice", 0)]

        it "replaceUnique" $ do
          (result1, result2, people) <- runTestApp backendType $ do
            let alice = person "Alice"
                bob = person "Bob"
            insertMany_ [alice, bob]
            result1 <- replaceUnique 1 $ alice{personName = "Bob"}
            result2 <- replaceUnique 2 $ bob{personAge = 100}
            people <- getPeople
            return (result1, result2, people)
          result1 `shouldBe` Just (UniqueName "Bob")
          result2 `shouldBe` Nothing
          map nameAndAge people `shouldBe` [("Alice", 0), ("Bob", 100)]

        it "onlyUnique" $ do
          result <- runTestApp backendType $ onlyUnique $ person "Alice"
          result `shouldBe` UniqueName "Alice"

        it "selectSourceRes" $ do
          result <- runTestApp backendType $ do
            insertMany_ [person "Alice", person "Bob"]
            acquire <- selectSourceRes [] []
            Acquire.with acquire $ \conduit ->
              runConduit $ conduit .| Conduit.mapC getName .| Conduit.sinkList
          result `shouldBe` ["Alice", "Bob"]

        it "selectFirst" $ do
          result <- runTestApp backendType $ do
            insert_ $ person "Alice"
            sequence
              [ selectFirst [PersonName ==. "Alice"] []
              , selectFirst [PersonName ==. "Bob"] []
              ]
          map (fmap getName) result `shouldBe` [Just "Alice", Nothing]

        it "selectKeysRes" $ do
          result <- runTestApp backendType $ do
            insertMany_ [person "Alice", person "Bob"]
            acquire <- selectKeysRes @_ @Person [] []
            Acquire.with acquire $ \conduit ->
              runConduit $ conduit .| Conduit.sinkList
          result `shouldBe` [1, 2]

        it "count" $ do
          result <- runTestApp backendType $ do
            insertMany_ $ map (\p -> p{personAge = 100}) [person "Alice", person "Bob"]
            count [PersonAge ==. 100]
          result `shouldBe` 2

        it "exists" $ do
          result <- runTestApp backendType $ do
            insertMany_ [person "Alice", person "Bob"]
            exists [PersonName ==. "Alice"]
          result `shouldBe` True

        it "selectSource" $ do
          result <- runTestApp backendType $ do
            insertMany_ [person "Alice", person "Bob"]
            runConduit $ selectSource [] [] .| Conduit.mapC getName .| Conduit.sinkList
          result `shouldBe` ["Alice", "Bob"]

        it "selectKeys" $ do
          result <- runTestApp backendType $ do
            insertMany_ [person "Alice", person "Bob"]
            runConduit $ selectKeys @Person [] [] .| Conduit.sinkList
          result `shouldBe` [1, 2]

        it "selectList" $ do
          result <- runTestApp backendType $ do
            insert_ $ person "Alice"
            insert_ $ person "Bob"
            selectList [] []
          map getName result `shouldBe` ["Alice", "Bob"]

        it "selectKeysList" $ do
          result <- runTestApp backendType $ do
            insert_ $ person "Alice"
            insert_ $ person "Bob"
            selectKeysList @Person [] []
          result `shouldBe` [1, 2]

        it "updateWhere" $ do
          result <- runTestApp backendType $ do
            insertMany_ [person "Alice", person "Bob"]
            updateWhere [PersonName ==. "Alice"] [PersonAge =. 100]
            getPeople
          map nameAndAge result `shouldBe` [("Alice", 100), ("Bob", 0)]

        it "deleteWhere" $ do
          result <- runTestApp backendType $ do
            insertMany_ [person "Alice", person "Bob"]
            deleteWhere [PersonName ==. "Alice"]
            getPeopleNames
          result `shouldBe` ["Bob"]

        it "updateWhereCount" $ do
          (rowsUpdated, people) <- runTestApp backendType $ do
            insertMany_ [person "Alice", person "Bob"]
            rowsUpdated <- updateWhereCount [PersonName ==. "Alice"] [PersonAge =. 100]
            people <- getPeople
            return (rowsUpdated, people)
          rowsUpdated `shouldBe` 1
          map nameAndAge people `shouldBe` [("Alice", 100), ("Bob", 0)]

        it "deleteWhereCount" $ do
          (rowsDeleted, names) <- runTestApp backendType $ do
            insertMany_ [person "Alice", person "Bob"]
            rowsDeleted <- deleteWhereCount [PersonName ==. "Alice"]
            names <- getPeopleNames
            return (rowsDeleted, names)
          rowsDeleted `shouldBe` 1
          names `shouldBe` ["Bob"]

        it "parseMigration" $ do
          result <- runTestApp backendType $ do
            setupUnsafeMigration
            parseMigration migration

          let sql = case backendType of
                Sqlite ->
                  [ P.eq
                      ( False
                      , Text.concat
                          [ "CREATE TEMP TABLE \"person_backup\"("
                          , "\"id\" INTEGER PRIMARY KEY,"
                          , "\"name\" VARCHAR NOT NULL,"
                          , "\"age\" INTEGER NOT NULL,"
                          , "CONSTRAINT \"unique_name\" UNIQUE (\"name\"))"
                          ]
                      )
                  , P.anything
                  , P.eq (True, "DROP TABLE \"person\"")
                  , P.anything
                  , P.anything
                  , P.eq (False, "DROP TABLE \"person_backup\"")
                  ]
                Postgresql ->
                  [ P.eq (True, "ALTER TABLE \"person\" DROP COLUMN \"foo\"")
                  ]

          result `shouldSatisfy` P.right (P.list sql)

        it "parseMigration'" $ do
          let action :: (Migration -> TestApp a) -> IO a
              action f = runTestApp backendType $ do
                setupUnsafeMigration
                f migration

          result <- action parseMigration
          result' <- action parseMigration'
          Right result' `shouldBe` result

        it "printMigration" $
          runTestApp backendType $ do
            setupUnsafeMigration
            printMigration migration

        it "showMigration" $ do
          result <- runTestApp backendType $ do
            setupUnsafeMigration
            showMigration migration

          let sql = case backendType of
                Sqlite ->
                  [ P.eq $
                      Text.concat
                        [ "CREATE TEMP TABLE \"person_backup\"("
                        , "\"id\" INTEGER PRIMARY KEY,"
                        , "\"name\" VARCHAR NOT NULL,"
                        , "\"age\" INTEGER NOT NULL,"
                        , "CONSTRAINT \"unique_name\" UNIQUE (\"name\"));"
                        ]
                  , P.anything
                  , P.eq "DROP TABLE \"person\";"
                  , P.anything
                  , P.anything
                  , P.eq "DROP TABLE \"person_backup\";"
                  ]
                Postgresql ->
                  [ P.eq "ALTER TABLE \"person\" DROP COLUMN \"foo\";"
                  ]

          result `shouldSatisfy` P.list sql

        it "getMigration" $ do
          result <- runTestApp backendType $ do
            setupUnsafeMigration
            getMigration migration

          let sql = case backendType of
                Sqlite ->
                  [ P.eq $
                      Text.concat
                        [ "CREATE TEMP TABLE \"person_backup\"("
                        , "\"id\" INTEGER PRIMARY KEY,"
                        , "\"name\" VARCHAR NOT NULL,"
                        , "\"age\" INTEGER NOT NULL,"
                        , "CONSTRAINT \"unique_name\" UNIQUE (\"name\"))"
                        ]
                  , P.anything
                  , P.eq "DROP TABLE \"person\""
                  , P.anything
                  , P.anything
                  , P.eq "DROP TABLE \"person_backup\""
                  ]
                Postgresql ->
                  [ P.eq "ALTER TABLE \"person\" DROP COLUMN \"foo\""
                  ]

          result `shouldSatisfy` P.list sql

        it "runMigration" $ do
          result <- runTestApp backendType $ do
            setupSafeMigration
            runMigration migration
            getSchemaColumnNames backendType "person"
          result `shouldNotSatisfy` P.elem "removed_column"

        it "runMigrationQuiet" $ do
          (withQuiet, cols) <- runTestApp backendType $ do
            setupSafeMigration
            sql <- runMigrationQuiet migration
            cols <- getSchemaColumnNames backendType "person"
            return (sql, cols)
          withSilent <- runTestApp backendType $ do
            setupSafeMigration
            runMigrationSilent migration
          cols `shouldNotSatisfy` P.elem "removed_column"
          withQuiet `shouldBe` withSilent

        it "runMigrationSilent" $ do
          (sqlPlanned, sqlExecuted, cols) <- runTestApp backendType $ do
            setupSafeMigration
            sqlPlanned <- getMigration migration
            sqlExecuted <- runMigrationSilent migration
            cols <- getSchemaColumnNames backendType "person"
            return (sqlPlanned, sqlExecuted, cols)
          cols `shouldNotSatisfy` P.elem "removed_column"
          sqlExecuted `shouldBe` sqlPlanned

        it "runMigrationUnsafe" $ do
          result <- runTestApp backendType $ do
            setupUnsafeMigration
            runMigrationUnsafe migration
            getSchemaColumnNames backendType "person"
          result `shouldNotSatisfy` P.elem "removed_column"

        it "runMigrationUnsafeQuiet" $ do
          (sqlPlanned, sqlExecuted, cols) <- runTestApp backendType $ do
            setupUnsafeMigration
            sqlPlanned <- getMigration migration
            sqlExecuted <- runMigrationUnsafeQuiet migration
            cols <- getSchemaColumnNames backendType "person"
            return (sqlPlanned, sqlExecuted, cols)
          cols `shouldNotSatisfy` P.elem "removed_column"
          sqlExecuted `shouldBe` sqlPlanned

        it "getFieldName" $ do
          result <-
            runTestApp backendType $
              getFieldName PersonName
          result `shouldBe` "\"name\""

        it "getTableName" $ do
          result <-
            runTestApp backendType $
              getTableName $
                person "Alice"
          result `shouldBe` "\"person\""

        it "withRawQuery" $ do
          result <- runTestApp backendType $ do
            insertMany_ [person "Alice", person "Bob"]
            withRawQuery "SELECT name FROM person" [] $
              Conduit.mapC (getFirstPersistValue @Text) .| Conduit.sinkList

          result `shouldBe` ["Alice", "Bob"]

        it "rawQueryRes" $ do
          result <- runTestApp backendType $ do
            insertMany_ [person "Alice", person "Bob"]
            acquire <- rawQueryRes "SELECT name FROM person" []
            Acquire.with acquire $ \conduit ->
              runConduit $ conduit .| Conduit.mapC (getFirstPersistValue @Text) .| Conduit.sinkList
          result `shouldBe` ["Alice", "Bob"]

        it "rawQuery" $ do
          result <- runTestApp backendType $ do
            insertMany_ [person "Alice", person "Bob"]
            runConduit $ rawQuery "SELECT name FROM person" [] .| Conduit.mapC (getFirstPersistValue @Text) .| Conduit.sinkList
          result `shouldBe` ["Alice", "Bob"]

        it "rawExecute" $ do
          result <- runTestApp backendType $ do
            insertMany_ [person "Alice", person "Bob"]
            rawExecute "UPDATE person SET age = 100 WHERE name = 'Alice'" []
            getPeople
          map nameAndAge result `shouldBe` [("Alice", 100), ("Bob", 0)]

        it "rawExecuteCount" $ do
          (rowsUpdated, people) <- runTestApp backendType $ do
            insertMany_ [person "Alice", person "Bob"]
            rowsUpdated <- rawExecuteCount "UPDATE person SET age = 100 WHERE name = 'Alice'" []
            people <- getPeople
            return (rowsUpdated, people)
          rowsUpdated `shouldBe` 1
          map nameAndAge people `shouldBe` [("Alice", 100), ("Bob", 0)]

        it "rawSql" $ do
          result <- runTestApp backendType $ do
            insertMany_ [person "Alice", person "Bob"]
            rawSql @(Single String) "SELECT name FROM person" []
          map unSingle result `shouldBe` ["Alice", "Bob"]

        it "transactionSave" $ do
          result1 <- runTestApp backendType $ do
            catchTestError $ withTransaction $ do
              insert_ $ person "Alice"
              insertAndFail $ person "Bob"
            getPeopleNames
          result1 `shouldBe` []

          result2 <- runTestApp backendType $ do
            catchTestError $ withTransaction $ do
              insert_ $ person "Alice"
              transactionSave
              insertAndFail $ person "Bob"
            getPeopleNames
          result2 `shouldBe` ["Alice"]

        it "transactionSaveWithIsolation" $ do
          result1 <- runTestApp backendType $ do
            catchTestError $ withTransaction $ do
              insert_ $ person "Alice"
              insertAndFail $ person "Bob"
            getPeopleNames
          result1 `shouldBe` []

          result2 <- runTestApp backendType $ do
            catchTestError $ withTransaction $ do
              insert_ $ person "Alice"
              transactionSaveWithIsolation Serializable
              insertAndFail $ person "Bob"
            getPeopleNames
          result2 `shouldBe` ["Alice"]

        it "transactionUndo" $ do
          result <- runTestApp backendType $ withTransaction $ do
            insert_ $ person "Alice"
            transactionUndo
            getPeopleNames
          result `shouldBe` []

        it "transactionUndoWithIsolation" $ do
          result <- runTestApp backendType $ withTransaction $ do
            insert_ $ person "Alice"
            transactionUndoWithIsolation Serializable
            getPeopleNames
          result `shouldBe` []

      describe "Interop with third-party Persistent libraries" $ do
        it "unsafeLiftSql" $ do
          let alice = person "Alice"
          result <- runTestApp backendType $ do
            insert_ alice
            esqueletoSelect $
              E.from $
                E.table @Person
          result `shouldBe` [Entity 1 alice]

{- Persistent helpers -}

getFirstPersistValue :: (PersistField a) => [PersistValue] -> a
getFirstPersistValue = \case
  [] -> error "Unexpectedly got no values"
  v : _ -> fromPersistValueOrFail v

fromPersistValueOrFail :: (PersistField a) => PersistValue -> a
fromPersistValueOrFail = either (error . Text.unpack) id . fromPersistValue

{- Meta SQL helpers -}

-- | Put the database in a state where running a migration is safe.
setupSafeMigration :: (MonadSqlQuery m) => m ()
setupSafeMigration = rawExecute "ALTER TABLE person ADD COLUMN removed_column VARCHAR" []

-- | Put the database in a state where running a migration is unsafe.
setupUnsafeMigration :: (MonadSqlQuery m) => m ()
setupUnsafeMigration = rawExecute "ALTER TABLE person ADD COLUMN foo VARCHAR" []

-- | Get the names of all columns in the given table.
getSchemaColumnNames :: (MonadSqlQuery m) => BackendType -> String -> m [String]
getSchemaColumnNames backendType tableName = map unSingle <$> rawSql sql []
  where
    sql = Text.pack $ case backendType of
      Sqlite -> "SELECT name FROM pragma_table_info('" ++ tableName ++ "')"
      Postgresql ->
        unlines
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
  liftIO $ result `shouldBe` Left TestError

insertAndFail ::
  ( MonadRerunnableIO m
  , MonadSqlQuery m
  , PersistRecordBackend record SqlBackend
  , Typeable record
  , SafeToInsert record
  ) =>
  record
  -> m ()
insertAndFail record = do
  insert_ record
  rerunnableIO $ throwIO TestError
