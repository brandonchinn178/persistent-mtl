{- AUTOCOLLECT.TEST -}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Mocked (
  -- $AUTOCOLLECT.TEST.export$
) where

import Conduit (runConduit, runResourceT, (.|))
import qualified Conduit
import qualified Data.Acquire as Acquire
import qualified Data.Map.Strict as Map
import Database.Persist.Sql (
  Entity (..),
  Single (..),
  toPersistValue,
  (=.),
  (==.),
 )
import Test.Tasty
import Test.Tasty.HUnit

import Database.Persist.Monad
import Database.Persist.Monad.TestUtils
import Example

test =
  testGroup
    "withTransaction"
    [ testCase "it doesn't error with MockSqlQueryT" $
        runMockSqlQueryT
          (withTransaction $ insert_ $ person "Alice")
          [ withRecord @Person $ \case
              Insert_ _ -> Just ()
              _ -> Nothing
          ]
    ]

test =
  testGroup
    "Persistent API"
    [ testCase "get" $ do
        result <-
          runMockSqlQueryT
            (mapM get [1, 2])
            [ withRecord @Person $ \case
                Get n
                  | n == 1 -> Just $ Just $ person "Alice"
                  | n == 2 -> Just Nothing
                _ -> Nothing
            ]
        map (fmap personName) result @?= [Just "Alice", Nothing]
    , testCase "getMany" $ do
        result <-
          runMockSqlQueryT
            (getMany [1])
            [ withRecord @Person $ \case
                GetMany _ -> Just $ Map.fromList [(1, person "Alice")]
                _ -> Nothing
            ]
        personName <$> Map.lookup 1 result @?= Just "Alice"
    , testCase "getJust" $ do
        result <-
          runMockSqlQueryT
            (getJust 1)
            [ withRecord @Person $ \case
                GetJust _ -> Just $ person "Alice"
                _ -> Nothing
            ]
        personName result @?= "Alice"
    , testCase "getJustEntity" $ do
        result <-
          runMockSqlQueryT
            (getJustEntity 1)
            [ withRecord @Person $ \case
                GetJustEntity _ -> Just $ Entity 1 $ person "Alice"
                _ -> Nothing
            ]
        getName result @?= "Alice"
    , testCase "getEntity" $ do
        result <-
          runMockSqlQueryT
            (mapM getEntity [1, 2])
            [ withRecord @Person $ \case
                GetEntity n
                  | n == 1 -> Just $ Just $ Entity 1 $ person "Alice"
                  | n == 2 -> Just Nothing
                _ -> Nothing
            ]
        map (fmap getName) result @?= [Just "Alice", Nothing]
    , testCase "belongsTo" $ do
        let post1 = Post "Post #1" 1 (Just 1)
            post2 = Post "Post #2" 1 Nothing
        result <-
          runMockSqlQueryT
            (mapM (belongsTo postEditor) [post1, post2])
            [ withRecord @(Post, Person) $ \case
                BelongsTo _ Post{postEditor = Just 1} -> Just $ Just $ person "Alice"
                BelongsTo _ Post{postEditor = Nothing} -> Just Nothing
                _ -> Nothing
            ]
        map (fmap personName) result @?= [Just "Alice", Nothing]
    , testCase "belongsToJust" $ do
        let post1 = Post "Post #1" 1 Nothing
        result <-
          runMockSqlQueryT
            (belongsToJust postAuthor post1)
            [ withRecord @(Post, Person) $ \case
                BelongsToJust _ _ -> Just $ person "Alice"
                _ -> Nothing
            ]
        personName result @?= "Alice"
    , testCase "insert" $ do
        result <-
          runMockSqlQueryT
            (insert $ person "Alice")
            [ withRecord @Person $ \case
                Insert _ -> Just 1
                _ -> Nothing
            ]
        result @?= 1
    , testCase "insert_" $ do
        result <-
          runMockSqlQueryT
            (insert_ $ person "Alice")
            [ withRecord @Person $ \case
                Insert_ _ -> Just ()
                _ -> Nothing
            ]
        result @?= ()
    , testCase "insertMany" $ do
        result <-
          runMockSqlQueryT
            (insertMany [person "Alice", person "Bob"])
            [ withRecord @Person $ \case
                InsertMany records -> Just $ map fromIntegral [1 .. length records]
                _ -> Nothing
            ]
        result @?= [1, 2]
    , testCase "insertMany_" $ do
        result <-
          runMockSqlQueryT
            (insertMany_ [person "Alice", person "Bob"])
            [ withRecord @Person $ \case
                InsertMany_ _ -> Just ()
                _ -> Nothing
            ]
        result @?= ()
    , testCase "insertEntityMany" $ do
        result <-
          runMockSqlQueryT
            (insertEntityMany [Entity 1 $ person "Alice"])
            [ withRecord @Person $ \case
                InsertEntityMany _ -> Just ()
                _ -> Nothing
            ]
        result @?= ()
    , testCase "insertKey" $ do
        result <-
          runMockSqlQueryT
            (insertKey 1 $ person "Alice")
            [ withRecord @Person $ \case
                InsertKey _ _ -> Just ()
                _ -> Nothing
            ]
        result @?= ()
    , testCase "repsert" $ do
        result <-
          runMockSqlQueryT
            (repsert 1 $ person "Alice")
            [ withRecord @Person $ \case
                Repsert _ _ -> Just ()
                _ -> Nothing
            ]
        result @?= ()
    , testCase "repsertMany" $ do
        result <-
          runMockSqlQueryT
            (repsertMany [(1, person "Alice")])
            [ withRecord @Person $ \case
                RepsertMany _ -> Just ()
                _ -> Nothing
            ]
        result @?= ()
    , testCase "replace" $ do
        result <-
          runMockSqlQueryT
            (replace 1 $ person "Alice")
            [ withRecord @Person $ \case
                Replace _ _ -> Just ()
                _ -> Nothing
            ]
        result @?= ()
    , testCase "delete" $ do
        result <-
          runMockSqlQueryT
            (delete @Person 1)
            [ withRecord @Person $ \case
                Delete _ -> Just ()
                _ -> Nothing
            ]
        result @?= ()
    , testCase "update" $ do
        result <-
          runMockSqlQueryT
            (update 1 [PersonName =. "Alicia"])
            [ withRecord @Person $ \case
                Update _ _ -> Just ()
                _ -> Nothing
            ]
        result @?= ()
    , testCase "updateGet" $ do
        result <-
          runMockSqlQueryT
            (updateGet 1 [PersonName =. "Alicia"])
            [ withRecord @Person $ \case
                UpdateGet _ _ -> Just $ person "Alicia"
                _ -> Nothing
            ]
        personName result @?= "Alicia"
    , testCase "insertEntity" $ do
        let alice = person "Alice"
        result <-
          runMockSqlQueryT
            (insertEntity alice)
            [ withRecord @Person $ \case
                InsertEntity _ -> Just $ Entity 1 alice
                _ -> Nothing
            ]
        entityVal result @?= alice
    , testCase "insertRecord" $ do
        let alice = person "Alice"
        result <-
          runMockSqlQueryT
            (insertRecord alice)
            [ withRecord @Person $ \case
                InsertRecord _ -> Just alice
                _ -> Nothing
            ]
        result @?= alice
    , testCase "getBy" $ do
        result <-
          runMockSqlQueryT
            (mapM getBy [UniqueName "Alice", UniqueName "Bob"])
            [ withRecord @Person $ \case
                GetBy (UniqueName "Alice") -> Just $ Just $ Entity 1 $ person "Alice"
                GetBy (UniqueName "Bob") -> Just Nothing
                _ -> Nothing
            ]
        map (fmap getName) result @?= [Just "Alice", Nothing]
    , testCase "getByValue" $ do
        result <-
          runMockSqlQueryT
            (mapM getByValue [person "Alice", person "Bob"])
            [ withRecord @Person $ \case
                GetByValue Person{personName = "Alice"} -> Just $ Just $ Entity 1 $ person "Alice"
                GetByValue Person{personName = "Bob"} -> Just Nothing
                _ -> Nothing
            ]
        map (fmap getName) result @?= [Just "Alice", Nothing]
    , testCase "checkUnique" $ do
        result <-
          runMockSqlQueryT
            (mapM checkUnique [person "Alice", person "Bob"])
            [ withRecord @Person $ \case
                CheckUnique Person{personName = "Alice"} -> Just $ Just $ UniqueName "Alice"
                CheckUnique Person{personName = "Bob"} -> Just Nothing
                _ -> Nothing
            ]
        result @?= [Just $ UniqueName "Alice", Nothing]
    , testCase "checkUniqueUpdateable" $ do
        result <-
          runMockSqlQueryT
            (mapM checkUniqueUpdateable [Entity 1 $ person "Alice", Entity 2 $ person "Bob"])
            [ withRecord @Person $ \case
                CheckUniqueUpdateable (Entity _ Person{personName = "Alice"}) -> Just $ Just $ UniqueName "Alice"
                CheckUniqueUpdateable (Entity _ Person{personName = "Bob"}) -> Just Nothing
                _ -> Nothing
            ]
        result @?= [Just $ UniqueName "Alice", Nothing]
    , testCase "deleteBy" $ do
        result <-
          runMockSqlQueryT
            (deleteBy $ UniqueName "Alice")
            [ withRecord @Person $ \case
                DeleteBy _ -> Just ()
                _ -> Nothing
            ]
        result @?= ()
    , testCase "insertUnique" $ do
        result <-
          runMockSqlQueryT
            (mapM insertUnique [person "Alice", person "Bob"])
            [ withRecord @Person $ \case
                InsertUnique Person{personName = "Alice"} -> Just $ Just 1
                InsertUnique Person{personName = "Bob"} -> Just Nothing
                _ -> Nothing
            ]
        result @?= [Just 1, Nothing]
    , testCase "upsert" $ do
        let alice = person "Alice"
        result <-
          runMockSqlQueryT
            (upsert alice [PersonAge =. 100])
            [ withRecord @Person $ \case
                Upsert _ _ -> Just $ Entity 1 alice
                _ -> Nothing
            ]
        result @?= Entity 1 alice
    , testCase "upsertBy" $ do
        let alice = person "Alice"
        result <-
          runMockSqlQueryT
            (upsertBy (UniqueName "Alice") alice [PersonAge =. 100])
            [ withRecord @Person $ \case
                UpsertBy _ _ _ -> Just $ Entity 1 alice
                _ -> Nothing
            ]
        result @?= Entity 1 alice
    , testCase "putMany" $ do
        result <-
          runMockSqlQueryT
            (putMany [person "Alice"])
            [ withRecord @Person $ \case
                PutMany _ -> Just ()
                _ -> Nothing
            ]
        result @?= ()
    , testCase "insertBy" $ do
        let alice = person "Alice"
        result <-
          runMockSqlQueryT
            (mapM insertBy [alice, person "Bob"])
            [ withRecord @Person $ \case
                InsertBy Person{personName = "Alice"} -> Just $ Left $ Entity 1 alice
                InsertBy Person{personName = "Bob"} -> Just $ Right 2
                _ -> Nothing
            ]
        result @?= [Left $ Entity 1 alice, Right 2]
    , testCase "insertUniqueEntity" $ do
        let bob = person "Bob"
        result <-
          runMockSqlQueryT
            (mapM insertUniqueEntity [person "Alice", bob])
            [ withRecord @Person $ \case
                InsertUniqueEntity Person{personName = "Alice"} -> Just Nothing
                InsertUniqueEntity Person{personName = "Bob"} -> Just $ Just $ Entity 1 bob
                _ -> Nothing
            ]
        result @?= [Nothing, Just $ Entity 1 bob]
    , testCase "replaceUnique" $ do
        result <-
          runMockSqlQueryT
            (mapM (uncurry replaceUnique) [(1, person "Alice"), (2, person "Bob")])
            [ withRecord @Person $ \case
                ReplaceUnique _ Person{personName = "Alice"} -> Just Nothing
                ReplaceUnique _ Person{personName = "Bob"} -> Just $ Just $ UniqueName "Bob"
                _ -> Nothing
            ]
        result @?= [Nothing, Just $ UniqueName "Bob"]
    , testCase "onlyUnique" $ do
        result <-
          runMockSqlQueryT
            (onlyUnique $ person "Alice")
            [ withRecord @Person $ \case
                OnlyUnique _ -> Just $ UniqueName "Alice"
                _ -> Nothing
            ]
        result @?= UniqueName "Alice"
    , testCase "selectSourceRes" $ do
        acquire <-
          runMockSqlQueryT
            (selectSourceRes [] [])
            [ mockSelectSource $ \_ _ ->
                Just
                  [ Entity 1 $ person "Alice"
                  , Entity 2 $ person "Bob"
                  ]
            ]
        result <- Acquire.with acquire $ \conduit ->
          runConduit $ conduit .| Conduit.mapC getName .| Conduit.sinkList
        result @?= ["Alice", "Bob"]
    , testCase "selectFirst" $ do
        result1 <-
          runMockSqlQueryT
            (selectFirst [PersonName ==. "Alice"] [])
            [ withRecord @Person $ \case
                SelectFirst _ _ -> Just $ Just $ Entity 1 $ person "Alice"
                _ -> Nothing
            ]
        getName <$> result1 @?= Just "Alice"
        result2 <-
          runMockSqlQueryT
            (selectFirst [PersonName ==. "Alice"] [])
            [ withRecord @Person $ \case
                SelectFirst _ _ -> Just Nothing
                _ -> Nothing
            ]
        result2 @?= Nothing
    , testCase "selectKeysRes" $ do
        let keys = [1, 2, 3]
        acquire <-
          runMockSqlQueryT
            (selectKeysRes @_ @Person [] [])
            [ mockSelectKeys $ \_ _ -> Just keys
            ]
        result <- Acquire.with acquire $ \conduit ->
          runConduit $ conduit .| Conduit.sinkList
        result @?= keys
    , testCase "count" $ do
        result <-
          runMockSqlQueryT
            (count @Person [])
            [ withRecord @Person $ \case
                Count _ -> Just 10
                _ -> Nothing
            ]
        result @?= 10
    , testCase "exists" $ do
        result <-
          runMockSqlQueryT
            (exists @Person [])
            [ withRecord @Person $ \case
                Exists _ -> Just True
                _ -> Nothing
            ]
        result @?= True
    , testCase "selectSource" $ do
        result <-
          runResourceT $
            runMockSqlQueryT
              (runConduit $ selectSource [] [] .| Conduit.mapC getName .| Conduit.sinkList)
              [ mockSelectSource $ \_ _ ->
                  Just
                    [ Entity 1 $ person "Alice"
                    , Entity 2 $ person "Bob"
                    ]
              ]
        result @?= ["Alice", "Bob"]
    , testCase "selectKeys" $ do
        let keys = [1, 2, 3]
        result <-
          runResourceT $
            runMockSqlQueryT
              (runConduit $ selectKeys @Person [] [] .| Conduit.sinkList)
              [ mockSelectKeys $ \_ _ -> Just keys
              ]
        result @?= keys
    , testCase "selectList" $ do
        result <-
          runMockSqlQueryT
            (selectList [] [])
            [ withRecord @Person $ \case
                SelectList _ _ ->
                  Just
                    [ Entity 1 (person "Alice")
                    , Entity 2 (person "Bob")
                    ]
                _ -> Nothing
            ]
        map getName result @?= ["Alice", "Bob"]
    , testCase "selectKeysList" $ do
        let keys = [1, 2, 3]
        result <-
          runMockSqlQueryT
            (selectKeysList @Person [] [])
            [ withRecord @Person $ \case
                SelectKeysList _ _ -> Just keys
                _ -> Nothing
            ]
        result @?= keys
    , testCase "updateWhere" $ do
        result <-
          runMockSqlQueryT
            (updateWhere [] [PersonAge =. 100])
            [ withRecord @Person $ \case
                UpdateWhere _ _ -> Just ()
                _ -> Nothing
            ]
        result @?= ()
    , testCase "deleteWhere" $ do
        result <-
          runMockSqlQueryT
            (deleteWhere [PersonName ==. "Alice"])
            [ withRecord @Person $ \case
                DeleteWhere _ -> Just ()
                _ -> Nothing
            ]
        result @?= ()
    , testCase "updateWhereCount" $ do
        result <-
          runMockSqlQueryT
            (updateWhereCount [] [PersonAge =. 100])
            [ withRecord @Person $ \case
                UpdateWhereCount _ _ -> Just 10
                _ -> Nothing
            ]
        result @?= 10
    , testCase "deleteWhereCount" $ do
        result <-
          runMockSqlQueryT
            (deleteWhereCount [PersonName ==. "Alice"])
            [ withRecord @Person $ \case
                DeleteWhereCount _ -> Just 10
                _ -> Nothing
            ]
        result @?= 10
    , testCase "getFieldName" $ do
        result <-
          runMockSqlQueryT
            (getFieldName PersonName)
            [ withRecord @Person $ \case
                GetFieldName PersonName -> Just "\"name\""
                _ -> Nothing
            ]
        result @?= "\"name\""
    , testCase "getTableName" $ do
        result <-
          runMockSqlQueryT
            (getTableName $ person "Alice")
            [ withRecord @Person $ \case
                GetTableName _ -> Just "\"person\""
                _ -> Nothing
            ]
        result @?= "\"person\""
    , testCase "withRawQuery" $ do
        let query = "SELECT name FROM person"
            row1 = [toPersistValue @String "Alice"]
            row2 = [toPersistValue @String "Bob"]
            rows = [row1, row2]
        result <-
          runMockSqlQueryT
            (withRawQuery query [] Conduit.sinkList)
            [ mockWithRawQuery $ \sql _ ->
                if sql == query
                  then Just rows
                  else Nothing
            ]
        result @?= rows
    , testCase "rawQueryRes" $ do
        let row1 = [toPersistValue @String "Alice"]
            row2 = [toPersistValue @String "Bob"]
            rows = [row1, row2]
        acquire <-
          runMockSqlQueryT
            (rawQueryRes "SELECT name FROM person" [])
            [ mockRawQuery $ \_ _ -> Just rows
            ]
        result <- Acquire.with acquire $ \conduit ->
          runConduit $ conduit .| Conduit.sinkList
        result @?= rows
    , testCase "rawQuery" $ do
        let row1 = [toPersistValue @String "Alice"]
            row2 = [toPersistValue @String "Bob"]
            rows = [row1, row2]
        result <-
          runResourceT $
            runMockSqlQueryT
              (runConduit $ rawQuery "SELECT name FROM person" [] .| Conduit.sinkList)
              [ mockRawQuery $ \_ _ -> Just rows
              ]
        result @?= rows
    , testCase "rawExecute" $ do
        result <-
          runMockSqlQueryT
            (rawExecute "DELETE FROM person" [])
            [ mockQuery $ \case
                RawExecute _ _ -> Just ()
                _ -> Nothing
            ]
        result @?= ()
    , testCase "rawExecuteCount" $ do
        result <-
          runMockSqlQueryT
            (rawExecuteCount "DELETE FROM person" [])
            [ mockQuery $ \case
                RawExecuteCount _ _ -> Just 10
                _ -> Nothing
            ]
        result @?= 10
    , testCase "rawSql" $ do
        let names = ["Alice", "Bob"] :: [String]
        result <-
          runMockSqlQueryT
            (rawSql "SELECT name FROM person" [])
            [ mockRawSql $ \_ _ -> Just $ map ((: []) . toPersistValue) names
            ]
        map unSingle result @?= names
    ]
