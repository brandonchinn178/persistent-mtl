{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module MockedSpec (spec) where

import Conduit (runConduit, runResourceT, (.|))
import qualified Conduit
import qualified Data.Acquire as Acquire
import qualified Data.Map.Strict as Map
import Data.Maybe (listToMaybe)
import Database.Persist.Sql (
  Entity (..),
  Single (..),
  toPersistValue,
  toSqlKey,
  (=.),
  (==.),
 )
import Skeletest
import qualified Skeletest.Predicate as P
import UnliftIO (SomeException)

import Database.Persist.Monad
import Database.Persist.Monad.TestUtils
import Example

spec :: Spec
spec = do
  describe "withTransaction" $ do
    it "doesn't error with MockSqlQueryT" $
      runMockSqlQueryT
        (withTransaction $ insert_ $ person "Alice")
        [ withRecord @Person $ \case
            Insert_ _ -> Just ()
            _ -> Nothing
        ]

  describe "MockSqlQueryT" $ do
    it "errors if it could not find a mock" $ do
      let errMsg e = (listToMaybe . lines . show) (e :: SomeException)
      runMockSqlQueryT getPeopleNames [] `shouldSatisfy` P.throws (errMsg P.>>> P.just (P.eq "Could not find mock for query: SelectList{..}<Person>"))

    it "continues after a mock doesn't match" $ do
      result <-
        runMockSqlQueryT
          getPeopleNames
          [ withRecord @Post $ \_ -> error "getPeopleNames matched Post record"
          , mockQuery $ \_ -> Nothing
          , withRecord @Person $ \case
              SelectList _ _ ->
                Just
                  [ Entity (toSqlKey 1) (Person "Alice" 10)
                  , Entity (toSqlKey 2) (Person "Bob" 20)
                  ]
              _ -> Nothing
          ]

      result `shouldBe` ["Alice", "Bob"]

  describe "Persistent API" $ do
    it "get" $ do
      result <-
        runMockSqlQueryT
          (mapM get [1, 2])
          [ withRecord @Person $ \case
              Get n
                | n == 1 -> Just $ Just $ person "Alice"
                | n == 2 -> Just Nothing
              _ -> Nothing
          ]
      map (fmap personName) result `shouldBe` [Just "Alice", Nothing]

    it "getMany" $ do
      result <-
        runMockSqlQueryT
          (getMany [1])
          [ withRecord @Person $ \case
              GetMany _ -> Just $ Map.fromList [(1, person "Alice")]
              _ -> Nothing
          ]
      personName <$> Map.lookup 1 result `shouldBe` Just "Alice"

    it "getJust" $ do
      result <-
        runMockSqlQueryT
          (getJust 1)
          [ withRecord @Person $ \case
              GetJust _ -> Just $ person "Alice"
              _ -> Nothing
          ]
      personName result `shouldBe` "Alice"

    it "getJustEntity" $ do
      result <-
        runMockSqlQueryT
          (getJustEntity 1)
          [ withRecord @Person $ \case
              GetJustEntity _ -> Just $ Entity 1 $ person "Alice"
              _ -> Nothing
          ]
      getName result `shouldBe` "Alice"

    it "getEntity" $ do
      result <-
        runMockSqlQueryT
          (mapM getEntity [1, 2])
          [ withRecord @Person $ \case
              GetEntity n
                | n == 1 -> Just $ Just $ Entity 1 $ person "Alice"
                | n == 2 -> Just Nothing
              _ -> Nothing
          ]
      map (fmap getName) result `shouldBe` [Just "Alice", Nothing]

    it "belongsTo" $ do
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
      map (fmap personName) result `shouldBe` [Just "Alice", Nothing]

    it "belongsToJust" $ do
      let post1 = Post "Post #1" 1 Nothing
      result <-
        runMockSqlQueryT
          (belongsToJust postAuthor post1)
          [ withRecord @(Post, Person) $ \case
              BelongsToJust _ _ -> Just $ person "Alice"
              _ -> Nothing
          ]
      personName result `shouldBe` "Alice"

    it "insert" $ do
      result <-
        runMockSqlQueryT
          (insert $ person "Alice")
          [ withRecord @Person $ \case
              Insert _ -> Just 1
              _ -> Nothing
          ]
      result `shouldBe` 1

    it "insert_" $ do
      result <-
        runMockSqlQueryT
          (insert_ $ person "Alice")
          [ withRecord @Person $ \case
              Insert_ _ -> Just ()
              _ -> Nothing
          ]
      result `shouldBe` ()

    it "insertMany" $ do
      result <-
        runMockSqlQueryT
          (insertMany [person "Alice", person "Bob"])
          [ withRecord @Person $ \case
              InsertMany records -> Just $ map fromIntegral [1 .. length records]
              _ -> Nothing
          ]
      result `shouldBe` [1, 2]

    it "insertMany_" $ do
      result <-
        runMockSqlQueryT
          (insertMany_ [person "Alice", person "Bob"])
          [ withRecord @Person $ \case
              InsertMany_ _ -> Just ()
              _ -> Nothing
          ]
      result `shouldBe` ()

    it "insertEntityMany" $ do
      result <-
        runMockSqlQueryT
          (insertEntityMany [Entity 1 $ person "Alice"])
          [ withRecord @Person $ \case
              InsertEntityMany _ -> Just ()
              _ -> Nothing
          ]
      result `shouldBe` ()

    it "insertKey" $ do
      result <-
        runMockSqlQueryT
          (insertKey 1 $ person "Alice")
          [ withRecord @Person $ \case
              InsertKey _ _ -> Just ()
              _ -> Nothing
          ]
      result `shouldBe` ()

    it "repsert" $ do
      result <-
        runMockSqlQueryT
          (repsert 1 $ person "Alice")
          [ withRecord @Person $ \case
              Repsert _ _ -> Just ()
              _ -> Nothing
          ]
      result `shouldBe` ()

    it "repsertMany" $ do
      result <-
        runMockSqlQueryT
          (repsertMany [(1, person "Alice")])
          [ withRecord @Person $ \case
              RepsertMany _ -> Just ()
              _ -> Nothing
          ]
      result `shouldBe` ()

    it "replace" $ do
      result <-
        runMockSqlQueryT
          (replace 1 $ person "Alice")
          [ withRecord @Person $ \case
              Replace _ _ -> Just ()
              _ -> Nothing
          ]
      result `shouldBe` ()

    it "delete" $ do
      result <-
        runMockSqlQueryT
          (delete @Person 1)
          [ withRecord @Person $ \case
              Delete _ -> Just ()
              _ -> Nothing
          ]
      result `shouldBe` ()

    it "update" $ do
      result <-
        runMockSqlQueryT
          (update 1 [PersonName =. "Alicia"])
          [ withRecord @Person $ \case
              Update _ _ -> Just ()
              _ -> Nothing
          ]
      result `shouldBe` ()

    it "updateGet" $ do
      result <-
        runMockSqlQueryT
          (updateGet 1 [PersonName =. "Alicia"])
          [ withRecord @Person $ \case
              UpdateGet _ _ -> Just $ person "Alicia"
              _ -> Nothing
          ]
      personName result `shouldBe` "Alicia"

    it "insertEntity" $ do
      let alice = person "Alice"
      result <-
        runMockSqlQueryT
          (insertEntity alice)
          [ withRecord @Person $ \case
              InsertEntity _ -> Just $ Entity 1 alice
              _ -> Nothing
          ]
      entityVal result `shouldBe` alice

    it "insertRecord" $ do
      let alice = person "Alice"
      result <-
        runMockSqlQueryT
          (insertRecord alice)
          [ withRecord @Person $ \case
              InsertRecord _ -> Just alice
              _ -> Nothing
          ]
      result `shouldBe` alice

    it "getBy" $ do
      result <-
        runMockSqlQueryT
          (mapM getBy [UniqueName "Alice", UniqueName "Bob"])
          [ withRecord @Person $ \case
              GetBy (UniqueName "Alice") -> Just $ Just $ Entity 1 $ person "Alice"
              GetBy (UniqueName "Bob") -> Just Nothing
              _ -> Nothing
          ]
      map (fmap getName) result `shouldBe` [Just "Alice", Nothing]

    it "getByValue" $ do
      result <-
        runMockSqlQueryT
          (mapM getByValue [person "Alice", person "Bob"])
          [ withRecord @Person $ \case
              GetByValue Person{personName = "Alice"} -> Just $ Just $ Entity 1 $ person "Alice"
              GetByValue Person{personName = "Bob"} -> Just Nothing
              _ -> Nothing
          ]
      map (fmap getName) result `shouldBe` [Just "Alice", Nothing]

    it "checkUnique" $ do
      result <-
        runMockSqlQueryT
          (mapM checkUnique [person "Alice", person "Bob"])
          [ withRecord @Person $ \case
              CheckUnique Person{personName = "Alice"} -> Just $ Just $ UniqueName "Alice"
              CheckUnique Person{personName = "Bob"} -> Just Nothing
              _ -> Nothing
          ]
      result `shouldBe` [Just $ UniqueName "Alice", Nothing]

    it "checkUniqueUpdateable" $ do
      result <-
        runMockSqlQueryT
          (mapM checkUniqueUpdateable [Entity 1 $ person "Alice", Entity 2 $ person "Bob"])
          [ withRecord @Person $ \case
              CheckUniqueUpdateable (Entity _ Person{personName = "Alice"}) -> Just $ Just $ UniqueName "Alice"
              CheckUniqueUpdateable (Entity _ Person{personName = "Bob"}) -> Just Nothing
              _ -> Nothing
          ]
      result `shouldBe` [Just $ UniqueName "Alice", Nothing]

    it "deleteBy" $ do
      result <-
        runMockSqlQueryT
          (deleteBy $ UniqueName "Alice")
          [ withRecord @Person $ \case
              DeleteBy _ -> Just ()
              _ -> Nothing
          ]
      result `shouldBe` ()

    it "insertUnique" $ do
      result <-
        runMockSqlQueryT
          (mapM insertUnique [person "Alice", person "Bob"])
          [ withRecord @Person $ \case
              InsertUnique Person{personName = "Alice"} -> Just $ Just 1
              InsertUnique Person{personName = "Bob"} -> Just Nothing
              _ -> Nothing
          ]
      result `shouldBe` [Just 1, Nothing]

    it "upsert" $ do
      let alice = person "Alice"
      result <-
        runMockSqlQueryT
          (upsert alice [PersonAge =. 100])
          [ withRecord @Person $ \case
              Upsert _ _ -> Just $ Entity 1 alice
              _ -> Nothing
          ]
      result `shouldBe` Entity 1 alice

    it "upsertBy" $ do
      let alice = person "Alice"
      result <-
        runMockSqlQueryT
          (upsertBy (UniqueName "Alice") alice [PersonAge =. 100])
          [ withRecord @Person $ \case
              UpsertBy _ _ _ -> Just $ Entity 1 alice
              _ -> Nothing
          ]
      result `shouldBe` Entity 1 alice

    it "putMany" $ do
      result <-
        runMockSqlQueryT
          (putMany [person "Alice"])
          [ withRecord @Person $ \case
              PutMany _ -> Just ()
              _ -> Nothing
          ]
      result `shouldBe` ()

    it "insertBy" $ do
      let alice = person "Alice"
      result <-
        runMockSqlQueryT
          (mapM insertBy [alice, person "Bob"])
          [ withRecord @Person $ \case
              InsertBy Person{personName = "Alice"} -> Just $ Left $ Entity 1 alice
              InsertBy Person{personName = "Bob"} -> Just $ Right 2
              _ -> Nothing
          ]
      result `shouldBe` [Left $ Entity 1 alice, Right 2]

    it "insertUniqueEntity" $ do
      let bob = person "Bob"
      result <-
        runMockSqlQueryT
          (mapM insertUniqueEntity [person "Alice", bob])
          [ withRecord @Person $ \case
              InsertUniqueEntity Person{personName = "Alice"} -> Just Nothing
              InsertUniqueEntity Person{personName = "Bob"} -> Just $ Just $ Entity 1 bob
              _ -> Nothing
          ]
      result `shouldBe` [Nothing, Just $ Entity 1 bob]

    it "replaceUnique" $ do
      result <-
        runMockSqlQueryT
          (mapM (uncurry replaceUnique) [(1, person "Alice"), (2, person "Bob")])
          [ withRecord @Person $ \case
              ReplaceUnique _ Person{personName = "Alice"} -> Just Nothing
              ReplaceUnique _ Person{personName = "Bob"} -> Just $ Just $ UniqueName "Bob"
              _ -> Nothing
          ]
      result `shouldBe` [Nothing, Just $ UniqueName "Bob"]

    it "onlyUnique" $ do
      result <-
        runMockSqlQueryT
          (onlyUnique $ person "Alice")
          [ withRecord @Person $ \case
              OnlyUnique _ -> Just $ UniqueName "Alice"
              _ -> Nothing
          ]
      result `shouldBe` UniqueName "Alice"

    it "selectSourceRes" $ do
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
      result `shouldBe` ["Alice", "Bob"]

    it "selectFirst" $ do
      result1 <-
        runMockSqlQueryT
          (selectFirst [PersonName ==. "Alice"] [])
          [ withRecord @Person $ \case
              SelectFirst _ _ -> Just $ Just $ Entity 1 $ person "Alice"
              _ -> Nothing
          ]
      getName <$> result1 `shouldBe` Just "Alice"
      result2 <-
        runMockSqlQueryT
          (selectFirst [PersonName ==. "Alice"] [])
          [ withRecord @Person $ \case
              SelectFirst _ _ -> Just Nothing
              _ -> Nothing
          ]
      result2 `shouldBe` Nothing

    it "selectKeysRes" $ do
      let keys = [1, 2, 3]
      acquire <-
        runMockSqlQueryT
          (selectKeysRes @_ @Person [] [])
          [ mockSelectKeys $ \_ _ -> Just keys
          ]
      result <- Acquire.with acquire $ \conduit ->
        runConduit $ conduit .| Conduit.sinkList
      result `shouldBe` keys

    it "count" $ do
      result <-
        runMockSqlQueryT
          (count @Person [])
          [ withRecord @Person $ \case
              Count _ -> Just 10
              _ -> Nothing
          ]
      result `shouldBe` 10

    it "exists" $ do
      result <-
        runMockSqlQueryT
          (exists @Person [])
          [ withRecord @Person $ \case
              Exists _ -> Just True
              _ -> Nothing
          ]
      result `shouldBe` True

    it "selectSource" $ do
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
      result `shouldBe` ["Alice", "Bob"]

    it "selectKeys" $ do
      let keys = [1, 2, 3]
      result <-
        runResourceT $
          runMockSqlQueryT
            (runConduit $ selectKeys @Person [] [] .| Conduit.sinkList)
            [ mockSelectKeys $ \_ _ -> Just keys
            ]
      result `shouldBe` keys

    it "selectList" $ do
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
      map getName result `shouldBe` ["Alice", "Bob"]

    it "selectKeysList" $ do
      let keys = [1, 2, 3]
      result <-
        runMockSqlQueryT
          (selectKeysList @Person [] [])
          [ withRecord @Person $ \case
              SelectKeysList _ _ -> Just keys
              _ -> Nothing
          ]
      result `shouldBe` keys

    it "updateWhere" $ do
      result <-
        runMockSqlQueryT
          (updateWhere [] [PersonAge =. 100])
          [ withRecord @Person $ \case
              UpdateWhere _ _ -> Just ()
              _ -> Nothing
          ]
      result `shouldBe` ()

    it "deleteWhere" $ do
      result <-
        runMockSqlQueryT
          (deleteWhere [PersonName ==. "Alice"])
          [ withRecord @Person $ \case
              DeleteWhere _ -> Just ()
              _ -> Nothing
          ]
      result `shouldBe` ()

    it "updateWhereCount" $ do
      result <-
        runMockSqlQueryT
          (updateWhereCount [] [PersonAge =. 100])
          [ withRecord @Person $ \case
              UpdateWhereCount _ _ -> Just 10
              _ -> Nothing
          ]
      result `shouldBe` 10

    it "deleteWhereCount" $ do
      result <-
        runMockSqlQueryT
          (deleteWhereCount [PersonName ==. "Alice"])
          [ withRecord @Person $ \case
              DeleteWhereCount _ -> Just 10
              _ -> Nothing
          ]
      result `shouldBe` 10

    it "getFieldName" $ do
      result <-
        runMockSqlQueryT
          (getFieldName PersonName)
          [ withRecord @Person $ \case
              GetFieldName PersonName -> Just "\"name\""
              _ -> Nothing
          ]
      result `shouldBe` "\"name\""

    it "getTableName" $ do
      result <-
        runMockSqlQueryT
          (getTableName $ person "Alice")
          [ withRecord @Person $ \case
              GetTableName _ -> Just "\"person\""
              _ -> Nothing
          ]
      result `shouldBe` "\"person\""

    it "withRawQuery" $ do
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
      result `shouldBe` rows

    it "rawQueryRes" $ do
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
      result `shouldBe` rows

    it "rawQuery" $ do
      let row1 = [toPersistValue @String "Alice"]
          row2 = [toPersistValue @String "Bob"]
          rows = [row1, row2]
      result <-
        runResourceT $
          runMockSqlQueryT
            (runConduit $ rawQuery "SELECT name FROM person" [] .| Conduit.sinkList)
            [ mockRawQuery $ \_ _ -> Just rows
            ]
      result `shouldBe` rows

    it "rawExecute" $ do
      result <-
        runMockSqlQueryT
          (rawExecute "DELETE FROM person" [])
          [ mockQuery $ \case
              RawExecute _ _ -> Just ()
              _ -> Nothing
          ]
      result `shouldBe` ()

    it "rawExecuteCount" $ do
      result <-
        runMockSqlQueryT
          (rawExecuteCount "DELETE FROM person" [])
          [ mockQuery $ \case
              RawExecuteCount _ _ -> Just 10
              _ -> Nothing
          ]
      result `shouldBe` 10

    it "rawSql" $ do
      let names = ["Alice", "Bob"] :: [String]
      result <-
        runMockSqlQueryT
          (rawSql "SELECT name FROM person" [])
          [ mockRawSql $ \_ _ -> Just $ map ((: []) . toPersistValue) names
          ]
      map unSingle result `shouldBe` names
