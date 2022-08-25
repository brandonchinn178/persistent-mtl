{- AUTOCOLLECT.TEST -}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

module MockSqlQueryT (
  -- $AUTOCOLLECT.TEST.export$
) where

import Database.Persist (Entity (..))
import Database.Persist.Sql (toSqlKey)
import Test.Tasty.HUnit
import UnliftIO (SomeException, try)

import Database.Persist.Monad.TestUtils
import Example

test =
  testCase "it errors if it could not find a mock" $ do
    result <- try $ runMockSqlQueryT getPeopleNames []
    case result of
      Right _ -> assertFailure "runMockSqlQueryT did not fail"
      Left e -> do
        let msg = head $ lines $ show (e :: SomeException)
        msg @?= "Could not find mock for query: SelectList{..}<Person>"

test =
  testCase "it continues after a mock doesn't match" $ do
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

    result @?= ["Alice", "Bob"]
