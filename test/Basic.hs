{- AUTOCOLLECT.TEST -}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

module Basic (
  ) where

{- AUTOCOLLECT.TEST.export -}

import Test.Tasty.HUnit

import Database.Persist.Monad
import Database.Persist.Monad.TestUtils
import Example

test =
  testCase "Ensure withTransaction README example typechecks" $ do
    let foo :: MonadSqlQuery m => m ()
        foo = insert_ $ person "Alice"
        bar :: MonadSqlQuery m => m ()
        bar = insert_ $ person "Bob"
        fooAndBar :: MonadSqlQuery m => m ()
        fooAndBar = withTransaction $ foo >> bar
    runMockSqlQueryT
      fooAndBar
      [ withRecord @Person $ \case
          Insert_ _ -> Just ()
          _ -> Nothing
      , withRecord @Person $ \case
          Insert_ _ -> Just ()
          _ -> Nothing
      ]
