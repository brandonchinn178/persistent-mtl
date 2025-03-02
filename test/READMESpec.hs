{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

module READMESpec (spec) where

import Skeletest

import Database.Persist.Monad
import Database.Persist.Monad.TestUtils
import Example

spec :: Spec
spec = do
  it "withTransaction example works" $ do
    let foo :: (MonadSqlQuery m) => m ()
        foo = insert_ $ person "Alice"
        bar :: (MonadSqlQuery m) => m ()
        bar = insert_ $ person "Bob"
        fooAndBar :: (MonadSqlQuery m) => m ()
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
