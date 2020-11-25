# `persistent-mtl`

[![CircleCI](https://img.shields.io/circleci/build/github/brandonchinn178/persistent-mtl)](https://app.circleci.com/pipelines/github/brandonchinn178/persistent-mtl)
[![Hackage](https://img.shields.io/hackage/v/persistent-mtl)](https://hackage.haskell.org/package/persistent-mtl)
[![Codecov](https://img.shields.io/codecov/c/gh/brandonchinn178/persistent-mtl)](https://codecov.io/gh/brandonchinn178/persistent-mtl)

Use the `persistent` API in your monad transformer stack, seamlessly interleaving business logic with database operations by simply dropping `SqlQueryT` into your stack.

Features:
* Easy integration into a monad transformer stack
* Monad type class to generalize functions that use database operations
* Simple transaction control
* Supports mocking database operations in tests

## Quickstart

```hs
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Logger (runStderrLoggingT)
import Database.Persist.Sql (Entity(..), toSqlKey, (<.))
import Database.Persist.Monad
import Database.Persist.Sqlite (withSqliteConn)
import Database.Persist.TH
import UnliftIO (MonadUnliftIO(..), wrappedWithRunInIO)

import Database.Persist.Monad.TestUtils (runMockSqlQueryT, withRecord)
import Test.Tasty.HUnit (Assertion, (@?=))

share [mkPersist sqlSettings, mkMigrate "migrate"] [persistLowerCase|
Person
  name String
  age Int
  deriving Show Eq
|]

newtype MyApp a = MyApp
  { unMyApp :: SqlQueryT IO a
  }
  deriving (Functor, Applicative, Monad, MonadIO, MonadSqlQuery)

instance MonadUnliftIO MyApp where
  withRunInIO = wrappedWithRunInIO MyApp unMyApp

getYoungPeople :: (MonadIO m, MonadSqlQuery m) => m [Entity Person]
getYoungPeople = selectList [PersonAge <. 18] []

main :: IO ()
main = runStderrLoggingT $ withSqliteConn ":memory:" $ \conn ->
  liftIO $ runSqlQueryT (BackendSingle conn) $ unMyApp $ do
    runMigration migrate
    insert_ $ Person "Alice" 25
    insert_ $ Person "Bob" 10
    youngsters <- getYoungPeople
    liftIO $ print youngsters

-- unit test with mocks!
unit_my_function :: Assertion
unit_my_function = do
  let person1 = Entity (toSqlKey 1) (Person "Child1" 10)

  result <- runMockSqlQueryT getYoungPeople
    [ withRecord @Person $ \case
        SelectList _ _ -> Just [person1]
        _ -> Nothing
    ]

  result @?= [person1]
```

## What's wrong with just using `persistent`?

TODO
