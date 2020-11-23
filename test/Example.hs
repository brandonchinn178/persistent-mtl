{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Example
  ( TestApp
  , runTestApp
  , runTestAppWithPool

    -- * Functions
  , getPeople

    -- * Models
  , Person(..)
  ) where

import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Logger (runNoLoggingT)
import Database.Persist (Entity)
import Database.Persist.Sqlite (withSqliteConn, withSqlitePool)
import Database.Persist.TH
    (mkMigrate, mkPersist, persistLowerCase, share, sqlSettings)
import UnliftIO (MonadUnliftIO(..), wrappedWithRunInIO)

import Database.Persist.Monad

share [mkPersist sqlSettings, mkMigrate "migrate"] [persistLowerCase|
Person
  name String
  age Int Maybe
  deriving Show Eq
|]

newtype TestApp a = TestApp
  { unTestApp :: SqlQueryT IO a
  } deriving
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadSqlQuery
    )

instance MonadUnliftIO TestApp where
  withRunInIO = wrappedWithRunInIO TestApp unTestApp

runTestApp :: TestApp a -> IO a
runTestApp m = runNoLoggingT $ withSqliteConn ":memory:" (runTestAppWith m . BackendSingle)

runTestAppWithPool :: TestApp a -> IO a
runTestAppWithPool m = runNoLoggingT $ withSqlitePool ":memory:" 5 (runTestAppWith m . BackendPool)

runTestAppWith :: MonadIO m => TestApp a -> SqlQueryBackend -> m a
runTestAppWith m backend = liftIO . runSqlQueryT backend . unTestApp $ do
  _ <- runMigrationSilent migrate
  m

getPeople :: MonadSqlQuery m => m [Entity Person]
getPeople = selectList [] []
