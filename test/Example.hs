{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
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
{-# OPTIONS_GHC -Wno-missing-methods #-}

module Example
  ( TestApp
  , runTestApp

    -- * Helper functions
  , person
  , getPeopleNames
  , getName

    -- * Models
  , Person(..)
  , Post(..)
  , Unique(..)
  ) where

import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Logger (runNoLoggingT)
import qualified Data.Text as Text
import Database.Persist.Sql (Entity(..), Key, Unique, toSqlKey)
import Database.Persist.Sqlite (withSqlitePool)
import Database.Persist.TH
    ( mkDeleteCascade
    , mkMigrate
    , mkPersist
    , persistLowerCase
    , share
    , sqlSettings
    )
import UnliftIO (MonadUnliftIO(..), withSystemTempDirectory, wrappedWithRunInIO)

import Database.Persist.Monad

share
  [ mkPersist sqlSettings
  , mkDeleteCascade sqlSettings
  , mkMigrate "migrate"
  ]
  [persistLowerCase|
Person
  name String
  age Int
  UniqueName name
  deriving Show Eq

Post
  title String
  author PersonId
  deriving Show Eq
|]

deriving instance Eq (Unique Person)

-- Let tests use a literal number for keys
instance Num (Key Person) where
  fromInteger = toSqlKey . fromInteger

instance Num (Key Post) where
  fromInteger = toSqlKey . fromInteger

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
runTestApp m =
  withSystemTempDirectory "persistent-mtl-testapp" $ \dir -> do
    let db = Text.pack $ dir ++ "/db.sqlite"
    runNoLoggingT $ withSqlitePool db 5 $ \pool ->
      liftIO . runSqlQueryT pool . unTestApp $ do
        _ <- runMigrationSilent migrate
        m

{- Helper functions -}

person :: String -> Person
person name = Person name 0

getName :: Entity Person -> String
getName = personName . entityVal

getPeopleNames :: MonadSqlQuery m => m [String]
getPeopleNames = map getName <$> selectList [] []
