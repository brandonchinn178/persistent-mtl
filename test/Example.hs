{-# LANGUAGE CPP #-}
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
  , runTestAppWith

    -- * Person
  , Person(..)
  , person
  , getPeople
  , getPeopleNames
  , getName
  , nameAndAge

    -- * Post
  , Post(..)
  , post
  , getPosts
  , getPostTitles

    -- * Persistent
  , EntityField(..)
  , Unique(..)
  , migration
  ) where

import Control.Arrow ((&&&))
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Resource (MonadResource, ResourceT, runResourceT)
import Database.Persist.Sql
    (Entity(..), EntityField, Key, SelectOpt(..), Unique, toSqlKey)
import Database.Persist.TH
    (mkMigrate, mkPersist, persistLowerCase, share, sqlSettings)
import UnliftIO (MonadUnliftIO(..), wrappedWithRunInIO)

import Control.Monad.IO.Rerunnable (MonadRerunnableIO)
import Database.Persist.Monad
import TestUtils.DB (BackendType(..), withTestDB)

share
  [ mkPersist sqlSettings
  , mkMigrate "migration"
  ]
  [persistLowerCase|
Person
  name          String
  age           Int
  removedColumn String SafeToRemove
  UniqueName name
  deriving Show Eq

Post
  title  String
  author PersonId
  editor PersonId Maybe
  deriving Show Eq
|]

deriving instance Eq (Unique Person)
#if !MIN_VERSION_persistent_template(2,6,0) || MIN_VERSION_persistent_template(2,9,0)
deriving instance Show (Unique Person)
#endif

-- Let tests use a literal number for keys
instance Num (Key Person) where
  fromInteger = toSqlKey . fromInteger

instance Num (Key Post) where
  fromInteger = toSqlKey . fromInteger

newtype TestApp a = TestApp
  { unTestApp :: SqlQueryT (ResourceT IO) a
  } deriving
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadRerunnableIO
    , MonadSqlQuery
    , MonadResource
    )

instance MonadUnliftIO TestApp where
  withRunInIO = wrappedWithRunInIO TestApp unTestApp

runTestApp :: BackendType -> TestApp a -> IO a
runTestApp backendType m =
  withTestDB backendType $ \pool ->
    runResourceT . runSqlQueryT pool . unTestApp $ do
      _ <- runMigrationSilent migration
      m

runTestAppWith :: BackendType -> (SqlQueryEnv -> SqlQueryEnv) -> TestApp a -> IO a
runTestAppWith backendType f m =
  withTestDB backendType $ \pool -> do
    let env = mkSqlQueryEnv pool f
    runResourceT . runSqlQueryTWith env . unTestApp $ do
      _ <- runMigrationSilent migration
      m

{- Person functions -}

person :: String -> Person
person name = Person name 0

getName :: Entity Person -> String
getName = personName . entityVal

getPeople :: MonadSqlQuery m => m [Person]
getPeople = map entityVal <$> selectList [] [Asc PersonId]

getPeopleNames :: MonadSqlQuery m => m [String]
getPeopleNames = map personName <$> getPeople

nameAndAge :: Person -> (String, Int)
nameAndAge = personName &&& personAge

{- Post functions -}

post :: String -> Key Person -> Post
post title author = Post title author Nothing

getPosts :: MonadSqlQuery m => m [Post]
getPosts = map entityVal <$> selectList [] []

getPostTitles :: MonadSqlQuery m => m [String]
getPostTitles = map postTitle <$> getPosts
