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
import Database.Persist.Sqlite (withSqlitePool)
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

getYoungPeople :: MonadSqlQuery m => m [Entity Person]
getYoungPeople = selectList [PersonAge <. 18] []

main :: IO ()
main = runStderrLoggingT $ withSqlitePool "db.sqlite" 5 $ \pool ->
  liftIO $ runSqlQueryT pool $ unMyApp $ do
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

### Using `persistent` in production code

`persistent` runs all of its functions in `SqlPersistT`, which is an alias for `ReaderT SqlBackend`. Since all functions run in this concrete monad and not a generalized type class, it becomes difficult to integrate database operations into your monad transformer stack. Below are some examples of trying to integrate `persistent` functions into a monad transformer application, and the drawbacks of each option.

#### Option 1: Add `SqlPersistT` to your monad transformer stack

One might look at the `SqlPersistT` type and think it's a monad transformer, and add it to their monad transformer stack. But since `persistent` functions run in the concrete `SqlPersistT` monad (and not with a type class), you'll need some way of lifting `SqlPersistT` into your application monad.

Before going further, I do want to point out that `SqlBackend` represents a single database connection, so adding `SqlPersistT` to your monad transformer stack would run your entire application in a single connection (read: single transaction)! So for most applications, this option probably won't work for you, but let's assume you have a use-case where this isn't an issue.

Option 1a is to write `liftSqlPersist` specifically for your application monad:

```hs
newtype MyApp a = MyApp (ReaderT MyAppConfig (SqlPersistT (LoggingT IO)) a)

-- Notice the duplication here: anything inside `SqlPersistT` in your stack
-- needs to go in here.
liftSqlPersist :: SqlPersistT (LoggingT IO) a -> MyApp a
liftSqlPersist = MyApp . lift
```

But then any function that runs database connections is taken out of mtl-style add needs to be concretely typed to `MyApp`

```hs
-- you originally had a nice mtl-style function with a generalized monad
foo :: MonadReader MyAppConfig m => m ()
foo = do
  config <- ask
  _ <- bar config
  return ()

-- but adding a database operation forces us to remove the generalization
foo :: MyApp ()
foo = do
  config <- ask
  _ <- bar config
  _ <- liftSqlPersist $ get $ configUserId config
  return ()
```

So then you might try option 1b and write a type class that will lift `SqlPersistT`:

```hs
class MonadLiftSqlPersist m where
  -- Remember how we had to duplicate anything inside `SqlPersistT` in your
  -- stack? The stack within `SqlPersistT` can be different between monads, so
  -- you need to define the inner type for each monad
  type Inner m :: Type -> Type

  liftSqlPersist :: SqlPersistT (Inner m) a -> m a

instance MonadLiftSqlPersist MyApp where
  type Inner MyApp = LoggingT IO
  liftSqlPersist = MyApp . lift
```

which still has the unfortunate problem of copy-pasting whatever is inside `SqlPersistT` into the `Inner` type family instance.

But the main problem with both of these options is that `liftSqlPersist` will only contain the context you put inside `SqlPersistT`, meaning that within a `liftSqlPersist` action, you can't get access to `MyAppConfig`! Of course, you could always make `SqlPersistT` the very first monad transformer in your stack, but that might not work in another situation. Plus, you'd have even more monad transformers to copy into the type of `liftSqlPersist`.

#### Option 2: Manually run `runSqlPool` every time you run a `persistent` function

Here, you might store the `Pool SqlBackend` in your monad transformer stack and then use `runSqlPool` to immediately unwrap `SqlPersistT`.

```hs
data MyAppConfig = MyAppConfig
  { backendPool :: Pool SqlBackend
  , ...
  }

runQuery :: MonadReader MyAppConfig m => SqlPersistT m a -> m a
runQuery m = do
  MyAppConfig{backendPool} <- ask
  runSqlPool m backendPool

foo :: MonadReader MyAppConfig m => m ()
foo = do
  config <- ask
  _ <- bar config
  _ <- runQuery $ get $ configUserId config
  return ()
```

Great! Let me first say that this is *not a bad solution*. You could even make your own type class like `MonadHasBackendPool` to abstract away monads that contain a `Pool SqlBackend`, not necessarily the whole `MyAppConfig`.

There are two drawbacks with this approach, one minor drawback and one major drawback. The minor drawback is that you have to put the `Pool SqlBackend` into your environment yourself. It would be great if there could be a monad transformer and type class already made for you to easily plug it in. It's not that much code, so this isn't a big deal, but if you're quickly bootstrapping a new project with persistent, it'd be nice to reach for something already built.

The major drawback with this approach is transactions and composability. `runSqlPool` (and `runQuery` in this example) runs its action within a single transaction. Say you have two functions that run separate, composable actions that interleave business logic and database operations:

```hs
foo :: MonadReader MyAppConfig m => m ()
foo = do
  -- business logic
  runQuery $ insert_ $ ...
  -- more business logic

bar :: MonadReader MyAppConfig m => m ()
bar = do
  -- business logic
  runQuery $ insert_ $ ...
  -- more business logic
```

There is no way to compose `foo` and `bar` so that it all runs within a single database transaction. You could try

```hs
fooAndBar :: MonadReader MyAppConfig m => m ()
fooAndBar = runQuery $ do
  lift foo
  -- something else
  lift bar
```

but `foo` and `bar` each run their own `runQuery` function, so actually, `fooAndBar` uses three connections (i.e. three transactions): one connection from `runQuery` in `fooAndBar` and one connection each from `foo` and `bar`.

#### Option 3: `persistent-mtl`

So what does `persistent-mtl` do differently?

1. It stores the entire `Pool SqlBackend` in `SqlQueryT`, which means you *can* add `SqlQueryT` to your monad transformer stack. Remember that the problem with adding `SqlPersistT` to your monad transformer stack is that your entire application would run with a single database connection, aka a single database transaction.
1. It provides a `MonadSqlQuery` type class out of the box and all of `persistent`'s functions lifted to use `MonadSqlQuery`
1. It provides a `withTransaction` function that runs the given action within a single transaction. For example,

    ```hs
    foo :: MonadSqlQuery m => m ()
    foo = do
      -- business logic
      insert_ $ ...
      -- more business logic

    bar :: MonadSqlQuery m => m ()
    bar = do
      -- business logic
      insert_ $ ...
      -- more business logic

    fooAndBar :: MonadSqlQuery m => m ()
    fooAndBar = withTransaction $ do
      foo
      -- something else
      bar
    ```

    `fooAndBar` will run both `foo` and `bar` in the same transaction. Note that `foo` and `bar` themselves don't say anything about transactions. By default, using a `persistent` function without `withTransaction` will run each query in its own transaction. And if `foo` did use `withTransaction`, it would start a transaction within a transaction (if the SQL backend supports it). Now, `foo` and `bar` are composable!

In summary, `persistent-mtl` takes all the good things about option 2, implements them out of the box (so you don't have to do it yourself), and makes your business logic functions composable with transactions behaving the way YOU want.

### Easy transaction management

Some databases will throw an error if two transactions conflict (e.g. [PostgreSQL](https://www.postgresql.org/docs/9.5/transaction-iso.html)). The client is expected to retry transactions if this error is thrown. `persistent` doesn't easily support this out of the box, but `persistent-mtl` does!

```hs
import Database.PostgreSQL.Simple.Errors (isSerializationError)

main :: IO ()
main = withPostgresqlPool "..." 5 $ \pool -> do
  let env = mkSqlQueryEnv pool $ \env -> env
        { retryIf = maybe False isSerializationError . fromException
        , retryLimit = 100 -- defaults to 10
        }

  -- in any of the marked transactions below, if someone else is querying
  -- the postgresql database at the same time with queries that conflict
  -- with yours, your operations will automatically be retried
  runSqlQueryTWith env $ do
    -- transaction 1
    insert_ $ ...

    -- transaction 2
    withTransaction $ do
      insert_ $ ...

      -- transaction 2.5: transaction-within-a-transaction is supported in PostgreSQL
      withTransaction $ do
        insert_ $ ...

      insert_ $ ...

    -- transaction 3
    insert_ $ ...
```

Because of this built-in retry support, any IO actions inside `withTransaction` have to be explicitly marked with `rerunnableIO`. If you try to use a function with a `MonadIO m` constraint, you'll get a compile-time error!

```
.../Foo.hs:100:5: error:
    • Cannot run arbitrary IO actions within a transaction. If the IO action is rerunnable, use rerunnableIO
    • In a stmt of a 'do' block: arbitraryIO
      In the second argument of ‘($)’, namely
        ‘withTransaction
           $ do insert_ record1
                arbitraryIO
                insert_ record2’
    |
100 |     arbitraryIO
    |     ^^^^^^^^^^^
```

Note that this **only** applies for transactions, so `MonadIO` and `MonadSqlQuery` constraints can still co-exist (for a function with IO actions that are not rerunnable) as long as the function is never called within `withTransaction`.

### Testing functions that use `persistent` operations

Generally, I would recommend someone using `persistent` in their application to make a monad type class containing the API for their domain, like

```hs
class MonadAppService m where
  getYoungPeople :: m [Entity Person]

instance MonadAppService MyApp where
  getYoungPeople = selectList [PersonAge <. 18] []
```

so that writing unit tests would mock out domain-level abstractions. I generally wouldn't recommend mocking out the entire database state; if you're testing complex database queries, you should just write integration tests and check that the queries do what you expect on an actual database.

But maybe you have a small function that uses `selectList` and it's not worth making a whole type class to wrap that call. With `persistent`, `selectList` runs a `SqlPersistT` action, which is completely un-introspectable. Sure, you could pass in a `SqlBackend` that intercepts all queries, but you'd be mocking extremely low level behavior — your mock would need to know the exact `SELECT` query `selectList` sends.

`persistent-mtl`, on the other hand, provides `MockSqlQueryT` which you can use to execute your `MonadSqlQuery` functions with a list of mocks, where a mock intercepts `SqlQueryRep`, a data representation of each `persistent` function, and returns the result. For example, to mock `selectList`, you'd simply do

```hs
runMockSqlQueryT getYoungPeople
  [ withRecord @Person $ \case
      SelectList _ _ -> Just mockedPersonList
      _ -> Nothing
  ]
```

and `MockSqlQueryT` would intercept a `selectList` call for a `Person` record and return your mocked result. Each `persistent` function has a corresponding data type constructor (with a few exceptions, such as `selectSource`, which works differently).

If your function does some complex raw SQL queries, you can intercept those like this:

```hs
crazyFunction :: MonadSqlQuery m => String -> m [Int]
crazyFunction postTitle = rawSql
  "SELECT age FROM person INNER JOIN post ON person.id = post.author WHERE post.title = ?"
  [toPersistValue postTitle]

let mockRawSql = mockQuery $ \case
      RawSql _ [toPersistValue "foo"] -> Just [1]
      RawSql _ [toPersistValue "bar"] -> Just [2]
      _ -> Nothing

-- returns [1]
runMockSqlQueryT (crazyFunction "foo") [mockRawSql]

-- returns [2]
runMockSqlQueryT (crazyFunction "bar") [mockRawSql]

-- error: Could not find mock for query
runMockSqlQueryT (crazyFunction "baz") [mockRawSql]
```
