# Generating lifted persistent functions

## Usage

```
scripts/generate/run.sh
```

## Configuration

Configuration lives in `persistent-api.yaml`. The file contains a list of objects, where each object corresponds to a `persistent` function that we want to lift into the `MonadSqlQuery` context. Follow the instructions in this guide to update the YAML file, which will generate the appropriate Haskell code. If you're trying to add a function that doesn't fit neatly with the instructions or categories, bring up a discussion with other developers.

### Pre-configuration steps

First, write down the type of the function and modify it according to these instructions:

1. Remove the `forall` section

1. Update type variables
    1. Specialize `backend` as `SqlBackend`
    1. Ensure the `PersistEntity` type variable is named `record`. If there are multiple `PersistEntity` types, they should be named `record1`, `record2`, etc. Choose an order that makes sense for `withRecord`, e.g.

        ```hs
        withRecord @(record1, record2, ...) $ ...
        ```

1. Simplify constraints
    1. Remove redundant constraints, e.g. `PersistQueryRead SqlBackend`
    1. Substitute `PersistRecordBackend`, if applicable
    1. Remove `BackendCompatible SqlBackend env` and replace any usage of `env` with `SqlBackend`
    1. Remove `MonadReader SqlBackend m` and replace any usage of `m` with `ReaderT SqlBackend m`
        * e.g.
            ```hs
            foo :: MonadReader SqlBackend m => m Foo

            -- change to this:
            foo :: ReaderT SqlBackend m Foo
            ```

1. Simplify monads
    1. If a function returns a `ReaderT r m1 (ConduitM ... m2 ...)` action, change it to `m (ConduitM ... m2 ...)`
        * Note the change from `m1` to `m`
    1. If a function `pipeSomething` returns a `ConduitM i o (ReaderT r m) a` action, there should be a corresponding `pipeSomethingRes` function with the same type, except returning `Acquire (ConduitM i o m2 a)`. Replace `ReaderT r m` with just `m` and make sure to set the `conduitFrom` option later.
    1. If a function returns a `ReaderT r m a` action, change it to `m a`

For example, following these steps for `insertRecord` would make the following changes:

```hs
insertRecord
  :: forall record backend m.
     ( PersistEntityBackend record ~ BaseBackend backend
     , PersistEntity record
     , MonadIO m
     , PersistStoreWrite backend
     )
  => record -> ReaderT backend m record

-- Remove `forall`
insertRecord
  :: ( PersistEntityBackend record ~ BaseBackend backend
     , PersistEntity record
     , MonadIO m
     , PersistStoreWrite backend
     )
  => record -> ReaderT backend m record

-- Update type variables
insertRecord
  :: ( PersistEntityBackend record ~ BaseBackend SqlBackend
     , PersistEntity record
     , MonadIO m
     , PersistStoreWrite SqlBackend
     )
  => record -> ReaderT SqlBackend m record

-- Simplify constraints
insertRecord
  :: ( PersistRecordBackend record SqlBackend
     , MonadIO m
     )
  => record -> ReaderT SqlBackend m record

-- Simplify monads
insertRecord
  :: ( PersistRecordBackend record SqlBackend
     , MonadIO m
     )
  => record -> m record
```

### Updating YAML configuration

Add the function to the YAML file in the following format:

* `name`: The function's name
* `condition`: The CPP condition that must be satisfied for the function to compile successfully
    * e.g. `MIN_VERSION_persistent(2,11,0)` for a function that was added in 2.11.0
* `constraints`: The function's constraints, except for any `Monad m`, `MonadIO m`, or `MonadUnliftIO m` constraints
    * If the function has another `m2` monad type variable, leave those constraints alone
* `args`: The function's arguments
* `result`: The function's result, without the `m` type variable
    * e.g. a function that returns `m (Maybe Foo)` should have `result` set to just `(Maybe Foo)`
    * If this function is a conduit function, leave the result as `ConduitM i o m a`
* `conduitFrom`: Set this to the name of the `pipeSomethingRes` function that corresponds with this function

There should be no references to `m` here.
