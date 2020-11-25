# Generating lifted persistent functions

## Usage

```
scripts/generate/run.sh
```

## Configuration

Configuration lives in `persistent-api.yaml`. The file contains a list of objects, where each object corresponds to a `persistent` function that we want to lift into the `MonadSqlQuery` context. Follow the instructions in this guide to update the YAML file, which will generate the appropriate Haskell code. If you're trying to add a function that doesn't fit neatly with the instructions or categories, bring up a discussion with other developers.

To add a function, write down its type then do the following:

1. Update type variables
    * Specialize `backend` as `SqlBackend`
    * Ensure the `PersistEntity` type variable is named `record`. If there are multiple `PersistEntity` types, they should be named `record1`, `record2`, etc. Choose an order that makes sense for `withRecord`, e.g.

        ```hs
        withRecord @(record1, record2, ...) $ ...
        ```

1. Simplify constraints
    * Remove redundant constraints, e.g. `PersistQueryRead SqlBackend`
    * Substitute `PersistRecordBackend`, if applicable

1. Look at the monad type(s) in the function:
    1. If there are multiple monad types, go to the "Advanced" section
    1. If the monad has a constraint other than `Monad`, `MonadIO`, or `MonadUnliftIO`, go to the "Advanced" section
    1. Otherwise, go to the "Basic" section

### Basic

First, make the following modifications:

1. Ensure the monad type variable is named `m`
1. Remove all `Monad`, `MonadIO`, and `MonadUnliftIO` constraints
    * At this point, the monad shouldn't have any constraints
1. Rename `ReaderT SqlBackend m a` as `m a`

Then add the function to the YAML file in the following format:

* `name`: The function's name
* `condition`: (optional) The CPP condition that must be satisfied for the function to compile successfully
    * e.g. `MIN_VERSION_persistent(2,11,0)` for a function that was added in 2.11.0
* `constraints`: The function's constraints
* `args`: The function's arguments
* `result`: The function's result, without the monad
    * e.g. a function that returns `m (Maybe Foo)` should have `result` set to just `(Maybe Foo)`

There should be no references to `m` here.

### Advanced

TODO
