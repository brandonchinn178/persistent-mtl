# Unreleased

# v0.5.2

* Add GHC 9.10 support
* Drop support for GHC < 9.8

# v0.5.1

* Add GHC 9.8 support
* Drop support for GHC < 9.4

# v0.5.0.1

* Add GHC 9.4 support

# v0.5.0.0

* Export `SqlQueryT` constructor ([#46](https://github.com/brandonchinn178/persistent-mtl/pull/46))
* Drop support for GHC < 8.10
* Drop support for `persistent` < 2.13
* Add support for `persistent-2.14`
* Add `Database.Persist.Sql.Shim` module that re-exports `Database.Persist.Sql` and `Database.Persist.Monad.Shim`
* Add `MonadFix` instance to `SqlQueryT` and `SqlTransaction`
* Add `getSqlBackendPool`
* Move `rerunnableLift` into `MonadRerunnableTrans` typeclass in `Control.Monad.Trans.Rerunnable`
* Add `catchSqlTransaction`
* Add `retryCallback` to `SqlQueryEnv`

# v0.4.0.0

* Add some mtl instances: `MonadThrow`, `MonadCatch`, `MonadMask`, `MonadLogger`, `MonadReader`
* Removed support for GHC 8.2, 8.4
* Add `MonadSqlQuery (TransactionM m)` superclass constraint to allow writing functions generic on some `MonadSqlQuery m` using `withTransaction`, as shown in examples in README

# v0.3.0.0

* Add `unsafeLiftSql` ([#38](https://github.com/brandonchinn178/persistent-mtl/pull/38))

# v0.2.2.0

* Fix for persistent 2.13

# v0.2.1.0

* Add `rerunnableLift` for `SqlTransaction`
* Use `unliftio-pool` instead of `resourcet-pool`, which has better async exeception safety

# v0.2.0.0

* Use a separate monad within `withTransaction` to prevent unsafe/arbitrary IO actions ([#7](https://github.com/brandonchinn178/persistent-mtl/issues/7), [#28](https://github.com/brandonchinn178/persistent-mtl/issues/28))
* Add `MonadRerunnableIO` to support IO actions within `withTransaction` only if the IO action is determined to be rerunnable
* Add built-in support for retrying transactions if a serialization error occurs
* Remove `SqlQueryRep` as an export from `Database.Persist.Monad`. You shouldn't ever need it for normal usage. It is now re-exported by `Database.Persist.Monad.TestUtils`, since most of the usage of `SqlQueryRep` is in mocking queries. If you need it otherwise, you can import it directly from `Database.Persist.Monad.SqlQueryRep`.

# v0.1.0.1

Fix quickstart

# v0.1.0.0

Initial release
* `SqlQueryT` + `MonadSqlQuery`
* Autogenerated persistent API
* `MockQueryT`
