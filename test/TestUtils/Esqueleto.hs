{-# LANGUAGE OverloadedStrings #-}

module TestUtils.Esqueleto (
  esqueletoSelect,
) where

import qualified Database.Esqueleto.Experimental as E

import Database.Persist.Monad (MonadSqlQuery, unsafeLiftSql)

esqueletoSelect :: (MonadSqlQuery m, E.SqlSelect a r) => E.SqlQuery a -> m [r]
esqueletoSelect q = unsafeLiftSql "esqueleto-select" (E.select q)
