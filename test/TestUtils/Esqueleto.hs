{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module TestUtils.Esqueleto
  ( esqueletoSelect
  ) where

#if MIN_VERSION_esqueleto(3,5,0)
import qualified Database.Esqueleto.Experimental as E
import qualified Database.Esqueleto.Internal.Internal as E
#else
import qualified Database.Esqueleto as E
import qualified Database.Esqueleto.Internal.Sql as E
#endif

import Database.Persist.Monad (MonadSqlQuery, unsafeLiftSql)

esqueletoSelect :: (MonadSqlQuery m, E.SqlSelect a r) => E.SqlQuery a -> m [r]
esqueletoSelect q = unsafeLiftSql "esqueleto-select" (E.select q)
