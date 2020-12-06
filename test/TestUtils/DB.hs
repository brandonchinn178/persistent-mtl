module TestUtils.DB
  ( withTestDB
  ) where

import Control.Monad.Logger (runNoLoggingT)
import Data.Pool (Pool)
import qualified Data.Text as Text
import Database.Persist.Sql (SqlBackend)
import Database.Persist.Sqlite (withSqlitePool)
import UnliftIO (liftIO, withSystemTempDirectory)

withTestDB :: (Pool SqlBackend -> IO a) -> IO a
withTestDB f =
  withSystemTempDirectory "persistent-mtl-testapp" $ \dir -> do
    let db = Text.pack $ dir ++ "/db.sqlite"
    runNoLoggingT $ withSqlitePool db 5 $ \pool -> liftIO $ f pool
