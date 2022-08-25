{- AUTOCOLLECT.TEST -}
{-# LANGUAGE CPP #-}

module SqlQueryRepTest (
  -- $AUTOCOLLECT.TEST.export$
) where

import qualified Data.ByteString.Lazy.Char8 as Char8
import Test.Tasty
import Test.Tasty.Golden

import Generated

persistentVersionDir :: FilePath
#if MIN_VERSION_persistent(2,15,0)
persistentVersionDir = error "Running tests against persistent > 2.14 is not supported"
#elif MIN_VERSION_persistent(2,14,0)
persistentVersionDir = "persistent-2.14/"
#elif MIN_VERSION_persistent(2,13,0)
persistentVersionDir = "persistent-2.13/"
#else
persistentVersionDir = error "Running tests against persistent < 2.13 is not supported"
#endif

test =
  golden "Show representation" (persistentVersionDir ++ "sqlqueryrep_show_representation.golden") $
    pure $
      unlines allSqlQueryRepShowRepresentations

golden :: String -> FilePath -> IO String -> TestTree
golden name fp action = goldenVsStringDiff name diffCmd ("test/goldens/" ++ fp) $ Char8.pack <$> action
  where
    diffCmd expected actual = ["diff", "-u", expected, actual]
