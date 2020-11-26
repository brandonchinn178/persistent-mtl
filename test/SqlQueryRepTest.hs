{-# LANGUAGE CPP #-}

module SqlQueryRepTest where

import qualified Data.ByteString.Lazy.Char8 as Char8
import Test.Tasty
import Test.Tasty.Golden

import Generated

{-# ANN module "HLint: ignore" #-}

persistentVersionDir :: FilePath
#if MIN_VERSION_persistent(2,11,0)
persistentVersionDir = "persistent-2.11/"
#elif MIN_VERSION_persistent(2,10,0)
persistentVersionDir = "persistent-2.10/"
#elif MIN_VERSION_persistent(2,9,0)
persistentVersionDir = "persistent-2.9/"
#elif MIN_VERSION_persistent(2,8,0)
persistentVersionDir = "persistent-2.8/"
#else
persistentVersionDir = undefined
#endif

tests :: TestTree
tests = testGroup "SqlQueryRep tests"
  [ golden "Show representation" (persistentVersionDir ++ "sqlqueryrep_show_representation.golden") $
      pure $ unlines allSqlQueryRepShowRepresentations
  ]

golden :: String -> FilePath -> IO String -> TestTree
golden name fp action = goldenVsStringDiff name diffCmd ("test/goldens/" ++ fp) $ Char8.pack <$> action
  where
    diffCmd expected actual = ["diff", "-u", expected, actual]
