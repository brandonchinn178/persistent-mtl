{-# LANGUAGE CPP #-}

module SqlQueryRepSpec (spec) where

import Skeletest
import qualified Skeletest.Predicate as P

import Generated

persistentVersion :: String
#if MIN_VERSION_persistent(2,15,0)
persistentVersion = error "Running tests against persistent > 2.14 is not supported"
#elif MIN_VERSION_persistent(2,14,0)
persistentVersion = "2.14"
#else
persistentVersion = error "Running tests against persistent < 2.14 is not supported"
#endif

spec :: Spec
spec = do
  describe "SqlQueryRep" $ do
    it ("renders with Show (persistent-" <> persistentVersion <> ")") $ do
      unlines allSqlQueryRepShowRepresentations `shouldSatisfy` P.matchesSnapshot
