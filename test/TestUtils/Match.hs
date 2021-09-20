{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module TestUtils.Match
  ( (@?~)
  ) where

import Test.Predicates (Predicate(..))
import Test.Tasty.HUnit (Assertion, assertFailure)

-- | An HUnit operator for checking that the given arguments match.
(@?~) :: a -> Predicate a -> Assertion
(@?~) actual expected =
  if accept expected actual
    then return ()
    else assertFailure $ explain expected actual
