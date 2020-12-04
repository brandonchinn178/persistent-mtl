{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module TestUtils.Match
  ( Match(..)
  , (@?~)
  , CanMatch(..)
  ) where

import Test.Tasty.HUnit (Assertion, assertFailure)

data Match a = Match a | Anything

instance Show a => Show (Match a) where
  show Anything = "[...]"
  show (Match a) = show a

-- | An HUnit operator for checking that the given arguments match.
(@?~) :: (CanMatch actual expected, Show actual, Show expected) => actual -> expected -> Assertion
(@?~) actual expected =
  if matches actual expected
    then return ()
    else assertFailure $ unlines
      [ "Expected:"
      , "  " ++ show expected
      , "Got:"
      , "  " ++ show actual
      ]

class CanMatch a b where
  matches :: a -> b -> Bool

instance {-# OVERLAPPABLE #-} Eq a => CanMatch a a where
  matches = (==)

instance CanMatch a b => CanMatch a (Match b) where
  matches _ Anything = True
  matches a (Match b) = matches a b

instance CanMatch a b => CanMatch [a] [b] where
  matches as bs = length as == length bs && and (zipWith matches as bs)

instance (CanMatch a c, CanMatch b d) => CanMatch (Either a b) (Either c d) where
  matches (Left a) (Left c) = matches a c
  matches (Right b) (Right d) = matches b d
  matches _ _ = False
