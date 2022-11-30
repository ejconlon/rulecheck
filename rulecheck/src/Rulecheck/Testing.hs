{-# LANGUAGE FlexibleContexts #-} -- Unsafe in general, but probably OK for testing
{-# LANGUAGE FlexibleInstances #-} -- Unsafe in general, but probably OK for testing
{-# LANGUAGE UndecidableInstances #-} -- Unsafe in general, but probably OK for testing
{-# LANGUAGE IncoherentInstances #-} -- Unsafe in general, but probably OK for testing
{-# OPTIONS_GHC -Wno-orphans #-}
module Rulecheck.Testing
  ( TestableRule (..)
  , testTestableRule
  , SomeTestableRule (..)
  , testSomeTestableRule
  , testSomeTestableRules
  ) where

import Control.DeepSeq
import Control.Exception
import Data.Proxy
import Test.QuickCheck (Arbitrary (..), Testable (..), (===))
import Test.Tasty (TestName, TestTree, testGroup)
import Test.Tasty.QuickCheck (Property, counterexample, testProperty, ioProperty)

data TestableRule a z = TestableRule
  { trLhs :: !(a -> z)
  , trRhs :: !(a -> z)
  }


try' :: IO a -> IO (Either SomeException a)
try' = try

(=:=) :: (NFData a, Eq a, Show a) => a -> a -> Property
x =:= y = ioProperty $ do
  x' <- try' (evaluate $ force x)
  y' <- try' (evaluate $ force y)
  return $ case (x', y') of
    (Right xv, Right yv) -> xv === yv
    (Left _, Right _)    -> counterexample "LHS raised an exception, RHS is OK" False
    (Right _,Left _)     -> counterexample "RHS raised an exception, LHS is OK" False
    (Left _, Left _)     -> property True -- Both failed. TODO: Check if they fail in the same way?

instance Arbitrary a => Arbitrary (Proxy a)  where
  arbitrary = return Proxy

-- TODO: This rule is probably bad, remove it eventually
instance {-# OVERLAPS #-} (Num a) => Arbitrary a where
  arbitrary = fmap fromInteger arbitrary

instance Show (a -> b) where
  show _ = "A function"

-- Don't re-order the following four declarations!
-- GHC picks up incoherent instances by the order they appear
-- We should prefer test cases in this order
instance {-# INCOHERENT #-} (
  Arbitrary a, Show a,
  NFData z, Eq z, Show z) => Testable (TestableRule a z) where
  property x = property $ \a -> trLhs x a =:= trRhs x a

instance {-# INCOHERENT #-} (
  Arbitrary a, Show a,
  Arbitrary b, Show b,
  NFData c, Eq c, Show c) => Testable (TestableRule a (b -> c)) where
  property x =
      property $ \a b -> trLhs x a b =:= trRhs x a b

instance {-# INCOHERENT #-} (
  Arbitrary a, Show a,
  Arbitrary b, Show b,
  Arbitrary c, Show c,
  NFData d, Eq d, Show d) => Testable (TestableRule a (b -> c -> d)) where
  property x =
      property $ \a b c -> trLhs x a b c =:= trRhs x a b c

instance {-# INCOHERENT #-} (
  Arbitrary a, Show a,
  Arbitrary b, Show b,
  Arbitrary c, Show c,
  Arbitrary d, Show d,
  NFData e, Eq e, Show e) => Testable (TestableRule a (b -> c -> d -> e)) where
  property x =
      property $ \a b c d -> trLhs x a b c d =:= trRhs x a b c d




testTestableRule :: Testable (TestableRule a z) => TestName -> TestableRule a z -> TestTree
testTestableRule = testProperty

data SomeTestableRule where
  SomeTestableRule :: Testable (TestableRule a z) => TestableRule a z -> SomeTestableRule

testSomeTestableRule :: TestName -> SomeTestableRule -> TestTree
testSomeTestableRule tn (SomeTestableRule tr) = testTestableRule tn tr

testSomeTestableRules :: TestName -> [(TestName, SomeTestableRule)] -> TestTree
testSomeTestableRules gn = testGroup gn . fmap (uncurry testSomeTestableRule)
