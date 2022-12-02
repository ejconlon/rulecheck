{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-} -- Unsafe in general, but probably OK for testing
{-# LANGUAGE FlexibleInstances #-} -- Unsafe in general, but probably OK for testing
{-# LANGUAGE UndecidableInstances #-} -- Unsafe in general, but probably OK for testing
{-# LANGUAGE IncoherentInstances #-} -- Unsafe in general, but probably OK for testing
{-# LANGUAGE ScopedTypeVariables #-}
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
import Data.Typeable
import Test.QuickCheck (Arbitrary (..), Testable (..), (===))
import Test.Tasty (TestName, TestTree, testGroup)
import Test.Tasty.QuickCheck (Property, counterexample, testProperty, ioProperty)
import Text.Printf

data TestableRule a z = TestableRule
  { trLhs :: !(a -> z)
  , trRhs :: !(a -> z)
  }


try' :: IO a -> IO (Either SomeException a)
try' = try

type Comparable a = (NFData a, Eq a, Show a, Typeable a)

(=:=) :: Comparable a => a -> a -> Property
x =:= y = ioProperty $ do
  x' <- try' (evaluate $ force x)
  y' <- try' (evaluate $ force y)
  return $ case (x', y') of
    (Right xv, Right yv) -> comp xv yv
    (Left _, Right _)    -> counterexample "LHS raised an exception, RHS is OK" False
    (Right _,Left _)     -> counterexample "RHS raised an exception, LHS is OK" False
    (Left _, Left _)     -> property True -- Both failed. TODO: Check if they fail in the same way?
  where
    comp lhs rhs
      | (Just (lhs' :: Float), Just (rhs' :: Float)) <- (cast lhs, cast rhs)
      = compFloating lhs' rhs'
    comp lhs rhs
      | (Just (lhs' :: Double), Just (rhs' :: Double)) <- (cast lhs, cast rhs)
      = compFloating lhs' rhs'
    comp lhs rhs = lhs === rhs


    compFloating :: (PrintfArg a, RealFloat a) => a -> a -> Property
    compFloating lhs rhs | isNaN lhs && isNaN rhs = property True
    compFloating lhs rhs = counterexample errString $ abs (lhs - rhs) <= epsilon
      where
        errString :: String
        errString = printf "LHS was %f, RHS was %f" lhs rhs

        epsilon = 0



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
  Arbitrary a, Show a, Comparable z
  ) => Testable (TestableRule a z) where
  property x = property $ \a -> trLhs x a =:= trRhs x a

instance {-# INCOHERENT #-} (
  Arbitrary a, Show a,
  Arbitrary b, Show b,
  Comparable c) => Testable (TestableRule a (b -> c)) where
  property x =
      property $ \a b -> trLhs x a b =:= trRhs x a b

instance {-# INCOHERENT #-} (
  Arbitrary a, Show a,
  Arbitrary b, Show b,
  Arbitrary c, Show c,
  Comparable d) => Testable (TestableRule a (b -> c -> d)) where
  property x =
      property $ \a b c -> trLhs x a b c =:= trRhs x a b c

instance {-# INCOHERENT #-} (
  Arbitrary a, Show a,
  Arbitrary b, Show b,
  Arbitrary c, Show c,
  Arbitrary d, Show d,
  Comparable e) => Testable (TestableRule a (b -> c -> d -> e)) where
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
