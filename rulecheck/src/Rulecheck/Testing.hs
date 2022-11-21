module Rulecheck.Testing
  ( TestableRule (..)
  , testTestableRule
  , SomeTestableRule (..)
  , testSomeTestableRule
  , testSomeTestableRules
  ) where

import Test.QuickCheck (Arbitrary (..), Testable (..), (===))
import Test.Tasty (TestName, TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

data TestableRule a z = TestableRule
  { trLhs :: !(a -> z)
  , trRhs :: !(a -> z)
  }

instance {-# OVERLAPPING #-} (Arbitrary a, Show a, Arbitrary b, Show b, Eq c, Show c) => Testable (TestableRule a (b -> c)) where
  property x =
      property $ \a b -> trLhs x a b === trRhs x a b

instance {-# OVERLAPPABLE #-} (Arbitrary a, Show a, Eq z, Show z) => Testable (TestableRule a z) where
  property x = property $ \a -> trLhs x a === trRhs x a

testTestableRule :: Testable (TestableRule a z) => TestName -> TestableRule a z -> TestTree
testTestableRule = testProperty

data SomeTestableRule where
  SomeTestableRule :: Testable (TestableRule a z) => TestableRule a z -> SomeTestableRule

testSomeTestableRule :: TestName -> SomeTestableRule -> TestTree
testSomeTestableRule tn (SomeTestableRule tr) = testTestableRule tn tr

testSomeTestableRules :: TestName -> [(TestName, SomeTestableRule)] -> TestTree
testSomeTestableRules gn = testGroup gn . fmap (uncurry testSomeTestableRule)
