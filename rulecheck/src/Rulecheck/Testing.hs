module Rulecheck.Testing
  ( TestableRule (..)
  , testTestableRule
  , SomeTestableRule (..)
  , testSomeTestableRule
  , testSomeTestableRules
  , primFloatRule
  -- , primFloatRule2
  -- , primFloatRule3
  , primDoubleRule
  -- , primDoubleRule2
  -- , primDoubleRule3
  ) where

import GHC.Exts
import Test.QuickCheck (Arbitrary (..), Testable (..), (===))
import Test.Tasty (TestName, TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

data TestableRule a z = TestableRule
  { trLhs :: !(a -> z)
  , trRhs :: !(a -> z)
  }

instance (Arbitrary a, Show a, Eq z, Show z) => Testable (TestableRule a z) where
  property x = property $ \a -> trLhs x a === trRhs x a

testTestableRule :: (Arbitrary a, Show a, Eq z, Show z) => TestName -> TestableRule a z -> TestTree
testTestableRule = testProperty

data SomeTestableRule where
  SomeTestableRule :: (Arbitrary a, Show a, Eq z, Show z) => TestableRule a z -> SomeTestableRule

testSomeTestableRule :: TestName -> SomeTestableRule -> TestTree
testSomeTestableRule tn (SomeTestableRule tr) = testTestableRule tn tr

testSomeTestableRules :: TestName -> [(TestName, SomeTestableRule)] -> TestTree
testSomeTestableRules gn = testGroup gn . fmap (uncurry testSomeTestableRule)

-- Due to restrictions on where unboxed types can appear, we need some special cases
-- This can probably be simplified with templatehaskell or type families

primFloatRule :: (Float# -> Float#) -> (Float# -> Float#) -> TestableRule Float Float
primFloatRule f g = TestableRule (lift f) (lift g) where
  lift :: (Float# -> Float#) -> (Float -> Float)
  lift bf (F# x) = F# (bf x)

primDoubleRule :: (Double# -> Double#) -> (Double# -> Double#) -> TestableRule Double Double
primDoubleRule f g = TestableRule (lift f) (lift g) where
  lift :: (Double# -> Double#) -> (Double -> Double)
  lift bf (D# x) = D# (bf x)
