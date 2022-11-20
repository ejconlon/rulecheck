module Rulecheck.Testing
  ( TestableRule (..)
  , testTestableRule
  , SomeTestableRule (..)
  , testSomeTestableRule
  , testSomeTestableRules
  , primFloatRule
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

-- Due to restrictions on where unboxed types can appear, we need some special cases
primFloatRule :: (Float# -> Float#) -> (Float# -> Float#) -> SomeTestableRule
primFloatRule f g = SomeTestableRule (TestableRule (lift f) (lift g)) where
  lift :: (Float# -> Float#) -> (Float -> Float)
  lift bf (F# x) = F# (bf x)


testSomeTestableRule :: TestName -> SomeTestableRule -> TestTree
testSomeTestableRule tn (SomeTestableRule tr) = testTestableRule tn tr

testSomeTestableRules :: TestName -> [(TestName, SomeTestableRule)] -> TestTree
testSomeTestableRules gn = testGroup gn . fmap (uncurry testSomeTestableRule)
