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

-- import Basement.UArray
-- import Basement.Block
-- import Basement.Types.OffsetSize
-- import qualified Basement.From
-- import qualified Basement.Imports
-- import qualified Basement.UTF8.Base
import Control.DeepSeq
import Control.Exception
import Data.Proxy
-- import Foreign.Ptr
-- import GHC.Word
import Test.QuickCheck (Arbitrary (..), Testable (..), (===))
import Test.Tasty (TestName, TestTree, testGroup)
import Test.Tasty.QuickCheck (Property, counterexample, testProperty, ioProperty)

data TestableRule a z = TestableRule
  { trLhs :: !(a -> z)
  , trRhs :: !(a -> z)
  }

-- instance Arbitrary Basement.Imports.String where
--   arbitrary = fmap Basement.Imports.fromString arbitrary

-- instance NFData Basement.Imports.String where
--   rnf (Basement.UTF8.Base.String a) = seq a ()

-- instance Arbitrary (Ptr ()) where
--   arbitrary = return nullPtr

-- instance (Arbitrary a, PrimType a) => Arbitrary (UArray a) where
--   arbitrary = fmap (Basement.From.from :: Block a -> UArray a) arbitrary

-- instance (Arbitrary a, PrimType a) => Arbitrary (Block a) where
--   arbitrary = fmap Basement.Imports.fromList arbitrary


-- instance NFData (UArray a) where
--   rnf (UArray a b _) = seq (a, b) ()

-- instance NFData (CountOf a) where
--   rnf (CountOf x) = seq x ()

-- instance NFData a => NFData (Offset a) where
--   rnf (Offset x) = seq x ()

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

instance {-# OVERLAPPING #-} (
  Arbitrary a, Show a,
  Arbitrary b, Show b,
  Arbitrary c, Show c,
  Arbitrary d, Show d,
  NFData e, Eq e, Show e) => Testable (TestableRule a (b -> c -> d -> e)) where
  property x =
      property $ \a b c d -> trLhs x a b c d =:= trRhs x a b c d

instance {-# OVERLAPPING #-} (
  Arbitrary a, Show a,
  Arbitrary b, Show b,
  Arbitrary c, Show c,
  NFData d, Eq d, Show d) => Testable (TestableRule a (b -> c -> d)) where
  property x =
      property $ \a b c -> trLhs x a b c =:= trRhs x a b c

instance {-# OVERLAPPING #-} (
  Arbitrary a, Show a,
  Arbitrary b, Show b,
  NFData c, Eq c, Show c) => Testable (TestableRule a (b -> c)) where
  property x =
      property $ \a b -> trLhs x a b =:= trRhs x a b

instance {-# OVERLAPPABLE #-} (
  Arbitrary a, Show a,
  NFData z, Eq z, Show z) => Testable (TestableRule a z) where
  property x = property $ \a -> trLhs x a =:= trRhs x a

testTestableRule :: Testable (TestableRule a z) => TestName -> TestableRule a z -> TestTree
testTestableRule = testProperty

data SomeTestableRule where
  SomeTestableRule :: Testable (TestableRule a z) => TestableRule a z -> SomeTestableRule

testSomeTestableRule :: TestName -> SomeTestableRule -> TestTree
testSomeTestableRule tn (SomeTestableRule tr) = testTestableRule tn tr

testSomeTestableRules :: TestName -> [(TestName, SomeTestableRule)] -> TestTree
testSomeTestableRules gn = testGroup gn . fmap (uncurry testSomeTestableRule)
