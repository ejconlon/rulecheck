-- Here I'm working out what the test interface will be.
module Rulecheck.TestSketch where

import Test.QuickCheck (Arbitrary (..), (===), Testable (..))
import Test.Tasty (TestName, TestTree)
import Test.Tasty.QuickCheck (testProperty)

data Pair1 a1 z = Pair1
  { p1Lhs :: !(a1 -> z)
  , p1Rhs :: !(a1 -> z)
  }

instance (Arbitrary a1, Show a1, Eq z, Show z) => Testable (Pair1 a1 z) where
  property x = property $ \a1 -> p1Lhs x a1 === p1Rhs x a1

data Pair2 a1 a2 z = Pair2
  { p2Lhs :: !(a1 -> a2 -> z)
  , p2Rhs :: !(a1 -> a2 -> z)
  }

instance (Arbitrary a1, Show a1, Arbitrary a2, Show a2, Eq z, Show z) => Testable (Pair2 a1 a2 z) where
  property x = property $ \a1 -> property (Pair1 (p2Lhs x a1) (p2Rhs x a1))

data Pair3 a1 a2 a3 z = Pair3
  { p3Lhs :: !(a1 -> a2 -> a3 -> z)
  , p3Rhs :: !(a1 -> a2 -> a3 -> z)
  }

instance (Arbitrary a1, Show a1, Arbitrary a2, Show a2, Arbitrary a3, Show a3, Eq z, Show z) => Testable (Pair3 a1 a2 a3 z) where
  property x = property $ \a1 -> property (Pair2 (p3Lhs x a1) (p3Rhs x a1))

testPair1 :: (Arbitrary a1, Show a1, Eq z, Show z) => TestName -> Pair1 a1 z -> TestTree
testPair1 = testProperty

testPair2 :: (Arbitrary a1, Show a1, Arbitrary a2, Show a2, Eq z, Show z) => TestName -> Pair2 a1 a2 z -> TestTree
testPair2 = testProperty

testPair3 :: (Arbitrary a1, Show a1, Arbitrary a2, Show a2, Arbitrary a3, Show a3, Eq z, Show z) => TestName -> Pair3 a1 a2 a3 z -> TestTree
testPair3 = testProperty

-- data Apply1 a1 z = Apply1
--   { a1Gen1 :: !(Gen a1)
--   , a1Lhs :: !(a1 -> z)
--   , a1Rhs :: !(a1 -> z)
--   }

-- mkApply1 :: Arbitrary a1 => (a1 -> z) -> (a1 -> z) -> Apply1 a1 z
-- mkApply1 = Apply1 arbitrary

-- runApply1 :: Apply1 a1 z -> Gen (z, z)
-- runApply1 x = do
--   a1 <- a1Gen1 x
--   let !zl = a1Lhs x a1
--       !zr = a1Lhs x a1
--   pure (zl, zr)

-- data Apply2 a1 a2 z = Apply2
--   { a2Gen1 :: !(Gen a1)
--   , a2Gen2 :: !(Gen a2)
--   , a2Lhs :: !(a1 -> a2 -> z)
--   , a2Rhs :: !(a1 -> a2 -> z)
--   }

-- mkApply2 :: (Arbitrary a1, Arbitrary a2) => (a1 -> a2 -> z) -> (a1 -> a2 -> z) -> Apply2 a1 a2 z
-- mkApply2 = Apply2 arbitrary arbitrary

-- runApply2 :: Apply2 a1 a2 z -> Gen (z, z)
-- runApply2 x = do
--   a1 <- a2Gen1 x
--   a2 <- a2Gen2 x
--   let !zl = a2Lhs x a1 a2
--       !zr = a2Lhs x a1 a2
--   pure (zl, zr)

-- data Apply3 a1 a2 a3 z = Apply3
--   { a3Gen1 :: !(Gen a1)
--   , a3Gen2 :: !(Gen a2)
--   , a3Gen3 :: !(Gen a3)
--   , a3Lhs :: !(a1 -> a2 -> a3 -> z)
--   , a3Rhs :: !(a1 -> a2 -> a3 -> z)
--   }

-- mkApply3 :: (Arbitrary a1, Arbitrary a2, Arbitrary a3) => (a1 -> a2 -> a3 -> z) -> (a1 -> a2 -> a3 -> z) -> Apply3 a1 a2 a3 z
-- mkApply3 = Apply3 arbitrary arbitrary arbitrary

-- runApply3 :: Apply3 a1 a2 a3 z -> Gen (z, z)
-- runApply3 x = do
--   a1 <- a3Gen1 x
--   a2 <- a3Gen2 x
--   a3 <- a3Gen3 x
--   let !zl = a3Lhs x a1 a2 a3
--       !zr = a3Rhs x a1 a2 a3
--   pure (zl, zr)

-- pairProp :: (Eq z, Show z) => Gen (z, z) -> Property
-- pairProp g = forAll g (uncurry (===))
