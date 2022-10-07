module DemoTest.Manual.Dummy where

import Test.Tasty (TestTree)
import Test.Tasty.HUnit (testCase, (@?=))

test_simple :: TestTree
test_simple = testCase "simple" $ do
    let actual = (1 + 1) :: Int
        expected = 2 :: Int
    actual @?= expected
