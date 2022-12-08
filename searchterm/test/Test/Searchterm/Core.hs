module Test.Searchterm.Core (testCore) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

testCore :: TestTree
testCore = testGroup "Core" [testInferKind]

testInferKind :: TestTree
testInferKind = testCase "inferKind" $ do
  1 + 1 @?= (2 :: Int)
