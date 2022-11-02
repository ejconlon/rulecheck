module Main (main) where

-- import Test.Rulecheck (testRulecheck)
import Test.Rulecheck.Interface (testInterface)
import Test.Rulecheck.Synth.Search (testSearch)
import Test.Rulecheck.Synth.UnionMap (testUnionMap)
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main = defaultMain $ testGroup "Rulecheck"
  [ testUnionMap
  , testInterface
  , testSearch
  -- NOTE: Disabling these tests for now because they are a bit slow.
  -- , testRulecheck
  ]
