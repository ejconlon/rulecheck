module Main (main) where

import Test.Rulecheck (testRulecheck)
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main = defaultMain $ testGroup "Rulecheck"
  [ testRulecheck
  ]
