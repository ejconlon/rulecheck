-- | 

module Rulecheck.RuleExtraction where

import Rulecheck.Monad
import Rulecheck.Rule
import Rulecheck.Typecheck

getRulesFromFile :: String -> GhcM [Rule]
getRulesFromFile f = do
  tcm <- typecheck f
  let rules = getTypecheckedRuleDecls tcm
  mapM ruleFromDecl rules
