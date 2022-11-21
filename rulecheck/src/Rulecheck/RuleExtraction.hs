-- | 

module Rulecheck.RuleExtraction where

import Control.Monad.IO.Class (liftIO)
import GHC
import Rulecheck.Monad
import Rulecheck.Rule
import Rulecheck.Typecheck

getRulesFromFile :: String -> GhcM [Rule]
getRulesFromFile f = do
  tcm      <- typecheck f
  modRules <- mapM getRuleDecls tcm
  let rules = concat modRules
  liftIO $ putStrLn $ "Obtained " ++ show (length rules) ++ " rules from file " ++ f
  mapM ruleFromDecl rules
  where
    getRuleDecls :: TypecheckedModule -> GhcM [LRuleDecl GhcTc]
    getRuleDecls m =
      let
        decls   = getTypecheckedRuleDecls m
        modName = getModNameUnsafe m
      in
        liftIO (putStrLn $ "Module " ++ modName ++ " yielded "
                    ++ show (length decls) ++ " rules") >> return decls
