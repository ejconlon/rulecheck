-- | 

module Rulecheck.RuleExtraction where

import Control.Monad.IO.Class (liftIO)
import GHC (GhcTc, HscEnv, Kind, LHsBindLR, LHsExpr, LRuleDecl, ModSummary, ModuleName, Name, ParsedModule (pm_mod_summary),
            TyThing (..), TypecheckedMod (..), TypecheckedModule (..), getModuleGraph, mgModSummaries,
            modInfoLookupName, modInfoTopLevelScope, moduleName, ms_mod, parseModule, typecheckModule, pprModule)
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
    getRuleDecls mod =
      let
        decls = getTypecheckedRuleDecls mod
      in
        liftIO (putStrLn $
          "Module " ++ getModNameUnsafe mod ++ " yielded " ++ show (length decls) ++ " rules") >>
          return decls
