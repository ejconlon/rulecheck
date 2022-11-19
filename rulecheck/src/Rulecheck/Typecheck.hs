module Rulecheck.Typecheck
  ( getBinds
  , getNameUnsafe
  , getModNameUnsafe
  , getType
  , getTypecheckedRuleDecls
  , getTypeForNameUnsafe
  , typecheck
  ) where

import Control.Monad.IO.Class (liftIO)
import Data.List (find)
import Data.Maybe (fromJust)
import GHC (GhcTc, HscEnv, Kind, LHsBindLR, LHsExpr, LRuleDecl, ModSummary, ModuleName, Name, ParsedModule (pm_mod_summary),
            TyThing (..), TypecheckedMod (..), TypecheckedModule (..), getModuleGraph, mgModSummaries,
            modInfoLookupName, modInfoTopLevelScope, moduleName, ms_mod, parseModule, typecheckModule, pprModule)
import GHC.Core.Utils (exprType)
import GHC.Data.Bag (bagToList)
import GHC.HsToCore (deSugarExpr)
import GHC.Tc.Types (TcGblEnv (..))
import GHC.Types.Var (varType)
import GHC.Utils.Outputable (Outputable (..), showSDocUnsafe)
import HIE.Bios (loadFile)
import Rulecheck.Monad (GhcM)

getModSummaryName :: ModSummary -> String
getModSummaryName s = showSDocUnsafe (pprModule $ ms_mod s)


typecheck :: String -> GhcM [TypecheckedModule]
typecheck filename = do
  -- For some reason, this does not return the expected result.
  -- Get the typechecked module from the dependency graph.
  _      <- loadFile (filename, filename)
  graph  <- getModuleGraph
  parsed <- mapM parseModule (mgModSummaries graph)
  liftIO $ putStrLn $ "Parsed " ++ show (length parsed) ++ " modules from file " ++ filename
  mapM typecheckModule parsed

tcmName :: TypecheckedModule -> ModuleName
tcmName = moduleName . ms_mod . pm_mod_summary . tm_parsed_module

getModNameUnsafe :: TypecheckedModule -> String
getModNameUnsafe mod = showSDocUnsafe (ppr (tcmName mod))

-- | For debugging only
getNameUnsafe :: TypecheckedModule -> String -> Maybe Name
getNameUnsafe tcm symName =
  let
    topScope = fromJust $ modInfoTopLevelScope (moduleInfo tcm)
  in
    find (\n -> showSDocUnsafe (ppr n) == symName) topScope

-- | For debugging only
getBinds :: TypecheckedModule -> [LHsBindLR GhcTc GhcTc]
getBinds tcm = bagToList $ typecheckedSource tcm

getTypecheckedRuleDecls :: TypecheckedModule -> [LRuleDecl GhcTc]
getTypecheckedRuleDecls tcm = tcg_rules $ fst $ tm_internals_ tcm

-- | For debugging only
getTypeForNameUnsafe :: TypecheckedModule -> String -> GhcM (Maybe Kind)
getTypeForNameUnsafe tcm symName
  | Just name <- getNameUnsafe tcm symName
  = do
      thing <- modInfoLookupName (moduleInfo tcm) name
      return $ case thing of
        Just (AnId ident) -> Just $ varType ident
        Just _         -> error "TODO"
        Nothing        -> Nothing
  | otherwise = return Nothing


-- | For debugging only
getModuleTopLevelScope :: TypecheckedModule -> [String]
getModuleTopLevelScope m =
  map (showSDocUnsafe . ppr) $ fromJust $ modInfoTopLevelScope $ moduleInfo m

-- | Obtains the type of a given expression. Note that this requires converting
--   the expression to Core and computing the resulting type.
--   In GHC 9.4.2, this can be done in a pure fashion, see:
--   https://hackage.haskell.org/package/ghc-9.4.2/docs/GHC-Hs-Syn-Type.html
getType :: HscEnv -> LHsExpr GhcTc -> IO (Maybe Kind)
getType env expr = do
  (_, coreExpr) <- deSugarExpr env expr
  return (exprType <$> coreExpr)
