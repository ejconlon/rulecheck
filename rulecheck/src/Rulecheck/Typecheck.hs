module Rulecheck.Typecheck
  ( getBinds
  , getNameUnsafe
  , getType
  , getTypecheckedRuleDecls
  , getTypeForNameUnsafe
  , typecheck
  ) where

import Data.List (find)
import Data.Maybe (fromJust)
import GHC (GhcTc, HscEnv, Kind, LHsBindLR, LHsExpr, LRuleDecl, Name, TyThing (..), TypecheckedMod (..),
            TypecheckedModule (..), getModuleGraph, mgModSummaries, modInfoLookupName, modInfoTopLevelScope,
            parseModule, typecheckModule)
import GHC.Core.Utils (exprType)
import GHC.Data.Bag (bagToList)
import GHC.HsToCore (deSugarExpr)
import GHC.Tc.Types (TcGblEnv (..))
import GHC.Types.Var (varType)
import GHC.Utils.Outputable (Outputable (..), showSDocUnsafe)
import HIE.Bios (loadFile)
import Rulecheck.Monad (GhcM)

typecheck :: String -> GhcM TypecheckedModule
typecheck filename = do
  -- For some reason, this does not return the expected result.
  -- Get the typechecked module from the dependency graph.
  _                <- loadFile (filename, filename)
  graph            <- getModuleGraph
  case mgModSummaries graph of
    [modSummary] ->  do
      parsed <- parseModule modSummary
      typecheckModule parsed
    _ -> fail "Expected one module"

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
