module Rulecheck.Typecheck
  ( getBinds
  , getNameUnsafe
  , getType
  , getTypecheckedRuleDecls
  , getTypeForNameUnsafe
  , typecheck
  ) where

import Control.Monad.IO.Class (MonadIO (..))
import Data.List (find)
import Data.Maybe (fromJust)
import Data.Void (Void)
import HIE.Bios
import Rulecheck.Monad (GhcM)
import GHC hiding (exprType)
import GHC.Core.Utils (exprType)
import GHC.Tc.Types
import GHC.HsToCore
import GHC.Data.Bag (bagToList)
import GHC.Utils.Outputable
import GHC.Types.Var(Var, varType)

typecheck :: String -> GhcM a TypecheckedModule
typecheck filename =
  do
    cradle            <- liftIO getCradle
    compileOptsResult <- liftIO $ getCompilerOptions filename cradle
    let opts = fromCradleLoadResult compileOptsResult
    _                <- initSession opts
    -- For some reason, this does not return the expected result.
    -- Get the typechecked module from the dependency graph.
    _                <- loadFile (filename, filename)
    graph            <- getModuleGraph
    let [modSummary] = mgModSummaries graph
    parsed           <- parseModule modSummary
    typecheckModule parsed

  where

    fromCradleLoadResult (CradleSuccess r) = r
    fromCradleLoadResult other             = error (show other)

    getCradle :: IO (Cradle Void)
    getCradle = do
      maybeCradle <- findCradle filename
      case maybeCradle of
        Just c  -> loadCradle c
        Nothing -> loadImplicitCradle filename

getNameUnsafe :: TypecheckedModule -> String -> Maybe Name
getNameUnsafe tcm symName =
  let
    topScope = fromJust $ modInfoTopLevelScope (moduleInfo tcm)
  in
    find (\n -> showSDocUnsafe (ppr n) == symName) topScope

getBinds :: TypecheckedModule -> [LHsBindLR GhcTc GhcTc]
getBinds tcm = bagToList $ typecheckedSource tcm

getTypecheckedRuleDecls :: TypecheckedModule -> [LRuleDecl GhcTc]
getTypecheckedRuleDecls tcm = tcg_rules $ fst $ tm_internals_ tcm

getTypeForNameUnsafe :: TypecheckedModule -> String -> GhcM a (Maybe Kind)
getTypeForNameUnsafe tcm symName
  | Just name <- getNameUnsafe tcm symName
  = do
      thing <- modInfoLookupName (moduleInfo tcm) name
      return $ case thing of
        Just (AnId id) -> Just $ varType id
        Just _         -> undefined -- TODO
        Nothing        -> Nothing
  | otherwise = return Nothing


getModuleTopLevelScope :: TypecheckedModule -> [String]
getModuleTopLevelScope m =
  map (showSDocUnsafe . ppr) $ fromJust $ modInfoTopLevelScope $ moduleInfo m

getType :: HscEnv -> LHsExpr GhcTc -> IO (Maybe Kind)
getType env expr = do
  (_, coreExpr) <- deSugarExpr env expr
  return (exprType <$> coreExpr)
