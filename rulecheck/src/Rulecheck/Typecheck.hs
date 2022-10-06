module Rulecheck.Typecheck
  ( getBinds
  , getNameUnsafe
  , getRuleArguments
  , getRuleBody
  , getTypecheckedRules
  , getTypeForNameUnsafe
  , typecheck
  ) where

import Control.Monad.IO.Class (MonadIO (..))
import Data.List (find)
import Data.Maybe (fromJust)
import Data.Void (Void)
import HIE.Bios
import Rulecheck.Monad (GhcM)
import GHC
import GHC.Tc.Types
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

getTypecheckedRules :: TypecheckedModule -> [LRuleDecl GhcTc]
getTypecheckedRules tcm = tcg_rules $ fst $ tm_internals_ tcm

-- | Returns the left and right-hand-sides of the rule
getRuleBody :: LRuleDecl GhcTc -> (HsExpr GhcTc, HsExpr GhcTc)
getRuleBody decl =
  let
    decl' = unLoc decl
  in
    (unLoc (rd_lhs decl'), unLoc (rd_rhs decl'))

getRuleArguments :: LRuleDecl GhcTc -> [Var]
getRuleArguments decl =
  let
    args = rd_tmvs (unLoc decl)
  in
    map (getID . unLoc) args

  where
    getID (RuleBndr _ id) = unLoc id
    getID _               = error "unimplemented" -- TODO

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
