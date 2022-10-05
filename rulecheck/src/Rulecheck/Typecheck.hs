module Rulecheck.Typecheck (getNameUnsafe, typecheck) where

import Control.Monad.IO.Class (MonadIO (..))
import Data.List (find)
import Data.Maybe (fromJust)
import HIE.Bios
import Rulecheck.Monad (GhcM)
import GHC
import GHC.Utils.Outputable

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

    getCradle :: IO (Cradle ())
    getCradle = loadImplicitCradle filename

getNameUnsafe :: TypecheckedModule -> String -> Maybe Name
getNameUnsafe tcm symName =
  let
    topScope = fromJust $ modInfoTopLevelScope (moduleInfo tcm)
  in
    find (\n -> showSDocUnsafe (ppr n) == symName) topScope

getModuleTopLevelScope :: TypecheckedModule -> [String]
getModuleTopLevelScope m =
  map (showSDocUnsafe . ppr) $ fromJust $ modInfoTopLevelScope $ moduleInfo m
