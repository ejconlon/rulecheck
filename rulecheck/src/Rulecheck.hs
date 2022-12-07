{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
module Rulecheck where

import Control.Monad (when, unless)
import Data.Aeson (eitherDecodeFileStrict)
import qualified Data.Map as M
import Data.Either (fromRight)
import Data.Map (Map)
import Data.List (isInfixOf)
import Data.List.Utils
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Debug.Trace (trace)
import Prelude hiding (lines)
import Prettyprinter
import Rulecheck.Config
import Rulecheck.Monad (cradleGhcM)
import Rulecheck.Rendering
import Rulecheck.Rule
import Rulecheck.RuleRendering (TestModuleRenderOpts(..), ruleModuleDoc)
import Rulecheck.RuleExtraction
import Searchterm.Synth.Search
import Searchterm.Util
import Searchterm.Interface.Core hiding (ruleName)
import Searchterm.Interface.Decl (mkLineDecls)
import Searchterm.Interface.ParenPretty (docToText)
import Searchterm.Interface.Parser
import Searchterm.Interface.Printer
import Searchterm.Interface.Names
import Searchterm.Interface.Types (Line)
import System.Directory
import System.Environment (getArgs)
import System.Environment.Blank (getEnvDefault)
import Text.Printf

packageFilePath :: String -> PackageDescription -> String -> FilePath
packageFilePath prefix pkg file =
  prefix ++  "/" ++ name pkg ++ "-" ++ version pkg ++ "/" ++ file

data GenerateOptions =
  GenerateOptions
    { srcFile    :: FilePath
    , genModName :: String
    , genDeps    :: Set String
    , genModFile :: FilePath
    }

getModContents :: GenerateOptions -> IO String
getModContents (GenerateOptions {srcFile, genModName, genDeps}) =
  cradleGhcM srcFile $ do
    rulesInFile <- getRulesFromFile srcFile
    let rules = filter (not . skipRule srcFile . ruleName) rulesInFile

    let renderOpts = TestModuleRenderOpts genModName genDeps (overrideTypeSigs srcFile)

    -- `modDoc` is a "preliminary" version of the output test suite that may not
    -- include all necessary module inputs
    let modDoc = ruleModuleDoc renderOpts rules

    -- Determines the necessary module imports by identifying all of the Name's
    -- that need to be rendered in `modDoc`
    additionalImports <- Set.map internalToPublicMod <$> identifyRequiredImports modDoc

    -- Incorporate additional imports for re-render
    let renderOpts' = renderOpts{testImports = Set.union genDeps additionalImports}

    -- Final version of the test suite, with additional imports
    let modDoc' = ruleModuleDoc renderOpts' rules
    contents <- outputString modDoc'
    return $ "-- Generated for rules in file " ++ srcFile ++ "\n" ++ contents

generateFile :: GenerateOptions -> IO ()
generateFile opts = do
    modContents <- getModContents opts
    writeFile (genModFile opts) modContents

demoGenOpts :: GenerateOptions
demoGenOpts = GenerateOptions
  "demo-domain/src/DemoDomain.hs"
  "DemoTest.Generated.DemoDomain"
  (Set.fromList ["DemoDomain"])
  "demo-test/test/DemoTest/Generated/DemoDomain.hs"

getGenerateOptions :: FilePath -> Int -> PackageDescription -> GenerateOptions
getGenerateOptions path num desc =
  GenerateOptions
    path
    ("Rulecheck.Generated.Test" ++ show num)
    (Set.union (importsForPackage desc) defaultImports)
    (testGenDir desc ++ "/Test" ++ show num ++ ".hs")
  where
    defaultImports :: Set String
    defaultImports = Set.fromList ["GHC.Base", "GHC.Float", "GHC.Types", "GHC.Word"]


assertFileExists :: FilePath -> IO ()
assertFileExists path = do
      exists <- doesFileExist path
      unless exists $ error ("File " ++ path ++ " does not exist")

shouldSkip :: FilePath -> Bool
shouldSkip path = any (`isInfixOf` path) filesToSkip


copyTemplateFile :: PackageDescription -> FilePath -> IO ()
copyTemplateFile desc path = do
    pDir <- packageDir desc
    yamlTemplate <- readFile (testTemplateFile path)
    let contents =
          replace "{testname}" (name desc ++ "-test") $
          replace "{packagename}" (name desc) $
          replace "{packagedir}"pDir yamlTemplate
    writeFile (testBaseDir desc ++ "/" ++ path) contents

setupTestDirectory :: PackageDescription -> IO ()
setupTestDirectory pkg =
  do
    dirExists <- doesDirectoryExist (testBaseDir pkg)
    when dirExists $ removeDirectoryRecursive (testBaseDir pkg)
    createDirectoryIfMissing True (testGenDir pkg) -- This will create all intermediate dirs
    copyFile (testTemplateFile "Main.hs") (testSrcDir pkg ++ "/Main.hs")
    copyTemplateFile pkg "package.yaml"
    copyTemplateFile pkg "stack.yaml"
    instancesFilename <- arbitraryInstanceFile pkg
    copyFile instancesFilename (testGenDir pkg ++ "/ArbitraryInstances.hs")


processPackage :: String -> PackageDescription -> IO ()
processPackage prefix pkg = do
  putStrLn $ "Processing " ++ name pkg
  setupTestDirectory pkg
  mapM_ go (zip (map (packageFilePath prefix pkg) $ files pkg) [1..])
  where
    go :: (FilePath, Int) -> IO ()
    go (path, _) | shouldSkip path = putStrLn $ "Skipping file at path" ++ path
    go (path, n) = do
      assertFileExists path
      putStrLn $ "Processing file at " ++ path
      generateFile (getGenerateOptions path n pkg)

getPackagesToProcess :: [String] -> IO [PackageDescription]
getPackagesToProcess packages = do
  jsonResult <- eitherDecodeFileStrict packageDescriptionsFile
  return $ case jsonResult of
    Left err           -> error err
    Right descriptions -> go packages
      where
        go [] = filter (not . skip) $ case startFromPackage of
          Just p  -> dropWhile ((p /=) . name) descriptions
          Nothing -> descriptions
        go _  = filter ((`elem` packages) . name) descriptions

        skip package = name package `elem` packagesToSkip

rulecheck :: [String] -> IO ()
rulecheck args = do
  dir <- haskellPackagesDir
  toProcess <- getPackagesToProcess args
  mapM_ (processPackage dir) toProcess
  putStrLn "Done"

loadFileLines :: FilePath -> IO [Line]
loadFileLines fp = do
  contents <- TIO.readFile fp
  return $ concatMap go $ T.lines contents
  where
    go :: T.Text -> [Line]
    go t | T.null t = []
    go t | Right e <- parseLine fp t = [e]
    go t = trace ("Could not parse line " ++ T.unpack t) []


searchWithLines :: String -> [Line] -> IO ()
searchWithLines typName lines = do
  ds <- rethrow (mkLineDecls lines)
  tsNamed <- rethrow (parseType (T.pack (typName)))
  ts <- rethrow (namelessType tsNamed)
  maxSearchDepth <- fmap read (getEnvDefault "RC_MAX_SEARCH_DEPTH" "10")
  numResults <- fmap read (getEnvDefault "RC_NUM_RESULTS" "5")
  let useSkolem = UseSkolemNo
  let config = SearchConfig ds ts maxSearchDepth useSkolem
  case runSearchN config numResults of
    Left err      -> error (show err)
    Right results -> do
      printf "%d terms found for type %s\n" (length results) typName
      mapM_ showResult results
      where
        showResult result = do
          printf "Term: %s\nType:%s\n" (printTerm (renamedTm)) (docToText ty)
          where
            mkVarName (TmUniq i) = "x" ++ show i
            renamedTm            = fromRight undefined $ renameTerm mkVarName (foundTm result)
            ty                   = pretty $ unTyFoundScheme $ foundTy result

            simplify :: (Ord a) => Map a (Tm a a) -> Tm a a -> Tm a a
            simplify letBinds (TmFree x)  =
              case M.lookup x letBinds of
                Nothing -> TmFree x
                Just tm -> simplify letBinds tm
            simplify letBinds (TmApp f x) = TmApp (simplify letBinds f) (simplify letBinds x)
            simplify letBinds (TmLam x b) | simplify letBinds b == TmFree x = TmKnown (TmName "id")
            simplify letBinds (TmLam x b) = TmLam x (simplify letBinds b)
            simplify letBinds (TmLet ident tm body) =
              simplify (M.insert ident tm letBinds) body
            simplify letBinds (TmCase tm cases) =
              TmCase (simplify letBinds tm) (fmap (simplifyPat letBinds) cases)
            simplify _ other = other

            simplifyPat letBinds (PatPair pat bod) = PatPair pat (simplify letBinds bod)




searchterm :: [String] -> IO ()
searchterm [filename, typName] = do
  baseLines <- loadFileLines "searchterm/prelude.txt"
  pkgLines  <- loadFileLines filename
  searchWithLines typName $ baseLines ++ pkgLines
searchterm [typName] = do
  baseLines <- loadFileLines "searchterm/prelude.txt"
  searchWithLines typName baseLines
searchterm _ = putStrLn "Usage stack run -- --searchterm DEF_FILE TYP"

main :: IO ()
main = do
  args <- getArgs
  case args of
    ("--searchterm" : stArgs) -> searchterm stArgs
    _ -> rulecheck args
