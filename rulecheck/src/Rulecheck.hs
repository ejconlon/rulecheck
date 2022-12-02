{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NamedFieldPuns #-}
module Rulecheck where

import Control.Monad (when, unless)
import Data.Aeson (eitherDecodeFileStrict)
import Data.List (isInfixOf)
import Data.List.Utils
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as T
import Rulecheck.Config
import Rulecheck.Monad (cradleGhcM)
import Rulecheck.Rendering
import Rulecheck.Rule
import Rulecheck.RuleRendering (TestModuleRenderOpts(..), ruleModuleDoc)
import Rulecheck.RuleExtraction
import Searchterm.Synth.Search
import Searchterm.Util (loadDecls, rethrow, DeclSrc(..))
import Searchterm.Interface.Parser (parseType)
import Searchterm.Interface.Printer
import Searchterm.Interface.Names (namelessType)
import System.Directory
import System.Environment (getArgs)

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

searchterm :: [String] -> IO ()
searchterm [] = putStrLn "Need a type"
searchterm (typName : _) = do
  ds <- loadDecls (DeclSrcFile "testdata/base.txt")
  tsNamed <- rethrow (parseType (T.pack (typName)))
  ts <- rethrow (namelessType tsNamed)
  let maxSearchDepth = 5
  let useSkolem = UseSkolemYes
  let config = SearchConfig ds ts maxSearchDepth useSkolem
  let numResults = 10
  case runSearchN config numResults of
    Left err      -> error (show err)
    Right results -> mapM_ (putStrLn . T.unpack . printTerm . foundTm) results

main :: IO ()
main = do
  args <- getArgs
  case args of
    ("--searchterm" : stArgs) -> searchterm stArgs
    _ -> rulecheck args
