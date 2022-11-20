{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NamedFieldPuns #-}
module Rulecheck where

import Data.Aeson (eitherDecodeFileStrict)
import Data.List (isInfixOf)
import Data.List.Utils
import Data.Set (Set)
import qualified Data.Set as Set
import Rulecheck.Config
import Rulecheck.Monad (cradleGhcM)
import Rulecheck.Rendering (outputString)
import Rulecheck.Rule
import Rulecheck.RuleExtraction
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
    rules <- getRulesFromFile srcFile
    let modDoc = ruleModuleDoc genModName genDeps rules
    contents <- outputString genDeps modDoc
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

testBaseDir :: PackageDescription -> FilePath
testBaseDir desc = packageTestsPrefix ++ "/" ++ name desc ++ "-test"

testSrcDir :: PackageDescription -> FilePath
testSrcDir desc = testBaseDir desc ++ "/test"

testGenDir :: PackageDescription -> FilePath
testGenDir desc = testSrcDir desc ++ "/RuleCheck/Generated"


getGenerateOptions :: FilePath -> Int -> PackageDescription -> GenerateOptions
getGenerateOptions path num desc =
  GenerateOptions
    path
    ("RuleCheck.Generated.Test" ++ show num)
    (Set.union (importsForPackage desc) defaultImports)
    (testGenDir desc ++ "/Test" ++ show num ++ ".hs")
  where
    defaultImports :: Set String
    defaultImports = Set.fromList ["GHC.Base", "GHC.Float", "GHC.Types", "GHC.Word"]


assertFileExists :: FilePath -> IO ()
assertFileExists path = do
      exists <- doesFileExist path
      if not exists
        then error ("File " ++ path ++ " does not exist")
        else return ()


shouldSkip :: FilePath -> Bool
shouldSkip path = any (`isInfixOf` path) filesToSkip

setupTestDirectory :: PackageDescription -> IO ()
setupTestDirectory pkg =
  do
    dirExists <- doesDirectoryExist (testBaseDir pkg)
    if dirExists
      then removeDirectoryRecursive (testBaseDir pkg)
      else return ()
    createDirectoryIfMissing True (testGenDir pkg) -- This will create all intermediate dirs
    copyFile "test-template/Main.hs" (testSrcDir pkg ++ "/Main.hs")
    yamlTemplate <- readFile "test-template/package.yaml"
    let packageYaml = replace "{testname}" (name pkg ++ "-test") (replace "{packagename}" (name pkg) yamlTemplate)
    writeFile (testBaseDir pkg ++ "/package.yaml") packageYaml


processPackage :: String -> PackageDescription -> IO ()
processPackage prefix pkg = do
  putStrLn $ "Processing " ++ name pkg
  setupTestDirectory pkg
  mapM_ go (zip (map (packageFilePath prefix pkg) $ files pkg) [1..])
  where
    go :: (FilePath, Int) -> IO ()
    go (path, _) | shouldSkip path = putStrLn $ "Skipping file at path" ++ path
    go (path, n) | otherwise = do
      assertFileExists path
      putStrLn $ "Processing file at " ++ path
      generateFile (getGenerateOptions path n pkg)

startFromPackage :: Maybe String
startFromPackage = Nothing

getPackagesToProcess :: IO [PackageDescription]
getPackagesToProcess = do
  jsonResult <- eitherDecodeFileStrict packageDescriptionsFile
  case jsonResult of
    Left err           -> error err
    Right descriptions ->
      fmap go getArgs
        where
          go [] = filter (not . skip) $ case startFromPackage of
            Just p  -> dropWhile ((p /=) . name) descriptions
            Nothing -> descriptions
          go packages = filter ((`elem` packages) . name) descriptions

          skip package = elem (name package) packagesToSkip

main :: IO ()
main = do
  toProcess <- getPackagesToProcess
  mapM_ (processPackage "/Users/zgrannan/haskell-packages") toProcess
  putStrLn "Done"
