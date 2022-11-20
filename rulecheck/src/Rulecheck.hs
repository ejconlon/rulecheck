{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NamedFieldPuns #-}
module Rulecheck where

import Data.Aeson (FromJSON, eitherDecodeFileStrict)
import Data.List (isInfixOf)
import Data.List.Utils
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Generics
import Rulecheck.Monad (cradleGhcM)
import Rulecheck.Rendering (outputString)
import Rulecheck.Rule
import Rulecheck.RuleExtraction
import System.Directory
import System.Environment (getArgs)

packageDescriptionsFile :: FilePath
packageDescriptionsFile = "./packageDescriptions.json"

packageTestsPrefix :: String
packageTestsPrefix = "package-tests"

logFile :: FilePath
logFile = "log.txt"

data PackageDescription =
  PackageDescription
    {  files   :: [String]
    ,  name    :: String
    ,  version :: String
    } deriving (Generic, Show)

packageFilePath :: String -> PackageDescription -> String -> FilePath
packageFilePath prefix pkg file =
  prefix ++  "/" ++ name pkg ++ "-" ++ version pkg ++ "/" ++ file

instance FromJSON PackageDescription

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
    outputString genDeps modDoc

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


importsForPackage :: PackageDescription -> Set String
importsForPackage pkg | name pkg == "Color"
  = Set.fromList [ "Graphics.Color.Model.Internal"
                 , "Graphics.Color.Adaptation.Internal"
                 , "Graphics.Color.Space.Internal"
                 , "Graphics.Color.Algebra.Elevator"
                 ]
importsForPackage _ = Set.fromList []

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

filesToSkip :: [String]
filesToSkip =
  [ -- Only rules are for lower version, ignore for now
    "aeson-2.0.3.0/src/Data/Aeson/Internal/ByteString.hs"
  , "Agda-2.6.2.2/src/full/Agda/TypeChecking/Monad/Base.hs" -- Precedence parsing error
  , "Agda-2.6.2.2/src/data/MAlonzo/src/MAlonzo/RTE.hs" -- ???
  , "blaze-builder-0.4.2.2/benchmarks/LazyByteString.hs" -- Not a source file
     -- Only rules are for lower version, ignore for now
  , "base-compat-0.11.2/src/Data/List/Compat.hs"
  , "boring-0.2/src/Data/Boring.hs" -- This rule is in a comment
    -- Only rules are for lower version
  , "bytestring-builder-0.10.8.2.0/src/Data/ByteString/Short/Internal.hs"
    -- Could not load module (hidden package)
  , "bytestring-builder-0.10.8.2.0/src/Data/ByteString/Builder/Prim.hs"
  , "clash-lib-1.6.4/src/Clash/Util/Interpolate.hs" -- Could not resolve dependencies
  , "clash-prelude-1.6.4/src/Clash/Sized/Vector.hs" -- Lots of type errors
  , "fourmolu-0.4.0.0/data/examples/"               -- These aren't source files
  , "haskell-src-exts-1.23.1/tests/examples/"       -- These aren't source files
  , "hasktags-0.72.0/testcases/"                    -- These aren't source files
  , "matrix-static-0.3/src/Data/Matrix/Static.hs"   -- This causes a segfault, somehow
  , "ormolu-0.3.1.0/data/examples/"                 -- These aren't source files
  , "polysemy-1.6.0.0/src/Polysemy/State.hs"        -- Also segfaults, somehow
  , "streamly-0.8.1.1/src/inline.hs"                -- Doesn't seem to be a source file
  , "streamly-0.8.1.1/src/Streamly/Internal/Data/Array/PrimInclude.hs" -- Not a target
  , "streamly-0.8.1.1/src/Streamly/Internal/Data/Stream/PreludeCommon.hs" -- Not a target
  , "vector-0.12.3.1/internal/GenUnboxTuple.hs" -- Not a target
  ]

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
startFromPackage = Just "primitive"

packagesToSkip :: [String]
packagesToSkip =
  [ "clash-prelude"    -- Appears to have compilation issues
  , "doctest"          -- Cabal unknown target
  , "doctest-parallel" -- Cabal unknown target
  , "flat"             -- cannot satisfy -package dlist-0.8.0.7
  , "foundation"       -- Cabal cannot find basement package
  , "ghc-exactprint"   -- Cannot find various modules
  , "ghc-lib"          -- Issue with native headers (i.e missing ffitarget_x86.h)
  , "ghc-lib-parser"   -- Issue with native headers (i.e missing ffitarget_x86.h)
  , "leveldb-haskell"  -- Requires local installation of leveldb
  , "mysql-simple"     -- Cannot satisfy package -package mysql-0.2.1
  , "Rattus"           -- GHCi cannot find symbol during dynamic linking
  , "primitive"        -- Strange error (duplicate definition of symbol in runtime linking)
  , "sized"            -- Type errors
  , "type-of-html"     -- Strange error (duplicate definition of symbol in runtime linking)
  ]

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
