{-# LANGUAGE NamedFieldPuns #-}
module Rulecheck where

import Data.Aeson (FromJSON, eitherDecodeFileStrict)
import Data.Char
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Generics
import Rulecheck.Monad (cradleGhcM)
import Rulecheck.Rendering (outputString)
import Rulecheck.Rule (ruleFromDecl, ruleModuleDoc)
import Rulecheck.RuleExtraction

data PackageDescription =
  PackageDescription
    {  files   :: [String]
    ,  name    :: String
    ,  version :: String
    } deriving (Generic, Show)

packageFilePath :: PackageDescription -> String -> String -> FilePath
packageFilePath pkg prefix file =
  prefix ++  ""

instance FromJSON PackageDescription

data GenerateOptions =
  GenerateOptions
    { srcFile    :: FilePath
    , genModName :: String
    , genDeps    :: Set String
    , genModFile :: FilePath
    }

capitalize (h : t) = toUpper h : t

getOptions :: PackageDescription -> [GenerateOptions]
getOptions desc = map go (files desc) where

  baseModName = capitalize (name desc)
  genModName  = "Rulecheck.Generated." ++ baseModName
  genModFile  = baseModName ++ ".hs"

  go :: FilePath -> GenerateOptions
  go f = GenerateOptions f genModName (Set.fromList undefined) genModFile

generateFile :: GenerateOptions -> IO ()
generateFile (GenerateOptions {srcFile, genModName, genDeps, genModFile}) = do
  modContents <- cradleGhcM srcFile $ do
    rules <- getRulesFromFile srcFile
    modDoc <- ruleModuleDoc genModName genDeps rules
    outputString modDoc
  -- putStrLn modContents
  writeFile genModFile modContents

demoGenOpts = GenerateOptions
  "demo-domain/src/DemoDomain.hs"
  "DemoTest.Generated.DemoDomain"
  (Set.fromList ["DemoDomain"])
  "demo-test/test/DemoTest/Generated/DemoDomain.hs"

processPackage :: PackageDescription -> IO ()
processPackage pkg = do
  putStrLn $ "Processing " ++ name pkg
  mapM_ putStrLn (files pkg)

main :: IO ()
main = do
  jsonResult :: Either String [PackageDescription] <- eitherDecodeFileStrict "../auto/foo"
  case jsonResult of
    Left error         -> putStrLn error
    Right descriptions -> mapM_ processPackage descriptions
  -- Placeholder - just generate our fixed target for now
  -- generateFile demoGenOpts
  putStrLn "Done"
