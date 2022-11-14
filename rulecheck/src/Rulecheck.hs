{-# LANGUAGE NamedFieldPuns #-}
module Rulecheck where

import Data.Aeson (FromJSON, eitherDecodeFileStrict)
import Data.Char
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Generics
import GHC.Utils.Outputable (Outputable (..), SDoc, parens, pprWithCommas, text, ($+$), (<+>))
import Rulecheck.Monad (cradleGhcM)
import Rulecheck.Rendering (outputString)
import Rulecheck.Rule
import Rulecheck.RuleExtraction
import System.Directory

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

capitalize (h : t) = toUpper h : t

getOptions :: PackageDescription -> [GenerateOptions]
getOptions desc = map go (files desc) where

  baseModName = capitalize (name desc)
  genModName  = "Rulecheck.Generated." ++ baseModName
  genModFile  = baseModName ++ ".hs"

  go :: FilePath -> GenerateOptions
  go f = GenerateOptions f genModName (Set.fromList undefined) genModFile

getModContents :: GenerateOptions -> IO String
getModContents (GenerateOptions {srcFile, genModName, genDeps, genModFile}) =
  cradleGhcM srcFile $ do
    rules <- getRulesFromFile srcFile
    modDoc <- ruleModuleDoc genModName genDeps rules
    outputString modDoc

generateFile :: GenerateOptions -> IO ()
generateFile opts = do
  modContents <- getModContents opts
  writeFile (genModFile opts) modContents

demoGenOpts = GenerateOptions
  "demo-domain/src/DemoDomain.hs"
  "DemoTest.Generated.DemoDomain"
  (Set.fromList ["DemoDomain"])
  "demo-test/test/DemoTest/Generated/DemoDomain.hs"

ensureFileExists :: FilePath -> IO ()
ensureFileExists path = do
      exists <- doesFileExist path
      if not exists
        then error ("File " ++ path ++ " does not exist")
        else return ()

processPackage :: String -> PackageDescription -> IO ()
processPackage prefix pkg = do
  putStrLn $ "Processing " ++ name pkg
  mapM_ go (map (packageFilePath prefix pkg) $ files pkg)
  where
    ruleDoc rule = do
      l <- ruleSideDoc rule LHS
      r <- ruleSideDoc rule RHS
      return $ l $+$ text "->" $+$ r
    go :: FilePath -> IO ()
    go path = do
      ensureFileExists path
      putStrLn $ "Processing file at " ++ path
      rulesStr <- cradleGhcM path $ do
         rules <- getRulesFromFile path
         docs  <- mapM ruleDoc rules
         outputString docs
      putStrLn rulesStr

main :: IO ()
main = do
  jsonResult :: Either String [PackageDescription] <- eitherDecodeFileStrict "../auto/foo"
  case jsonResult of
    Left error         -> putStrLn error
    Right descriptions -> mapM_ (processPackage "/Users/zgrannan/haskell-packages") descriptions
  -- Placeholder - just generate our fixed target for now
  -- generateFile demoGenOpts
  putStrLn "Done"
