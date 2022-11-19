{-# LANGUAGE NamedFieldPuns #-}
module Rulecheck where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON, eitherDecodeFileStrict)
import Data.Char
import Data.List (isSuffixOf)
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Generics
import GHC.Utils.Outputable (Outputable (..), SDoc, parens, pprWithCommas, text, ($+$), (<+>))
import Rulecheck.Monad (cradleGhcM)
import Rulecheck.Rendering (outputString)
import Rulecheck.Rule
import Rulecheck.RuleExtraction
import System.IO (hPutStrLn, stderr)
import System.Directory

logFile :: String
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

demoGenOpts :: GenerateOptions
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

filesToSkip :: [String]
filesToSkip =
  [ -- Only rules are for lower version, ignore for now
    "aeson-2.0.3.0/src/Data/Aeson/Internal/ByteString.hs"
  , "Agda-2.6.2.2/src/full/Agda/TypeChecking/Monad/Base.hs" -- Precedence parsing error
  , "Agda-2.6.2.2/src/data/MAlonzo/src/MAlonzo/RTE.hs" -- Cabal error (unknown target)
  , "blaze-builder-0.4.2.2/benchmarks/LazyByteString.hs" -- Cabal error (unknown target)
     -- Only rules are for lower version, ignore for now
  , "base-compat-0.11.2/src/Data/List/Compat.hs"
  , "boring-0.2/src/Data/Boring.hs" -- This rule is in a comment
    -- Only rules are for lower version
  , "bytestring-builder-0.10.8.2.0/src/Data/ByteString/Short/Internal.hs"
    -- Could not load module (hidden package)
  , "bytestring-builder-0.10.8.2.0/src/Data/ByteString/Builder/Prim.hs"
  , "clash-lib-1.6.4/src/Clash/Util/Interpolate.hs" -- Could not resolve dependencies
  , "clash-prelude-1.6.4/src/Clash/Sized/Vector.hs" -- Lots of type errors
  ]

shouldSkip :: FilePath -> Bool
shouldSkip path = any (`isSuffixOf` path) filesToSkip

assert :: Monad m => Bool -> String -> m ()
assert True _    = return ()
assert False msg = error msg

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
    go path | shouldSkip path = putStrLn $ "Skipping file at path" ++ path
    go path | otherwise = do
      ensureFileExists path
      putStrLn $ "Processing file at " ++ path
      rulesStr <- cradleGhcM path $ do
        rules <- getRulesFromFile path
        if length rules == 0
          then liftIO $ appendFile logFile $ "No rules detected in file " ++ path
          else return ()
        docs  <- mapM ruleDoc rules
        outputString docs
      putStrLn rulesStr

startFromPackage :: Maybe String
startFromPackage = Just "clash-prelude"

packagesToSkip :: [String]
packagesToSkip = ["clash-prelude"]

main :: IO ()
main = do
  jsonResult :: Either String [PackageDescription] <- eitherDecodeFileStrict "../auto/foo"
  case jsonResult of
    Left error         -> putStrLn error
    Right descriptions ->
      mapM_ (processPackage "/Users/zgrannan/haskell-packages") toProcess
      where
        skip package = elem (name package) packagesToSkip
        toProcess = filter (not . skip) $ case startFromPackage of
            Just p  -> dropWhile ((p /=) . name) descriptions
            Nothing -> descriptions
  -- Placeholder - just generate our fixed target for now
  -- generateFile demoGenOpts
  putStrLn "Done"
