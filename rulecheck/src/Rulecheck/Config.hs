module Rulecheck.Config where

import Data.Aeson (FromJSON)
import Data.List (isInfixOf, isSuffixOf)
import Data.Set as Set
import GHC.Data.FastString
import GHC.Generics
import GHC.Types.Basic (RuleName)
import System.Directory

data PackageDescription =
  PackageDescription
    {  files   :: [String]
    ,  name    :: String
    ,  version :: String
    } deriving (Generic, Show)

instance FromJSON PackageDescription

testTemplateFile :: String -> FilePath
testTemplateFile f = testTemplateDir ++ "/" ++ f

arbitraryInstanceFile :: PackageDescription -> IO FilePath
arbitraryInstanceFile desc = do
  useS <- doesFileExist specializedF
  return $ if useS
    then specializedF
    else defaultF
 where
    specializedF = testTemplateFile ("extra-arbitrary-instances/" ++ name desc ++ ".hs")
    defaultF     = testTemplateFile "extra-arbitrary-instances/Default.hs"

testTemplateDir :: FilePath
testTemplateDir = "test-template"

packageDir :: PackageDescription -> FilePath
packageDir desc = haskellPackagesDir ++ "/" ++ name desc ++ "-" ++ version desc

testBaseDir :: PackageDescription -> FilePath
testBaseDir desc = packageTestsPrefix ++ "/" ++ name desc ++ "-test"

testSrcDir :: PackageDescription -> FilePath
testSrcDir desc = testBaseDir desc ++ "/test"

testGenDir :: PackageDescription -> FilePath
testGenDir desc = testSrcDir desc ++ "/Rulecheck/Generated"

startFromPackage :: Maybe String
startFromPackage = Nothing

-- IMPORTANT! This should be a fully-qualified directory
haskellPackagesDir :: FilePath
haskellPackagesDir = "/Users/zgrannan/haskell-packages"

packageDescriptionsFile :: FilePath
packageDescriptionsFile = "./packageDescriptions.json"

packageTestsPrefix :: String
packageTestsPrefix = "package-tests"

logFile :: FilePath
logFile = "log.txt"

-- Override type signatures for a given rule / path combo. This is typically
-- useful if the rule is defined in terms of a class constraint, and you want
-- to test the rule for certain instances
overrideTypeSigs :: FilePath -> RuleName -> Set String
overrideTypeSigs fp rn
  | "ListLike/UTF8.hs" `isSuffixOf` fp
  = case unpackFS rn of
      -- See https://hackage.haskell.org/package/ListLike-4.7.7/docs/Data-ListLike.html#g:4
      -- TODO: Consider more
      "fromListLike/a" -> Set.singleton "[Int] -> [Int]"
      _                -> Set.empty
  | "basement-0.0.15/Basement" `isInfixOf` fp
  = case unpackFS rn of
      "primOffsetRecast W8"   -> Set.singleton "Offset Word8 -> Offset Char"
      "sizeRecast from Word8" -> Set.singleton "CountOf Word8 -> CountOf Char"
      _                       -> Set.empty
overrideTypeSigs _ _ = Set.empty

skipRule :: FilePath -> RuleName -> Bool
skipRule fp rn | "basement-0.0.15/Basement" `isInfixOf` fp
  = case unpackFS rn of
      "String sFromList" -> True -- This rule uses raw pointers, skip for now
      _ -> False
skipRule _ _ = False

filesToSkip :: [String]
filesToSkip =
  [ "aeson-2.0.3.0/src/Data/Aeson/Internal/ByteString.hs" -- Rules apply to a lower version
  , "aeson-2.0.3.0/tests/UnitTests.hs"                    -- We don't care about rules in tests
  , "arithmoi-0.12.0.2/Math/NumberTheory/Moduli/SomeMod.hs" -- Applies on GADT
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

packagesToSkip :: [String]
packagesToSkip =
  [ "base-compat"      -- No applicable rules
  , "clash-prelude"    -- Appears to have compilation issues
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

-- Allows for adding additional imports, if necessary
importsForPackage :: PackageDescription -> Set String
importsForPackage pkg | name pkg == "basement"
                      = Set.fromList [ "Basement.Types.OffsetSize", "Data.Proxy" ]
importsForPackage _   = Set.empty

-- Given a possibly internal module, return a public version of that module with similar exports
-- For example, many types in the private module `Data.Set.Internal` are in `Data.Set`, so the
-- latter can be used instead.
-- This is used as a workaround for limitations in automatic import detection.
internalToPublicMod :: String -> String
internalToPublicMod "Data.Set.Internal" = "Data.Set"
internalToPublicMod s = s
