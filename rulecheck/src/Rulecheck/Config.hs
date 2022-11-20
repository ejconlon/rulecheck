module Rulecheck.Config where

import Data.Aeson (FromJSON)
import Data.Set as Set
import GHC.Generics

data PackageDescription =
  PackageDescription
    {  files   :: [String]
    ,  name    :: String
    ,  version :: String
    } deriving (Generic, Show)

instance FromJSON PackageDescription

packageDescriptionsFile :: FilePath
packageDescriptionsFile = "./packageDescriptions.json"

packageTestsPrefix :: String
packageTestsPrefix = "package-tests"

logFile :: FilePath
logFile = "log.txt"

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

importsForPackage :: PackageDescription -> Set String
importsForPackage pkg | name pkg == "fast-math" = Set.fromList [ "GHC.Exts", "GHC.Prim" ]
importsForPackage pkg | name pkg == "Color"
  = Set.fromList [ "Graphics.Color.Model.Internal"
                 , "Graphics.Color.Adaptation.Internal"
                 , "Graphics.Color.Space.Internal"
                 , "Graphics.Color.Algebra.Elevator"
                 ]
importsForPackage _ = Set.fromList []
