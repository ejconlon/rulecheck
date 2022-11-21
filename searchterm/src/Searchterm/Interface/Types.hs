{-# LANGUAGE OverloadedStrings #-}

-- | These are definitions for the interface files. You will generally be
-- parsing and printing with 'parseLinesIO' and 'printLines'.
module Searchterm.Interface.Types where

import Data.Foldable (toList)
import Data.Sequence (Seq)
import Prettyprinter (Pretty (..))
import qualified Prettyprinter as P
import Searchterm.Interface.Core (ClsScheme, InstScheme, ModName, Rule, TmName, TmVar, TyName, TyScheme (..), TyVar, Lit)

data DataLine = DataLine
  { dlName :: !TyName
  , dlVars :: !(Seq TyVar)
  } deriving stock (Eq, Show)

instance Pretty DataLine where
  pretty (DataLine tn tvs) = P.hsep ("data" : pretty tn : fmap pretty (toList tvs))

data ConsLine = ConsLine
  { clName :: !TyName
  , clCons :: !(Seq TmName)
  } deriving stock (Eq, Show)

instance Pretty ConsLine where
  pretty (ConsLine tn cs) = P.hsep ("constructors" : pretty tn : fmap pretty (toList cs))

newtype InstLine = InstLine
  { ilScheme :: InstScheme TyVar
  } deriving stock (Eq, Show)

instance Pretty InstLine where
  pretty (InstLine is) = P.hsep ["instance", pretty is]

data FuncLine = FuncLine
  { flName :: !TmName
  , flType :: !(TyScheme TyVar)
  } deriving stock (Eq, Show)

instance Pretty FuncLine where
  pretty (FuncLine tn sc) = P.hsep [pretty tn, "::", pretty sc]

newtype ClsLine = ClsLine
  { clScheme :: ClsScheme TyVar
  } deriving stock (Eq, Show)

instance Pretty ClsLine where
  pretty (ClsLine cs) = P.hsep ["class", pretty cs]

newtype ModLine = ModLine
  { mlName :: ModName
  } deriving stock (Eq, Show)

instance Pretty ModLine where
  pretty (ModLine mn) = P.hsep ["module", pretty mn]

newtype RuleLine = RuleLine
  { rlRule :: Rule TyVar TmVar
  } deriving stock (Eq, Show)

instance Pretty RuleLine where
  pretty (RuleLine ru) = P.hsep ["rule", pretty ru]

data LitLine = LitLine
  { llName :: !TyName
  , llValues :: !(Seq Lit)
  } deriving stock (Eq, Show)

instance Pretty LitLine where
  pretty (LitLine nm vals) = P.hsep ("literals" : pretty nm : fmap pretty (toList vals))

data Line =
    LineData !DataLine
  | LineCons !ConsLine
  | LineInst !InstLine
  | LineFunc !FuncLine
  | LineCls !ClsLine
  | LineMod !ModLine
  | LineRule !RuleLine
  | LineLit !LitLine
  deriving stock (Eq, Show)

instance Pretty Line where
  pretty = \case
    LineData dl -> pretty dl
    LineCons cl -> pretty cl
    LineInst il -> pretty il
    LineFunc fl -> pretty fl
    LineCls cl -> pretty cl
    LineMod ml -> pretty ml
    LineRule rl -> pretty rl
    LineLit ll -> pretty ll
