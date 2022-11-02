{-# LANGUAGE OverloadedStrings #-}

-- | These are definitions for the interface files. You will generally be
-- parsing and printing with 'parseLinesIO' and 'printLines'.
module Rulecheck.Interface.Types where

import Control.Monad (join)
import Data.Foldable (toList)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Prettyprinter (Doc, Pretty (..), (<+>))
import qualified Prettyprinter as P
import Rulecheck.Interface.Core (Cls, Inst, ModName, Scheme (..), TmName, TyName, TyVar)
import Rulecheck.Interface.ParenPretty (ParenPretty (..), parenToDoc)

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

data InstLine = InstLine
  { ilInst :: !(Inst TyVar)
  , ilParents :: !(Seq (Inst TyVar))
  } deriving stock (Eq, Show)

constraintsP :: (ParenPretty a, ParenPretty b) => Doc ann -> Seq a -> b -> Doc ann
constraintsP txt pars end = fullDoc where
  endDoc = parenToDoc end
  restDoc = case toList pars of
    [] -> endDoc
    [p] -> parenToDoc p <+> "=>" <+> endDoc
    ps -> "(" <> P.hsep (P.punctuate "," (fmap parenToDoc ps)) <> ")" <+> "=>" <+> endDoc
  fullDoc = txt <+> restDoc

instance Pretty InstLine where
  pretty (InstLine ins pars) = constraintsP "instance" pars ins

data FuncLine = FuncLine
  { flName :: !TmName
  , flForall :: !Bool
  , flType :: !(Scheme TyVar)
  } deriving stock (Eq, Show)

instance Pretty FuncLine where
  pretty (FuncLine tn fa sc) = P.hsep (join [[pretty tn], ["::"], faDoc, [pretty sc]]) where
    binders = schemeBinders sc
    faDoc = ["forall " <> P.hsep (fmap pretty (toList binders)) <> "." | fa && not (Seq.null binders)]

data ClsLine = ClsLine
  { clSelf :: !Cls
  , clParents :: !(Seq (Inst TyVar))
  } deriving stock (Eq, Show)

instance Pretty ClsLine where
  pretty (ClsLine cls pars) = constraintsP "class" pars cls

newtype ModLine = ModLine
  { mlName :: ModName
  } deriving stock (Eq, Show)

instance Pretty ModLine where
  pretty (ModLine mn) = P.hsep ["module", pretty mn]

data Line =
    LineData !DataLine
  | LineCons !ConsLine
  | LineInst !InstLine
  | LineFunc !FuncLine
  | LineCls !ClsLine
  | LineMod !ModLine
  deriving stock (Eq, Show)

instance Pretty Line where
  pretty = \case
    LineData dl -> pretty dl
    LineCons cl -> pretty cl
    LineInst il -> pretty il
    LineFunc fl -> pretty fl
    LineCls cl -> pretty cl
    LineMod ml -> pretty ml
