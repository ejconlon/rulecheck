{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Core definitions for types and terms
module Rulecheck.Interface.Core
  ( Index (..)
  , TyVar (..)
  , TmVar (..)
  , TyName (..)
  , TmName (..)
  , ClsName (..)
  , ModName (..)
  , Cls (..)
  , Inst (..)
  , Scheme (..)
  , Ty (..)
  , Tm (..)
  , TyF (..)
  , TmF (..)
  , bitraverseTyF
  ) where

import Data.Foldable (toList)
import Data.Functor.Foldable (project)
import Data.Functor.Foldable.TH (makeBaseFunctor)
import Data.Sequence (Seq)
import Data.String (IsString)
import Data.Text (Text)
import Prettyprinter (Pretty (..), (<+>))
import qualified Prettyprinter as P
import Rulecheck.Interface.ParenPretty (ParenPretty (..), parenAtom, parenDoc, parenList, parenToDoc)

-- | de Bruijn index
newtype Index = Index { unIndex :: Int }
  deriving stock (Show)
  deriving newtype (Eq, Ord, Num)

-- | Type variable
newtype TyVar = TyVar { unTyVar :: Text }
  deriving stock (Show)
  deriving newtype (Eq, Ord, IsString, Pretty)

-- | Term variable
newtype TmVar = TmVar { unTmVar :: Text }
  deriving stock (Show)
  deriving newtype (Eq, Ord, IsString, Pretty)

-- | Known type name
newtype TyName = TyName { unTyName :: Text }
  deriving stock (Show)
  deriving newtype (Eq, Ord, IsString, Pretty)

-- | Known term name
newtype TmName = TmName { unTmName :: Text }
  deriving stock (Show)
  deriving newtype (Eq, Ord, IsString, Pretty)

-- | Known class name
newtype ClsName = ClsName { unClsName :: Text }
  deriving stock (Show)
  deriving newtype (Eq, Ord, IsString, Pretty)

-- | Module name
newtype ModName = ModName { unModName :: Text }
  deriving stock (Show)
  deriving newtype (Eq, Ord, IsString, Pretty)

-- | Type with a hole for variables (can later be filled in with indices)
data Ty a =
    TyFree !a
  | TyCon !TyName !(Seq (Ty a))
  | TyFun (Ty a) (Ty a)
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)

-- | Term with a hole for variables (can later be filled in with indices)
data Tm b a =
    TmFree !a
  | TmKnown !TmName
  | TmApp (Tm b a) (Tm b a)
  | TmLam !b (Tm b a)
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)

-- The TH here punches a hole in the recursive parts too, so we get simple folds for types and terms

makeBaseFunctor ''Ty
deriving instance (Eq a, Eq r) => Eq (TyF a r)
deriving instance (Ord a, Ord r) => Ord (TyF a r)
deriving instance (Show a, Show r) => Show (TyF a r)

makeBaseFunctor ''Tm
deriving instance (Eq a, Eq b, Eq r) => Eq (TmF a b r)
deriving instance (Ord a, Ord b, Ord r) => Ord (TmF a b r)
deriving instance (Show a, Show b, Show r) => Show (TmF a b r)

-- saves me from deriving bitraverse?
bitraverseTyF :: Applicative m => (w -> m v) -> TyF w w -> m (TyF v v)
bitraverseTyF f = \case
  TyFreeF w -> fmap TyFreeF (f w)
  TyConF tn ws -> fmap (TyConF tn) (traverse f ws)
  TyFunF wl wr -> TyFunF <$> f wl <*> f wr

instance (Pretty a, ParenPretty r) => ParenPretty (TyF a r) where
  parenPretty s = res where
    res = \case
      TyFreeF w -> parenAtom w
      TyConF tn ws -> parenList isSubCon (parenAtom tn : fmap (parenPretty (("con", -1): s)) (toList ws))
      TyFunF wl wr -> parenList isLhsFun [parenPretty (("fun", 0):s) wl, "->", parenPretty (("fun", 1):s) wr]
    isSubCon = case s of
      ("con", _) : _ -> True
      _ -> False
    isLhsFun = case s of
      ("fun", 0) : _ -> True
      _ -> False

instance Pretty a => ParenPretty (Ty a) where
  parenPretty p = parenPretty p . project

instance (Pretty b, Pretty a, ParenPretty r) => ParenPretty (TmF b a r) where
  parenPretty s = \case
    TmFreeF a -> parenAtom a
    TmKnownF n -> parenAtom n
    TmAppF wl wr -> parenList True [parenPretty s wl, parenPretty s wr]
    TmLamF b w -> parenList True [parenDoc ("\\" <> pretty b), "->", parenPretty s w]

instance (Pretty b, Pretty a) => ParenPretty (Tm b a) where
  parenPretty p = parenPretty p . project

-- | Class decl
data Cls = Cls
  { clsName :: !ClsName
  , clsVars :: !(Seq TyVar)
  } deriving stock (Eq, Ord, Show)

instance ParenPretty Cls where
  parenPretty _ (Cls cn vs) = parenList False (parenAtom cn : fmap parenAtom (toList vs))

-- | Instance/Constraint decl
data Inst a = Inst
  { instName :: !ClsName
  , instVars :: !(Seq (Ty a))
  } deriving stock (Eq, Ord, Show)

instance Pretty a => ParenPretty (Inst a) where
  parenPretty s (Inst cn tys) = parenList False (parenAtom cn : fmap (parenPretty (("con", -1):s)) (toList tys))

-- | Type scheme
data Scheme a = Scheme
  { schemeBinders :: !(Seq TyVar)
  , schemeConstraints :: !(Seq (Inst a))
  , schemeBody :: !(Ty a)
  } deriving stock (Eq, Ord, Show)

instance Pretty a => Pretty (Scheme a) where
  pretty (Scheme _tvs pars ty) = fullDoc where
    endDoc = parenToDoc ty
    fullDoc = case toList pars of
      [] -> endDoc
      [p] -> parenToDoc p <+> "=>" <+> endDoc
      ps -> "(" <> P.hsep (P.punctuate "," (fmap parenToDoc ps)) <> ")" <+> "=>" <+> endDoc
