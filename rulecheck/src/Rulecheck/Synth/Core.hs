{-# LANGUAGE TemplateHaskell #-}

module Rulecheck.Synth.Core
  ( Index (..)
  , TyVar (..)
  , TmVar (..)
  , TyName (..)
  , TmName (..)
  , Scheme (..)
  , Ty (..)
  , Tm (..)
  , TyF (..)
  , TmF (..)
  , bitraverseTyF
  ) where

import Data.Functor.Foldable.TH (makeBaseFunctor)
import Data.Sequence (Seq)
import Data.String (IsString)
import Data.Text (Text)

-- | de Bruijn index
newtype Index = Index { unIndex :: Int }
  deriving stock (Show)
  deriving newtype (Eq, Ord, Num)

-- | Type variable
newtype TyVar = TyVar { unTyVar :: Text }
  deriving stock (Show)
  deriving newtype (Eq, Ord, IsString)

-- | Term variable
newtype TmVar = TmVar { unTmVar :: Text }
  deriving stock (Show)
  deriving newtype (Eq, Ord, IsString)

-- | Known type name
newtype TyName = TyName { unTyName :: Text }
  deriving stock (Show)
  deriving newtype (Eq, Ord, IsString)

-- | Known term name
newtype TmName = TmName { unTmName :: Text }
  deriving stock (Show)
  deriving newtype (Eq, Ord, IsString)

-- | Type scheme
data Scheme a = Scheme
  { schemeBinders :: !(Seq TyVar)
  , schemeBody :: !(Ty a)
  } deriving stock (Eq, Ord, Show)

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
