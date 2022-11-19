{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Core definitions for types and terms
module Searchterm.Interface.Core
  ( Index (..)
  , TyVar (..)
  , TmVar (..)
  , TyName (..)
  , TmName (..)
  , ClsName (..)
  , ModName (..)
  , RuleName (..)
  , Ty (..)
  , ConPat (..)
  , Pat (..)
  , PatPair (..)
  , Tm (..)
  , TyF (..)
  , TmF (..)
  , bitraverseTyF
  , Forall (..)
  , Inst (..)
  , Cls (..)
  , Strained (..)
  , strainedVars
  , ClsScheme (..)
  , InstScheme (..)
  , instSchemeBody
  , TyScheme (..)
  , tySchemeBody
  , Rw (..)
  , RwScheme (..)
  , Rule (..)
  , Partial (..)
  , partialToTy
  , tyToPartials
  , unrollTy
  , explodeTy
) where

import Control.Monad (join)
import Data.Foldable (toList)
import Data.Functor.Foldable (project)
import Data.Functor.Foldable.TH (makeBaseFunctor)
import Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq
import Data.String (IsString)
import Data.Text (Text)
import Prettyprinter (Pretty (..), (<+>))
import qualified Prettyprinter as P
import Searchterm.Interface.ParenPretty (ParenPretty (..), parenAtom, parenDoc, parenList, parenPrettyToDoc, parenToDoc)
import Data.List (intercalate)

-- | de Bruijn index
newtype Index = Index { unIndex :: Int }
  deriving stock (Show)
  deriving newtype (Eq, Ord, Num)

instance Pretty Index where
  pretty (Index i) = "?idx@" <> pretty i

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

-- | Rule name
newtype RuleName = RuleName { unRuleName :: Text }
  deriving stock (Show)
  deriving newtype (Eq, Ord, IsString, Pretty)

-- | Type with a hole for variables (can later be filled in with indices)
data Ty a =
    TyFree !a
  | TyCon !TyName !(Seq (Ty a))
  | TyFun (Ty a) (Ty a)
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)

-- | Constructor pattern for term case statements
data ConPat b = ConPat
  { conPatName :: !TmName
  , conPatVars :: !(Seq b)
  } deriving stock (Eq, Ord, Show)

-- | A pattern - for now we only generate constructor patterns
-- but this could be enriched with literals, wildcards, etc
newtype Pat b = Pat { patCon :: ConPat b }
  deriving stock (Show)
  deriving newtype (Eq, Ord)

-- | A "pattern pair" - LHS and RHS of a case match
data PatPair b tm = PatPair
  { ppPat :: !(Pat b)
  , ppBody :: !tm
  } deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)

-- | Term with a hole for variables (can later be filled in with indices)
data Tm b a =
    TmFree !a
  | TmKnown !TmName
  | TmApp (Tm b a) (Tm b a)
  | TmLam !b (Tm b a)
  | TmLet !b (Tm b a) (Tm b a)
  | TmCase (Tm b a) !(Seq (PatPair b (Tm b a)))
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
      TyConF tn ws -> parenList isSubCon (parenAtom tn : fmap (parenPretty (Just "app":s)) (toList ws))
      TyFunF wl wr -> parenList isLhsFun [parenPretty (Just "funl":s) wl, "->", parenPretty (Just "funr":s) wr]
    isSubCon = case s of
      Just "app" : _ -> True
      _ -> False
    isLhsFun = case s of
      Just "funl" : _ -> True
      _ -> False

instance (Pretty a, ParenPretty r) => Pretty (TyF a r) where
  pretty = parenPrettyToDoc

instance Pretty a => ParenPretty (Ty a) where
  parenPretty p = parenPretty p . project

instance Pretty a => Pretty (Ty a) where
  pretty = parenPrettyToDoc

instance Pretty b => Pretty (Pat b) where
  pretty (Pat (ConPat cn bs)) = P.hsep (pretty cn: fmap pretty (toList bs))

instance (Pretty b, Pretty a, ParenPretty r) => ParenPretty (TmF b a r) where
  parenPretty s = \case
    TmFreeF a -> parenAtom a
    TmKnownF n -> parenAtom n
    TmAppF wl wr -> parenList True [parenPretty (Nothing:s) wl, parenPretty (Nothing:s) wr]
    TmLamF b w -> parenList True [parenDoc ("\\" <> pretty b), "->", parenPretty (Nothing:s) w]
    TmLetF b arg body -> parenList True
      ["let", parenAtom b, "=", parenPretty (Nothing:s) arg, "in", parenPretty (Nothing:s) body]
    TmCaseF scrut pairs ->
      let start = ["case", parenPretty (Nothing:s) scrut, "of", "{"]
          mid = intercalate [";"] [[parenDoc (pretty pat), "->", parenPretty (Nothing:s) body] | PatPair pat body <- toList pairs]
          end = ["}"]
          parts = start ++ mid ++ end
      in parenList True parts

instance (Pretty b, Pretty a, ParenPretty r) => Pretty (TmF b a r) where
  pretty = parenPrettyToDoc

instance (Pretty b, Pretty a) => ParenPretty (Tm b a) where
  parenPretty p = parenPretty p . project

instance (Pretty b, Pretty a) => Pretty (Tm b a) where
  pretty = parenPrettyToDoc

data Forall b a = Forall
  { faBinders :: !(Seq b)
  , faBody :: a
  } deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)

instance (Pretty b, Pretty a) => Pretty (Forall b a) where
  pretty (Forall binders body) = startDoc where
    faDoc = ["forall " <> P.hsep (fmap pretty (toList binders)) <> "." | not (Seq.null binders)]
    startDoc = P.hsep (join [faDoc, [pretty body]])

-- | Instance/Constraint decl (The same datatype is used for both)
data Inst a = Inst
  { instName :: !ClsName
  , instVars :: !(Seq (Ty a))
  } deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)

instance Pretty a => Pretty (Inst a) where
  pretty (Inst cn tys) = parenToDoc (parenList False (parenAtom cn : fmap (parenPretty [Just "app"]) (toList tys)))

-- | Class decl
data Cls a = Cls
  { clsName :: !ClsName
  , clsVars :: !(Seq a)
  } deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)

instance Pretty a => Pretty (Cls a) where
  pretty (Cls cn vs) = P.hsep (pretty cn : fmap pretty (toList vs))

-- | Something conSTRAINed by typeclasses.
-- "Strain" is the best of some bad naming options. "Con" is constructor, etc...
data Strained b a = Strained
  { strainedBy :: !(Seq (Inst b))
  , strainedIn :: !a
  } deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)

instance (Pretty b, Pretty a) => Pretty (Strained b a) where
  pretty (Strained x y) = startDoc where
    endDoc = pretty y
    startDoc = case toList x of
      [] -> endDoc
      [p] -> pretty p <+> "=>" <+> endDoc
      ps -> "(" <> P.hsep (P.punctuate "," (fmap pretty ps)) <> ")" <+> "=>" <+> endDoc

strainedVars :: Foldable f => Strained b (f b) -> [b]
strainedVars (Strained x y) = (toList x >>= toList) ++ toList y

newtype ClsScheme a = ClsScheme { unClsScheme :: Forall TyVar (Strained a (Cls a)) }
  deriving stock (Show)
  deriving newtype (Eq, Ord, Pretty)

newtype InstScheme a = InstScheme { unInstScheme :: Forall TyVar (Strained a (Inst a)) }
  deriving stock (Show)
  deriving newtype (Eq, Ord, Pretty)

instSchemeBody :: InstScheme a -> Inst a
instSchemeBody = strainedIn . faBody . unInstScheme

newtype TyScheme a = TyScheme { unTyScheme :: Forall TyVar (Strained a (Ty a)) }
  deriving stock (Show)
  deriving newtype (Eq, Ord, Pretty)

tySchemeBody :: TyScheme a -> Ty a
tySchemeBody = strainedIn . faBody . unTyScheme

data Rw tmf = Rw
  { rwLhs :: !(Tm TmVar tmf)
  , rwRhs :: !(Tm TmVar tmf)
  } deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)

instance Pretty tmf => Pretty (Rw tmf) where
  pretty (Rw lhs rhs) = P.hsep [pretty lhs, "=", pretty rhs]

newtype RwScheme tmf = RwScheme { unRwScheme :: Forall TmVar (Rw tmf) }
  deriving stock (Show)
  deriving newtype (Eq, Ord, Pretty)

data Rule tyf tmf = Rule
  { ruleName :: !Text
  , ruleRw :: !(RwScheme tmf)
  , ruleTy :: !(TyScheme tyf)
  } deriving stock (Eq, Ord, Show)

instance (Pretty tyf, Pretty tmf) => Pretty (Rule tyf tmf) where
  pretty (Rule n rw ty) = P.hsep ["\"", pretty n, "\"", pretty rw, "::", pretty ty]

-- | The type of a partial function application
data Partial a = Partial
  { partialArgs :: !(Seq (Ty a))
  , partialRet :: !(Ty a)
  } deriving stock (Eq, Ord, Show)

partialToTy :: Partial a -> Ty a
partialToTy (Partial args0 ret) = go args0 where
  go = \case
    Empty -> ret
    x :<| xs -> TyFun x (go xs)

tyToPartials :: Ty a -> Seq (Partial a)
tyToPartials = onOuter where
  onOuter = \case
    TyFun x y -> onInner (Seq.singleton x) y
    _ -> Empty
  onInner as t = Partial as t :<| case t of
    TyFun x y -> onInner (as :|> x) y
    _ -> Empty

-- | "Unroll" argument types off the front of a function type
unrollTy :: Ty a -> (Ty a, Seq (Ty a))
unrollTy = go Empty where
  go !args = \case
    TyFun arg body -> go (args :|> arg) body
    nonFun -> (nonFun, args)

-- | "Explode" a type scheme into result scheme and function arguments.
-- NOTE: Forbids any constraints in the scheme.
explodeTy :: TyScheme a -> Maybe (TyScheme a, Seq (Ty a))
explodeTy (TyScheme (Forall tvs (Strained is tyStart))) =
  if Seq.null is
    then
      let (tyEnd, tyArgs) = unrollTy tyStart
      in Just (TyScheme (Forall tvs (Strained Empty tyEnd)), tyArgs)
    else Nothing

instance Pretty a => Pretty (Partial a) where
  pretty = pretty . partialToTy
