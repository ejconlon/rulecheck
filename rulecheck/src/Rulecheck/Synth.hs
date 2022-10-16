{-# LANGUAGE TemplateHaskell #-}

module Rulecheck.Synth where

import Control.Exception (Exception)
import Data.Foldable (toList)
import Data.Functor.Foldable (cata)
import Data.Functor.Foldable.TH (makeBaseFunctor)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.String (IsString)
import Data.Text (Text)
import Data.Typeable (Typeable)

-- Program synthesis

-- de Bruijn index
newtype Index = Index { unIndex :: Int }
  deriving stock (Show)
  deriving newtype (Eq, Ord, Num)

-- type variable
newtype TyVar = TyVar { unTyVar :: Text }
  deriving stock (Show)
  deriving newtype (Eq, Ord, IsString)

-- term variable
newtype TmVar = TmVar { unTmVar :: Text }
  deriving stock (Show)
  deriving newtype (Eq, Ord, IsString)

-- type name
newtype TyName = TyName { unTyName :: Text }
  deriving stock (Show)
  deriving newtype (Eq, Ord, IsString)

-- term name
newtype TmName = TmName { unTmName :: Text }
  deriving stock (Show)
  deriving newtype (Eq, Ord, IsString)

data Binder b a = Binder
  { binderVars :: !(Seq b)
  , binderBody :: !a
  } deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)

binder :: Foldable f => f b -> a -> Binder b a
binder = Binder . (Seq.fromList . toList)

type Scheme a = Binder TyVar (Ty a)
type Lambda a = Binder TmVar (Tm a)

data Ty a =
    TyFree !a
  | TyCon !TyName !(Seq (Ty a))
  | TyFun (Ty a) (Ty a)
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)

data Tm a =
    TmFree !a
  | TmKnown !TmName
  | TmApp (Tm a) (Tm a)
  | TmLam !(Lambda a)
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)

makeBaseFunctor ''Ty
deriving instance (Eq a, Eq r) => Eq (TyF a r)
deriving instance (Ord a, Ord r) => Ord (TyF a r)
deriving instance (Show a, Show r) => Show (TyF a r)

makeBaseFunctor ''Tm
deriving instance (Eq a, Eq r) => Eq (TmF a r)
deriving instance (Ord a, Ord r) => Ord (TmF a r)
deriving instance (Show a, Show r) => Show (TmF a r)

data AlignErr x =
    AlignErrFree !x !x
  | AlignErrConHead !TyName !TyName
  | AlignErrConArity !TyName !Int !Int
  | AlignErrMismatch
  deriving stock (Eq, Ord, Show)

instance (Show x, Typeable x) => Exception (AlignErr x)

alignTys :: Eq x => TyF x a -> TyF x b -> Either (AlignErr x) (TyF x (a, b))
alignTys one two =
  case (one, two) of
    (TyFreeF x1, TyFreeF x2) ->
      if x1 == x2
        then Right (TyFreeF x1)
        else Left (AlignErrFree x1 x2)
    (TyConF n as, TyConF m bs) ->
      if n == m
        then
          let la = Seq.length as
              lb = Seq.length bs
          in if la == lb
            then Right (TyConF n (Seq.zip as bs))
            else Left (AlignErrConArity n la lb)
        else Left (AlignErrConHead n m)
    (TyFunF q1 r1, TyFunF q2 r2) -> Right (TyFunF (q1, q2) (r1, r2))
    _ -> Left AlignErrMismatch

mightAlign :: TyF x1 a -> TyF x2 b -> Bool
mightAlign one two =
  case (one, two) of
    (TyFreeF _, TyFreeF _) -> True
    (TyConF n as, TyConF m bs) -> n == m && Seq.length as == Seq.length bs
    (TyFunF _ _, TyFunF _ _) -> True
    _ -> False

tmDepth :: Tm y -> Int
tmDepth = cata go where
  go = \case
    TmFreeF _ -> 1
    TmKnownF _ -> 1
    TmAppF x y -> 1 + max x y
    TmLamF (Binder _ b) -> 1 + b

data Decl = Decl
  { declName :: !TmName
  , declScheme :: !(Scheme Index)
  , declBody :: !(Tm Index)
  , declDepth :: !Int
  } deriving stock (Eq, Ord, Show)

data DeclErr =
    DeclErrTy !TyVar
  | DeclErrTm !TmVar
  deriving stock (Eq, Ord, Show)

instance Exception DeclErr

decl :: TmName -> Scheme TyVar -> Tm TmVar -> Either DeclErr Decl
decl = error "TODO"

newtype Uniq = Uniq { unUniq :: Int }
  deriving stock (Show)
  deriving newtype (Eq, Ord, Enum, Num)

data UnifyVar x =
    UnifyVarMeta !Uniq
  | UnifyVarGround !(TyF x Uniq)
  deriving stock (Eq, Ord, Show)
