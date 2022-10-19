{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Rulecheck.Synth where

import Control.Exception (Exception)
import Control.Monad (foldM)
import Control.Monad.Except (Except, MonadError (..), runExcept)
import Control.Monad.Reader (MonadReader (..), ReaderT (..))
import Data.Foldable (toList)
import Data.Functor.Foldable (cata)
import Data.Functor.Foldable.TH (makeBaseFunctor)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Sequence (Seq (..))
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
  , declBody :: !(Maybe (Tm Index))
  , declDepth :: !Int
  } deriving stock (Eq, Ord, Show)

data DeclErr =
    DeclErrTy !TyVar
  | DeclErrTm !TmVar
  | DeclErrDupe
  deriving stock (Eq, Ord, Show)

instance Exception DeclErr

decl :: TmName -> Scheme TyVar -> Maybe (Tm TmVar) -> Either DeclErr Decl
decl n s mt = do
  s' <- namelessTy s
  (mt', d) <- case mt of
    Nothing -> pure (Nothing, 0)
    Just t -> do
      t' <- namelessTm t
      let d = tmDepth t'
      pure (Just t', d)
  pure (Decl n s' mt' d)

newtype M r e a = M { unM :: ReaderT r (Except e) a }
  deriving newtype (Functor, Applicative, Monad, MonadReader r, MonadError e)

runM :: M r e a -> r -> Either e a
runM m r = runExcept (runReaderT (unM m) r)

namelessTm :: Tm TmVar -> Either DeclErr (Tm Index)
namelessTm = flip runM Seq.empty . cata go where
  go = \case
    TmFreeF a -> fmap TmFree (bind a)
    TmKnownF k -> pure (TmKnown k)
    TmAppF x y -> TmApp <$> x <*> y
    TmLamF (Binder tvs x) -> local (<> tvs) x
  bind a = do
    tvs <- ask
    let nvs = Seq.length tvs
    case Seq.findIndexR (== a) tvs of
      Nothing -> throwError (DeclErrTm a)
      Just lvl -> pure (Index (nvs - lvl - 1))

namelessTy :: Scheme TyVar -> Either DeclErr (Scheme Index)
namelessTy (Binder tvs ty) = fmap (Binder tvs) (traverse bind ty) where
  nvs = Seq.length tvs
  bind a =
    case Seq.findIndexR (== a) tvs of
      Nothing -> Left (DeclErrTy a)
      Just lvl -> Right (Index (nvs - lvl - 1))

decls :: [(TmName, Scheme TyVar, Maybe (Tm TmVar))] -> Either (TmName, DeclErr) (Map TmName Decl)
decls = foldM go Map.empty where
  go m (n, s, mt) =
    case decl n s mt of
      Left e -> Left (n, e)
      Right d ->
        case Map.lookup n m of
          Just _ -> Left (n, DeclErrDupe)
          Nothing -> Right (Map.insert n d m)

newtype Uniq = Uniq { unUniq :: Int }
  deriving stock (Show)
  deriving newtype (Eq, Ord, Enum, Num)

data UnifyVar x =
    UnifyVarMeta !Uniq
  | UnifyVarGround !(TyF x Uniq)
  deriving stock (Eq, Ord, Show)

exampleDecls :: Either (TmName, DeclErr) (Map TmName Decl)
exampleDecls = res where
  tyInt = TyCon "Int" Empty
  tyIntFun2 = TyFun tyInt (TyFun tyInt tyInt)
  tmA = TmFree "a"
  tmB = TmFree "b"
  res = decls
    [ ("myPlus", Binder Empty tyIntFun2, Just (TmLam (Binder (Seq.fromList ["a", "b"]) (TmApp (TmApp (TmKnown "+") tmA) tmB))))
    ]
