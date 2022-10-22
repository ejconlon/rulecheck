module Rulecheck.Synth.Decl
  ( Decl (..)
  , DeclErr
  , mkDecl
  , mkDecls
  ) where

import Control.Exception (Exception)
import Control.Monad (foldM)
import Control.Monad.Except (Except, MonadError (..), runExcept)
import Control.Monad.Reader (MonadReader (..), ReaderT (..))
import Data.Functor.Foldable (cata)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq
import Rulecheck.Synth.Core (Index (..), Scheme (..), Tm (..), TmF (..), TmName, TmVar, TyVar)

data Decl = Decl
  { declName :: !TmName
  , declScheme :: !(Scheme Index)
  , declBody :: !(Maybe (Tm TmVar Index))
  } deriving stock (Eq, Ord, Show)

data DeclErr =
    DeclErrTy !TyVar
  | DeclErrTm !TmVar
  | DeclErrDupe
  deriving stock (Eq, Ord, Show)

instance Exception DeclErr

mkDecl :: TmName -> Scheme TyVar -> Maybe (Tm TmVar TmVar) -> Either DeclErr Decl
mkDecl n s mt = do
  s' <- namelessTy s
  mt' <- case mt of
    Nothing -> pure Nothing
    Just t -> fmap Just (namelessTm t)
  pure (Decl n s' mt')

-- helper for the cata
runReaderExceptM :: r -> ReaderT r (Except e) a -> Either e a
runReaderExceptM r m = runExcept (runReaderT m r)

namelessTm :: Tm TmVar TmVar -> Either DeclErr (Tm TmVar Index)
namelessTm = runReaderExceptM Seq.empty . cata go where
  go = \case
    TmFreeF a -> fmap TmFree (bind a)
    TmKnownF k -> pure (TmKnown k)
    TmAppF x y -> TmApp <$> x <*> y
    TmLamF v x -> local (:|> v) x
  bind a = do
    tvs <- ask
    let nvs = Seq.length tvs
    case Seq.findIndexR (== a) tvs of
      Nothing -> throwError (DeclErrTm a)
      Just lvl -> pure (Index (nvs - lvl - 1))

namelessTy :: Scheme TyVar -> Either DeclErr (Scheme Index)
namelessTy (Scheme tvs ty) = fmap (Scheme tvs) (traverse bind ty) where
  nvs = Seq.length tvs
  bind a =
    case Seq.findIndexR (== a) tvs of
      Nothing -> Left (DeclErrTy a)
      Just lvl -> Right (Index (nvs - lvl - 1))

mkDecls :: [(TmName, Scheme TyVar, Maybe (Tm TmVar TmVar))] -> Either (TmName, DeclErr) (Map TmName Decl)
mkDecls = foldM go Map.empty where
  go m (n, s, mt) =
    case mkDecl n s mt of
      Left e -> Left (n, e)
      Right d ->
        case Map.lookup n m of
          Just _ -> Left (n, DeclErrDupe)
          Nothing -> Right (Map.insert n d m)
