-- | Declarations of terms in our universe
module Rulecheck.Interface.Decl
  ( Partial (..)
  , Decl (..)
  , DeclErr
  , mkDecl
  , mkDecls
  ) where

import Control.Exception (Exception)
import Control.Monad (foldM)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq
import Rulecheck.Interface.Core (Index (..), Scheme (..), TmName, Ty (..), TyVar)

-- | The type of a partial function application
data Partial = Partial
  { partialArgs :: !(Seq (Ty Index))
  , partialRet :: !(Ty Index)
  } deriving stock (Eq, Ord, Show)

-- | A declared term (essentially name and type scheme)
data Decl = Decl
  { declName :: !TmName
  -- ^ The name of the declared term (will be used in 'TmKnown' constructors)
  , declScheme :: !(Scheme Index)
  -- ^ The type scheme of the term
  , declPartials :: !(Seq Partial)
  -- ^ If this is a function declaration, types of partial applications
  } deriving stock (Eq, Ord, Show)

data DeclErr =
    DeclErrTy !TyVar
  | DeclErrDupe
  deriving stock (Eq, Ord, Show)

instance Exception DeclErr

mkDecl :: TmName -> Scheme TyVar -> Either DeclErr Decl
mkDecl n s = do
  s' <- namelessTy s
  let ps = matchPartials (schemeBody s')
  pure (Decl n s' ps)

matchPartials :: Ty Index -> Seq Partial
matchPartials = onOuter where
  onOuter = \case
    TyFun x y -> onInner (Seq.singleton x) y
    _ -> Empty
  onInner as t = Partial as t :<| case t of
    TyFun x y -> onInner (as :|> x) y
    _ -> Empty

namelessTy :: Scheme TyVar -> Either DeclErr (Scheme Index)
namelessTy (Scheme tvs ty) = fmap (Scheme tvs) (traverse bind ty) where
  nvs = Seq.length tvs
  bind a =
    case Seq.findIndexR (== a) tvs of
      Nothing -> Left (DeclErrTy a)
      Just lvl -> Right (Index (nvs - lvl - 1))

mkDecls :: [(TmName, Scheme TyVar)] -> Either (TmName, DeclErr) (Map TmName Decl)
mkDecls = foldM go Map.empty where
  go m (n, s) =
    case mkDecl n s of
      Left e -> Left (n, e)
      Right d ->
        case Map.lookup n m of
          Just _ -> Left (n, DeclErrDupe)
          Nothing -> Right (Map.insert n d m)
