-- | Declarations of terms in our universe
module Rulecheck.Interface.Decl where
  -- ( Partial (..)
  -- , Decl (..)
  -- , DeclErr
  -- , DeclSet (..)
  -- , mkDecl
  -- , mkDecls
  -- ) where

import Control.Exception (Exception)
import Control.Monad.Except (Except, MonadError (..), runExcept)
import Control.Monad.State.Strict (StateT (..), gets, modify')
import Data.Foldable (traverse_)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq
import Rulecheck.Interface.Core (Forall (..), Index (..), Inst (..), Strained (..), TmName, Ty (..), TyScheme (..),
                                 TyVar, tySchemeBody)
import Rulecheck.Interface.Types (FuncLine (..), InstLine (..), Line (..))

-- | The type of a partial function application
data Partial = Partial
  { partialArgs :: !(Seq (Ty Index))
  , partialRet :: !(Ty Index)
  } deriving stock (Eq, Ord, Show)

-- | A declared term (essentially name and type scheme)
data Decl = Decl
  { declName :: !TmName
  -- ^ The name of the declared term (will be used in 'TmKnown' constructors)
  , declType :: !(TyScheme Index)
  -- ^ The type scheme of the term
  , declPartials :: !(Seq Partial)
  -- ^ If this is a function declaration, types of partial applications
  } deriving stock (Eq, Ord, Show)

data DeclSet = DeclSet
  { dsMap :: !(Map TmName Decl)
  -- ^ Map from term to its definition
  , dsDeps :: !(Map (Inst TyVar) (Seq (Inst TyVar)))
  -- ^ Map from class instance to its parents (dependencies)
  } deriving stock (Eq, Show)

emptyDeclSet :: DeclSet
emptyDeclSet = DeclSet Map.empty Map.empty

data DeclErr =
    DeclErrTy !TyVar
  | DeclErrDupe
  | DeclErrNamed !TmName DeclErr
  | DeclErrInst !(Inst TyVar) DeclErr
  deriving stock (Eq, Ord, Show)

instance Exception DeclErr

mkDecl :: TmName -> TyScheme TyVar -> Either DeclErr Decl
mkDecl n s = do
  s' <- namelessType s
  let ps = matchPartials (tySchemeBody s')
  pure (Decl n s' ps)

matchPartials :: Ty Index -> Seq Partial
matchPartials = onOuter where
  onOuter = \case
    TyFun x y -> onInner (Seq.singleton x) y
    _ -> Empty
  onInner as t = Partial as t :<| case t of
    TyFun x y -> onInner (as :|> x) y
    _ -> Empty

namelessType :: TyScheme TyVar -> Either DeclErr (TyScheme Index)
namelessType (TyScheme (Forall tvs ct)) = TyScheme . Forall tvs <$> bindStr ct where
  bindStr (Strained cons ty) = Strained <$> traverse bindCon cons <*> traverse bind ty
  nvs = Seq.length tvs
  bind a =
    case Seq.findIndexR (== a) tvs of
      Nothing -> Left (DeclErrTy a)
      Just lvl -> Right (Index (nvs - lvl - 1))
  bindCon (Inst cn tys) = Inst cn <$> traverse (traverse bind) tys

type DeclM a = StateT DeclSet (Except DeclErr) a

runDeclM :: DeclM a -> DeclSet -> Either DeclErr (a, DeclSet)
runDeclM m = runExcept . runStateT m

execDeclM :: DeclM () -> DeclSet -> Either DeclErr DeclSet
execDeclM m = fmap snd . runDeclM m

insertTmDeclM :: TmName -> TyScheme TyVar -> DeclM ()
insertTmDeclM n s =
  case mkDecl n s of
    Left e -> throwError (DeclErrNamed n e)
    Right d -> do
      m <- gets dsMap
      case Map.lookup n m of
        Just _ -> throwError (DeclErrNamed n DeclErrDupe)
        Nothing -> modify' (\ds -> ds { dsMap = Map.insert n d (dsMap ds) })

insertInstM :: Inst TyVar -> Seq (Inst TyVar) -> DeclM ()
insertInstM self pars = do
  d <- gets dsDeps
  case Map.lookup self d of
    Just _ -> throwError (DeclErrInst self DeclErrDupe)
    Nothing -> modify' (\ds -> ds { dsDeps = Map.insert self pars (dsDeps ds) })

insertLineM :: Line -> DeclM ()
insertLineM = \case
    LineFunc (FuncLine n s) -> insertTmDeclM n s
    LineInst (InstLine self pars) -> insertInstM self pars
    -- Do we need to add anything else to the decl set for search?
    _ -> pure ()

mkDecls :: [(TmName, TyScheme TyVar)] -> Either DeclErr DeclSet
mkDecls = flip execDeclM emptyDeclSet . traverse_ (uncurry insertTmDeclM)

mkLineDecls :: [Line] -> Either DeclErr DeclSet
mkLineDecls = flip execDeclM emptyDeclSet . traverse_ insertLineM
