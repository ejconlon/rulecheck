-- | Declarations of terms in our universe
module Rulecheck.Interface.Decl
  ( Partial (..)
  , Decl (..)
  , DeclErr
  , DeclSet (..)
  , mkDecl
  , mkDecls
  ) where

import Control.Exception (Exception)
import Control.Monad.Except (Except, MonadError (..), runExcept)
import Control.Monad.State.Strict (StateT (..), gets, modify')
import Data.Foldable (traverse_)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq
import Rulecheck.Interface.Core (Index (..), Inst (..), Scheme (..), TmName, Ty (..), TyVar)
import Rulecheck.Interface.Types (FuncLine (..), Line (..))

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

newtype DeclSet = DeclSet
  { dsMap :: Map TmName Decl
  } deriving stock (Eq, Show)

emptyDeclSet :: DeclSet
emptyDeclSet = DeclSet Map.empty

data DeclErr =
    DeclErrTy !TyVar
  | DeclErrDupe
  | DeclErrNamed !TmName DeclErr
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
namelessTy (Scheme tvs pars ty) = Scheme tvs <$> traverse bindInst pars <*> traverse bind ty where
  nvs = Seq.length tvs
  bind a =
    case Seq.findIndexR (== a) tvs of
      Nothing -> Left (DeclErrTy a)
      Just lvl -> Right (Index (nvs - lvl - 1))
  bindInst (Inst cn tys) = Inst cn <$> traverse (traverse bind) tys

type DeclM a = StateT DeclSet (Except DeclErr) a

runDeclM :: DeclM a -> DeclSet -> Either DeclErr (a, DeclSet)
runDeclM m = runExcept . runStateT m

execDeclM :: DeclM () -> DeclSet -> Either DeclErr DeclSet
execDeclM m = fmap snd . runDeclM m

insertDeclM :: TmName -> Scheme TyVar -> DeclM ()
insertDeclM n s =
  case mkDecl n s of
    Left e -> throwError (DeclErrNamed n e)
    Right d -> do
      m <- gets dsMap
      case Map.lookup n m of
        Just _ -> throwError (DeclErrNamed n DeclErrDupe)
        Nothing -> modify' (\ds -> ds { dsMap = Map.insert n d m })

insertLineM :: Line -> DeclM ()
insertLineM = \case
    -- TODO add more info to declset
    LineFunc (FuncLine n _ s) -> insertDeclM n s
    _ -> pure ()

mkDecls :: [(TmName, Scheme TyVar)] -> Either DeclErr DeclSet
mkDecls = flip execDeclM emptyDeclSet . traverse_ (uncurry insertDeclM)

mkLineDecls :: [Line] -> Either DeclErr DeclSet
mkLineDecls = flip execDeclM emptyDeclSet . traverse_ insertLineM
