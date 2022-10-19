{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Rulecheck.Synth where

import Control.Exception (Exception)
import Control.Monad (foldM, when)
import Control.Monad.Except (Except, MonadError (..), runExcept)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (MonadReader (..), ReaderT (..), asks)
import Control.Monad.State.Strict (MonadState (..), StateT)
import Data.Foldable (for_)
import Data.Functor.Foldable (cata, project)
import Data.Functor.Foldable.TH (makeBaseFunctor)
import Data.IORef (newIORef, readIORef)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq
import Data.String (IsString)
import Data.Text (Text)

-- Program synthesis

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
data Tm a =
    TmFree !a
  | TmKnown !TmName
  | TmApp (Tm a) (Tm a)
  | TmLam !TmVar (Tm a)
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)

-- The TH here punches a hole in the recursive parts too, so we get simple folds for types and terms

makeBaseFunctor ''Ty
deriving instance (Eq a, Eq r) => Eq (TyF a r)
deriving instance (Ord a, Ord r) => Ord (TyF a r)
deriving instance (Show a, Show r) => Show (TyF a r)

makeBaseFunctor ''Tm
deriving instance (Eq a, Eq r) => Eq (TmF a r)
deriving instance (Ord a, Ord r) => Ord (TmF a r)
deriving instance (Show a, Show r) => Show (TmF a r)

-- | Something that can go wrong when aligning (matching) two types
data AlignErr =
    AlignErrConHead !TyName !TyName
  | AlignErrConArity !TyName !Int !Int
  | AlignErrMismatch
  deriving stock (Eq, Ord, Show)

instance Exception AlignErr

-- | Align (match) two types by lining up all the holes
alignTys :: TyF x a -> TyF y b -> Either AlignErr (TyF (x, y) (a, b))
alignTys one two =
  case (one, two) of
    (TyFreeF x, TyFreeF y) -> Right (TyFreeF (x, y))
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

-- | Do two types have the potential to align?
mightAlign :: TyF x a -> TyF y b -> Bool
mightAlign one two =
  case (one, two) of
    (TyFreeF _, TyFreeF _) -> True
    (TyConF n as, TyConF m bs) -> n == m && Seq.length as == Seq.length bs
    (TyFunF _ _, TyFunF _ _) -> True
    _ -> False

-- | Calculates the depth of a term (tree depth - max count of nested constructors)
tmDepth :: Tm y -> Int
tmDepth = cata go where
  go = \case
    TmFreeF _ -> 1
    TmKnownF _ -> 1
    TmAppF x y -> 1 + max x y
    TmLamF _ b -> 1 + b

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

mkDecl :: TmName -> Scheme TyVar -> Maybe (Tm TmVar) -> Either DeclErr Decl
mkDecl n s mt = do
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

mkDecls :: [(TmName, Scheme TyVar, Maybe (Tm TmVar))] -> Either (TmName, DeclErr) (Map TmName Decl)
mkDecls = foldM go Map.empty where
  go m (n, s, mt) =
    case mkDecl n s mt of
      Left e -> Left (n, e)
      Right d ->
        case Map.lookup n m of
          Just _ -> Left (n, DeclErrDupe)
          Nothing -> Right (Map.insert n d m)

newtype TyUniq = TyUniq { unTyUniq :: Int }
  deriving stock (Show)
  deriving newtype (Eq, Ord, Enum, Num)

newtype TmUniq = TmUniq { unTmUniq :: Int }
  deriving stock (Show)
  deriving newtype (Eq, Ord, Enum, Num)

type TyUnify = TyF TyUniq TyUniq
type TmUnify = TmF TmUniq TmUniq

data TyVert =
    TyVertMeta !TyVar
  | TyVertSkolem !TyVar
  | TyVertGround !TyUnify
  deriving stock (Eq, Ord, Show)

exampleDecls :: Either (TmName, DeclErr) (Map TmName Decl)
exampleDecls = res where
  tyInt = TyCon "Int" Empty
  tyIntFun2 = TyFun tyInt (TyFun tyInt tyInt)
  tmA = TmFree "a"
  tmB = TmFree "b"
  res = mkDecls
    [ ("myPlus", Scheme Empty tyIntFun2, Just (TmLam "a" (TmLam "b" (TmApp (TmApp (TmKnown "+") tmA) tmB))))
    ]

data Env = Env
  { envDecls :: !(Map TmName Decl)
  , envTys :: !(Map TyUniq TyVert)
  , envGoalKey :: !TyUniq
  , envGoalVal :: !TyUnify
  , envDepthLim :: !Int
  }

data St = St
  { stTySrc :: !TyUniq
  , stTmSrc :: !TmUniq
  , stTms :: !(Map TmUniq TmUnify)
  }

newtype SearchM a = SearchM { unSearchM :: ReaderT Env (StateT St IO) a }
  deriving newtype (Functor, Applicative, Monad, MonadReader Env, MonadState St, MonadIO)

runSearchM :: SearchM a -> Env -> St -> IO (a, St)
runSearchM = error "TODO"

innerSearch :: (TyUniq -> SearchM ()) -> (TmUniq -> SearchM ()) -> SearchM ()
innerSearch _nominate _answer = do
  goalVal <- asks envGoalVal
  depthLim <- asks envDepthLim
  decls <- asks envDecls
  -- Go through all known decls to find something that might fit this hole
  for_ (Map.toList decls) $ \(_name, decl) ->
    -- Only consider those that will fit our remaining depth
    when (declDepth decl <= depthLim) $ do
      -- TODO match against function forms instead of whole scheme
      -- Do a cheap check on the outside to see if it might align
      let candVal = project (schemeBody (declScheme decl))
      when (mightAlign goalVal candVal) $ do
        -- TODO add, align
        pure ()

initEnv :: Scheme Index -> Int -> IO Env
initEnv = error "TODO"

outerSearch :: Scheme Index -> Int -> IO ([TmUniq], Map TmUniq TmUnify)
outerSearch s d = go where
  go = do
    env <- initEnv s d
    nomRef <- newIORef []
    ansRef <- newIORef []
    stRef <- newIORef (St 0 0 mempty)
    let m = innerSearch (nominate nomRef) (answer ansRef)
    _ <- loop nomRef env m
    finals <- liftIO (readIORef ansRef)
    meanings <- fmap stTms (readIORef stRef)
    pure (finals, meanings)
  loop _nomRef _env _m = error "TODO"
  nominate _nomRef _cand = error "TODO"
  answer _ansRef _ans = error "TODO"
