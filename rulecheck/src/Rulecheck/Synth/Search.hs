module Rulecheck.Synth.Search
  ( TmUniq (..)
  , TmFound
  , SearchConfig (..)
  , SearchSusp
  , nextSearchResult
  , runSearchSusp
  , runSearchN
  ) where

import Control.Applicative (Alternative (..))
import Control.Exception (Exception)
import Control.Monad.Except (Except, MonadError (..), runExcept)
import Control.Monad.Identity (Identity (..))
import Control.Monad.Logic (LogicT, MonadLogic (..), observeManyT)
import Control.Monad.Reader (MonadReader (..), ReaderT (..), asks)
import Control.Monad.State.Strict (MonadState (..), State, StateT (..), gets, modify')
import Data.Foldable (foldl', toList)
import Data.Functor.Foldable (cata, project)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq
import Rulecheck.Synth.Align (TyUnify, TyUniq (..), TyVert (..), mightAlign, recAlignTys)
import Rulecheck.Synth.Core (Index (..), Scheme (..), Tm (..), TmName, TyF (..), TyVar (..))
import Rulecheck.Synth.Decl (Decl (..))
import Rulecheck.Synth.UnionMap (UnionMap)
import qualified Rulecheck.Synth.UnionMap as UM

runReaderStateT :: r -> s -> ReaderT r (StateT s m) a -> m (a, s)
runReaderStateT r s m = runStateT (runReaderT m r) s

runReaderStateExcept :: r -> s -> ReaderT r (StateT s (Except e)) a -> Either e (a, s)
runReaderStateExcept r s m = runExcept (runReaderStateT r s m)

runReaderState :: r -> s -> ReaderT r (State s) a -> (a, s)
runReaderState r s m = runIdentity (runReaderStateT r s m)

whenAlt :: Alternative f => Bool -> f a -> f a
whenAlt b fa = if b then fa else empty

interleaveAll :: (MonadLogic m, Foldable f) => f (m a) -> m a
interleaveAll = foldr interleave empty

choose :: (MonadLogic m, Functor f, Foldable f) => f a -> (a -> m b) -> m b
choose fa f = interleaveAll (fmap f fa)

matchFunction :: TyF a r -> Maybe (r, r)
matchFunction = \case
  TyFunF x y -> Just (x, y)
  _ -> Nothing

newtype TmUniq = TmUniq { unTmUniq :: Int }
  deriving stock (Show)
  deriving newtype (Eq, Ord, Enum, Num)

type TmFound = Tm TmUniq Index

data Env = Env
  { envDecls :: !(Map TmName Decl)
  -- ^ Top-level declarations usable in term search
  , envCtx :: !(Seq TyUniq)
  -- ^ Local type constraints
  , envGoalKey :: !TyUniq
  -- ^ The vertex id of the goal
  , envDepthLim :: !Int
  -- ^ Remaining depth for recursive search
  } deriving stock (Eq, Show)

data St = St
  { stTySrc :: !TyUniq
  -- ^ Next available unique vertex id
  , stTyMap :: !(UnionMap TyUniq TyVert)
  -- ^ Map of vertex id to data
  , stTmSrc :: !TmUniq
  -- ^ Next available unique term var binder
  } deriving stock (Eq, Show)

newtype SearchErr = SearchErrMissingIndex Index
  deriving stock (Eq, Show)

instance Exception SearchErr

newtype SearchM a = SearchM { unSearchM :: LogicT (ReaderT Env (StateT St (Except SearchErr))) a }
  deriving newtype (Functor, Applicative, Monad, Alternative, MonadReader Env, MonadState St, MonadLogic, MonadError SearchErr)

toListWithIndex :: Seq a -> [(a, Index)]
toListWithIndex ss = zip (toList ss) (fmap Index [Seq.length ss - 1 .. 0])

lookupCtx :: (MonadError SearchErr m, MonadReader (Seq x) m) => Index -> m x
lookupCtx i = do
  xs <- ask
  let j = Seq.length xs - unIndex i - 1
  case Seq.lookup j xs of
    Nothing -> throwError (SearchErrMissingIndex i)
    Just x -> pure x

insertTy :: (MonadError SearchErr m, MonadState St m) => (TyVar -> TyVert) -> Scheme Index -> m (TyUniq, TyUnify)
insertTy onVar (Scheme tvs ty) = res where
  insertRaw v (St srcx umx zz) = (srcx, St (srcx + 1) (UM.insert srcx v umx) zz)
  insert v = fmap (,v) (state (insertRaw (TyVertNode v)))
  acc (stx, ctxx) tv = let (u, sty) = insertRaw (onVar tv) stx in (sty, ctxx :|> u)
  onTy = \case
    TyFreeF i -> do
      u <- lookupCtx i
      insert (TyFreeF u)
    TyConF tn ps -> do
      us <- fmap (fmap fst) (sequence ps)
      insert (TyConF tn us)
    TyFunF am bm -> do
      au <- fmap fst am
      bu <- fmap fst bm
      insert (TyFunF au bu)
  res = do
    stStart <- get
    let (stMid, ctx) = foldl' acc (stStart, Seq.empty) tvs
        ea = runReaderStateT ctx stMid (cata onTy ty)
    case ea of
      Left err -> throwError err
      Right ((k, v), stEnd) -> do
        put stEnd
        pure (k, v)

insertMetaTy :: Scheme Index -> SearchM (TyUniq, TyUnify)
insertMetaTy = insertTy TyVertMeta

tryAlignTy :: TyUniq -> SearchM TyUniq
tryAlignTy candKey = do
  goalKey <- asks envGoalKey
  tyMap <- gets stTyMap
  case recAlignTys goalKey candKey tyMap of
    Left _ -> empty
    Right (u, tyMap') -> do
      modify' (\st -> st { stTyMap = tyMap' })
      pure u

-- | Find solutions in the context
ctxFits :: SearchM TmFound
ctxFits = do
  ctx <- asks envCtx
  choose (toListWithIndex ctx) $ \(candKey, idx) -> do
    _ <- tryAlignTy candKey
    pure (TmFree idx)

lookupGoal :: SearchM (TyUniq, TyUnify)
lookupGoal = do
  key <- asks envGoalKey
  um <- gets stTyMap
  let (mp, um') = UM.find key um
  case mp of
    Nothing -> empty
    Just (newKey, vert) -> do
      modify' (\st -> st { stTyMap = um' })
      case vert of
        TyVertNode val -> pure (newKey, val)
        _ -> empty

-- | Find solutions by instantiating decls
exactDeclFits :: SearchM TmFound
exactDeclFits = do
  decls <- asks envDecls
  (_, goalVal) <- lookupGoal
  choose (Map.toList decls) $ \(_name, decl) -> do
    -- Do a cheap check on the outside to see if it might align
    let candVal = project (schemeBody (declScheme decl))
    whenAlt (mightAlign goalVal candVal) $ do
      -- It might align. Add the type to the local search env and really align
      (candKey, _) <- insertMetaTy (declScheme decl)
      _ <- tryAlignTy candKey
      pure (TmKnown (declName decl))

-- | Find solutions to functions by adding to context and finding result
funIntroFits :: SearchM TmFound
funIntroFits = do
  (_, goalVal) <- lookupGoal
  case matchFunction goalVal of
    Nothing -> empty
    Just (argKey, retKey) -> do
      retTm <- local (\env -> env { envCtx = envCtx env :|> argKey, envGoalKey = retKey }) search
      b <- state (\st -> let s = stTmSrc st in (s, st { stTmSrc = s + 1 }))
      pure (TmLam b retTm)

-- | Find solutions by instantiating decl functions with matching return type
funElimFits :: SearchM TmFound
funElimFits = empty  -- TODO

search :: SearchM TmFound
search = res where
  fits = [ctxFits, exactDeclFits, funIntroFits, funElimFits]
  res = interleaveAll fits

data SearchConfig = SearchConfig
  { scDecls :: !(Map TmName Decl)
  , scTarget :: !(Scheme Index)
  , scDepthLim :: !Int
  } deriving stock (Eq, Show)

initEnvSt :: MonadError SearchErr m => SearchConfig -> m (Env, St)
initEnvSt (SearchConfig decls scheme depthLim) = do
  let stStart = St 0 UM.empty 0
  ((key, _), stEnd) <- runStateT (insertTy TyVertSkolem scheme) stStart
  let env = Env decls Seq.empty key depthLim
  pure (env, stEnd)

data SearchSusp a = SearchSusp
  { ssEnv :: !Env
  , ssSt :: !St
  , ssAct :: !(SearchM a)
  }

nextSearchResult :: SearchSusp a -> Either SearchErr (Maybe (a, SearchSusp a))
nextSearchResult (SearchSusp env st act) =
  let ea = runExcept (runReaderStateT env st (observeManyT 1 (msplit (unSearchM act))))
  in flip fmap ea $ \(xs, st') ->
    case xs of
      [] -> Nothing
      x:_ ->
        case x of
          Nothing -> Nothing
          Just (a, act') -> Just (a, SearchSusp env st' (SearchM act'))

runSearchSusp :: SearchConfig -> Either SearchErr (SearchSusp TmFound)
runSearchSusp sc = runExcept $ do
  (env, st) <- initEnvSt sc
  pure (SearchSusp env st search)

runSearchN :: SearchConfig -> Int -> Either SearchErr [TmFound]
runSearchN sc n = runExcept $ do
  (env, st) <- initEnvSt sc
  fmap fst (runReaderStateT env st (observeManyT n (unSearchM search)))
