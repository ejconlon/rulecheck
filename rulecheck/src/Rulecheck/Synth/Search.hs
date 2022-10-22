module Rulecheck.Synth.Search
  ( SearchConfig (..)
  , SearchSusp
  , nextSearchResult
  , runSearchSusp
  , runSearchN
  ) where

import Control.Applicative (Alternative (..))
import Control.Monad.Identity (Identity (..))
import Control.Monad.Logic (LogicT, MonadLogic (..), observeManyT)
import Control.Monad.Reader (MonadReader (..), ReaderT (..))
import Control.Monad.State.Strict (MonadState (..), State, StateT (..), gets)
import Data.Foldable (asum, foldl', toList)
import Data.Functor.Foldable (cata, embed, project)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq
import IntLike.Map (IntLikeMap)
import qualified IntLike.Map as ILM
import IntLike.Set (IntLikeSet)
import Rulecheck.Synth.Align (TyUnify, TyUniq (..), TyVert (..), mightAlign, recAlignTys)
import Rulecheck.Synth.Core (Index (..), Scheme (..), Tm, TmF (TmFreeF, TmKnownF), TmName, TyF (..))
import Rulecheck.Synth.Decl (Decl (..))
import Rulecheck.Synth.UnionMap (UnionMap)
import qualified Rulecheck.Synth.UnionMap as UM

choose :: (Functor f, Foldable f) => f a -> (a -> SearchM b) -> SearchM b
choose fa f = asum (fmap f fa)

whenAlt :: Alternative f => Bool -> f a -> f a
whenAlt b fa = if b then fa else empty

interleaveAll :: (MonadLogic m, Foldable f) => f (m a) -> m a
interleaveAll = foldr interleave empty

-- just a helper for the cata
runReaderStateM :: r -> s -> ReaderT r (StateT s m) a -> m (a, s)
runReaderStateM r s m = runStateT (runReaderT m r) s

newtype TmUniq = TmUniq { unTmUniq :: Int }
  deriving stock (Show)
  deriving newtype (Eq, Ord, Enum, Num)

type TmUnify = TmF Index TmUniq

data Env = Env
  { envDecls :: !(Map TmName Decl)
  , envCtx :: !(Seq TyUniq)
  , envTys :: !(UnionMap TyUniq TyVert)
  , envGoalKey :: !TyUniq
  , envGoalVal :: !TyUnify
  , envDepthLim :: !Int
  } deriving stock (Eq, Show)

data St = St
  { stTySrc :: !TyUniq
  , stTyReps :: !(IntLikeMap TyUniq (IntLikeSet TmUniq))
  , stTmSrc :: !TmUniq
  , stTms :: !(IntLikeMap TmUniq TmUnify)
  } deriving stock (Eq, Show)

data Instance = Instance !TyUniq !TmUniq
  deriving stock (Eq, Ord, Show)

newtype SearchM a = SearchM { unSearchM :: LogicT (ReaderT Env (State St)) a }
  deriving newtype (Functor, Applicative, Monad, Alternative, MonadReader Env, MonadState St, MonadLogic)

toListWithIndex :: Seq a -> [(a, Index)]
toListWithIndex ss = zip (toList ss) (fmap Index [Seq.length ss - 1 .. 0])

adaptSearch :: TmUniq -> SearchM (Tm Index)
adaptSearch u0 = do
  tms <- gets stTms
  let go u = embed (fmap go (ILM.partialLookup u tms))
  pure (go u0)

newTm :: TmUnify -> SearchM TmUniq
newTm t = state $ \st ->
  let v = stTmSrc st
      tms' = ILM.insert v t (stTms st)
  in (v, st { stTms = tms' })

newTy :: Scheme Index -> (TyUniq -> SearchM a) -> SearchM a
newTy _scheme onAdded = do
  let u = error "TODO"
  let env' = error "TODO"
  local (const env') (onAdded u)

ctxFits :: SearchM Instance
ctxFits = do
  Env _ ctx tys goalKey _ _ <- ask
  choose (toListWithIndex ctx) $ \(candKey, idx) -> do
    case recAlignTys goalKey candKey tys of
      Left _ -> empty
      Right (u, _) -> do
        v <- newTm (TmFreeF idx)
        pure $! Instance u v

exactDeclFits :: SearchM Instance
exactDeclFits = do
  Env decls _ tys goalKey goalVal _ <- ask
  choose (Map.toList decls) $ \(_name, decl) -> do
    -- Do a cheap check on the outside to see if it might align
    let candVal = project (schemeBody (declScheme decl))
    whenAlt (mightAlign goalVal candVal) $ do
      -- It might align. Add the type to the local search env and really align
      newTy (declScheme decl) $ \candKey -> do
        case recAlignTys goalKey candKey tys of
          Left _ -> empty
          Right (u, _) -> do
            v <- newTm (TmKnownF (declName decl))
            pure $! Instance u v

innerSearch :: SearchM Instance
innerSearch = do
  -- TODO add function fits
  interleave ctxFits exactDeclFits

data SearchConfig = SearchConfig
  { scDecls :: !(Map TmName Decl)
  , scTarget :: !(Scheme Index)
  , scDepthLim :: !Int
  } deriving stock (Eq, Show)

lookupCtx :: (MonadFail m, MonadReader (Seq x) m) => Index -> m x
lookupCtx i = do
  xs <- ask
  let j = Seq.length xs - unIndex i - 1
  case Seq.lookup j xs of
    Nothing -> fail ("Missing index " ++ show (unIndex i))
    Just x -> pure x

initEnvSt :: MonadFail m => SearchConfig -> m (Env, St)
initEnvSt (SearchConfig decls (Scheme tvs ty) depthLim) = do
  let (msrc, ctx) = foldl' (\((mx, srcx), ctxx) tv -> ((ILM.insert srcx (TyVertSkolem tv) mx, srcx + 1), ctxx :|> srcx)) ((ILM.empty, 0), Seq.empty) tvs
      insert v = do
        (m, src) <- get
        put (ILM.insert src (TyVertGround v) m, src + 1)
        pure (src, v)
  ((k, v), (m', src')) <- runReaderStateM ctx msrc $ flip cata ty $ \case
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
  let env = Env decls Seq.empty (UM.fromMap m') k v depthLim
      st = St src' ILM.empty 0 ILM.empty
  pure (env, st)

data SearchSusp a = SearchSusp
  { ssEnv :: !Env
  , ssSt :: !St
  , ssAct :: !(SearchM a)
  }

nextSearchResult :: SearchSusp a -> Maybe (a, SearchSusp a)
nextSearchResult (SearchSusp env st act) =
  let (xs, st') = runIdentity (runReaderStateM env st (observeManyT 1 (msplit (unSearchM act))))
  in case xs of
    [] -> Nothing
    x:_ ->
      case x of
        Nothing -> Nothing
        Just (a, act') -> Just (a, SearchSusp env st' (SearchM act'))

runSearchSusp :: MonadFail m => SearchConfig -> m (SearchSusp (Tm Index))
runSearchSusp sc = do
  (env, st) <- initEnvSt sc
  let act = innerSearch >>= \(Instance _ v) -> adaptSearch v
  pure (SearchSusp env st act)

runSearchN :: MonadFail m => SearchConfig -> Int -> m [Tm Index]
runSearchN sc n = do
  (env, st) <- initEnvSt sc
  let act = innerSearch >>= \(Instance _ v) -> adaptSearch v
  let (xs, _) = runIdentity (runReaderStateM env st (observeManyT n (unSearchM act)))
  pure xs
