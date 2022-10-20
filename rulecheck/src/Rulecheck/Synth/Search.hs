module Rulecheck.Synth.Search where

import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (MonadReader (..), ReaderT (..), asks)
import Control.Monad.State.Strict (MonadState (..), StateT (..))
import Data.Foldable (foldl', for_, toList)
import Data.Functor.Foldable (cata, project)
import Data.IORef (modifyIORef', newIORef, readIORef, writeIORef)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq
import IntLike.Map (IntLikeMap)
import qualified IntLike.Map as ILM
import IntLike.Set (IntLikeSet)
import Rulecheck.Synth.Align (TyUnify, TyUniq (..), TyVert (..), mightAlign, recAlignTys)
import Rulecheck.Synth.Core (Index (..), Scheme (..), TmF, TmName, TyF (..))
import Rulecheck.Synth.Decl (Decl (..))
import Rulecheck.Synth.UnionMap (UnionMap)
import qualified Rulecheck.Synth.UnionMap as UM

newtype TmUniq = TmUniq { unTmUniq :: Int }
  deriving stock (Show)
  deriving newtype (Eq, Ord, Enum, Num)

type TmUnify = TmF Index TyUniq

data Env = Env
  { envDecls :: !(Map TmName Decl)
  , envCtx :: !(Seq TyUniq)
  , envTys :: !(UnionMap TyUniq TyVert)
  , envParents :: ![TyUniq]
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

newtype SearchM a = SearchM { unSearchM :: ReaderT Env (StateT St IO) a }
  deriving newtype (Functor, Applicative, Monad, MonadReader Env, MonadState St, MonadIO)

runSearchM :: SearchM a -> Env -> St -> IO (a, St)
runSearchM m env = runStateT (runReaderT (unSearchM m) env)

toListWithIndex :: Seq a -> [(a, Index)]
toListWithIndex ss = zip (toList ss) (fmap Index [Seq.length ss - 1 .. 0])

innerSearch :: (Env -> SearchM ()) -> (TmUniq -> SearchM ()) -> SearchM ()
innerSearch _nominate _answer = do
  Env decls ctx _ _ goalKey goalVal _<- ask
  -- Go through all variables in context to find something that might fit the hole
  for_ (toListWithIndex ctx) $ \(candKey, _idx) -> do
    um <- asks envTys
    case recAlignTys goalKey candKey um of
      Left _ -> pure ()
      Right (_u, _) -> do
        -- TODO fill in var
        error "TODO"
  -- Go through all known decls to find something that might fit this hole exactly
  for_ (Map.toList decls) $ \(_name, decl) -> do
    -- Do a cheap check on the outside to see if it might align
    let candVal = project (schemeBody (declScheme decl))
    when (mightAlign goalVal candVal) $ do
      -- liftIO (putStrLn ("Might align: " ++ show candVal))  -- XXX
      insertingTy (declScheme decl) $ \candKey -> do
        um <- asks envTys
        case recAlignTys goalKey candKey um of
          Left _ -> pure ()
          Right (_u, _um') -> do
            -- * add term to map
            -- * if parents is empty (this is top), emit an answer
            --   this answer will need to be checked later
            --   because we'll only know if search successfully filled out all subterms
            --   eventually use a dependency map to emit only finished stuff
            -- * lookup the ty and nominate envs for children
            --   (with appropriate goals, depth, and parents)
            error "TODO"
      pure ()
  -- TODO go through all function decls to find something that might fit the hole
  -- given some arguments

insertingTy :: Scheme Index -> (TyUniq -> SearchM ()) -> SearchM ()
insertingTy _scheme onAdded = do
  let u = error "TODO"
  let env' = error "TODO"
  local (const env') (onAdded u)

-- just a helper for the cata
runReaderStateM :: r -> s -> ReaderT r (StateT s m) a -> m (a, s)
runReaderStateM r s m = runStateT (runReaderT m r) s

lookupCtx :: (MonadFail m, MonadReader (Seq x) m) => Index -> m x
lookupCtx i = do
  xs <- ask
  let j = Seq.length xs - unIndex i - 1
  case Seq.lookup j xs of
    Nothing -> fail ("Missing index " ++ show (unIndex i))
    Just x -> pure x

initEnvSt :: MonadFail m => Map TmName Decl -> Scheme Index -> Int -> m (Env, St)
initEnvSt decls (Scheme tvs ty) depthLim = do
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
  let env = Env decls Seq.empty (UM.fromMap m') [] k v depthLim
      st = St src' ILM.empty 0 ILM.empty
  pure (env, st)

outerSearch :: Map TmName Decl -> Scheme Index -> Int -> IO ([TmUniq], IntLikeMap TyUniq (IntLikeSet TmUniq), IntLikeMap TmUniq TmUnify)
outerSearch decls scheme depthLim = go where
  go = do
    (env, st) <- initEnvSt decls scheme depthLim
    nomRef <- newIORef ([] :: [Env])
    ansRef <- newIORef ([] :: [TmUniq])
    stRef <- newIORef st
    let m = innerSearch (nominate nomRef) (answer ansRef)
    _ <- loop nomRef stRef env m
    finals <- liftIO (readIORef ansRef)
    tyReps <- fmap stTyReps (readIORef stRef)
    tms <- fmap stTms (readIORef stRef)
    pure (finals, tyReps, tms)
  loop nomRef stRef env m = do
    st <- readIORef stRef
    ((), st') <- runSearchM m env st
    writeIORef stRef st'
    noms <- readIORef nomRef
    case noms of
      [] -> pure ()
      env':envs -> do
        writeIORef nomRef envs
        loop nomRef stRef env' m
  nominate nomRef env = liftIO (modifyIORef' nomRef (env:))
  answer ansRef ans = liftIO (modifyIORef' ansRef (ans:))
