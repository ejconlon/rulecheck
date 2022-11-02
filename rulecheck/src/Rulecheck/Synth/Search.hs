-- | Methods to enumerate terms of a given type
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
import Control.Monad.Logic (LogicT (..), MonadLogic (..), observeManyT)
import Control.Monad.Reader (MonadReader (..), ReaderT (..), asks)
import Control.Monad.State.Strict (MonadState (..), StateT (..), gets, modify')
import Data.Bifunctor (second)
import Data.Foldable (foldl', for_, toList)
import Data.Functor.Foldable (cata, project)
import qualified Data.Map.Strict as Map
import Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq
import Data.Traversable (for)
import ListT (ListT (..))
import qualified ListT
import Rulecheck.Interface.Core (ClsName, Forall (Forall), Index (..), Inst (..), Scheme (..), StraintTy (..), Tm (..),
                                 TmName, Ty, TyF (..), TyVar (..), schemeBody)
import Rulecheck.Interface.Decl (Decl (..), DeclSet (..), Partial (..))
import Rulecheck.Synth.Align (TyUnify, TyUniq (..), TyVert (..), mightAlign, recAlignTys)
import Rulecheck.Synth.UnionMap (UnionMap)
import qualified Rulecheck.Synth.UnionMap as UM

-- boilerplate
runReaderStateT :: r -> s -> ReaderT r (StateT s m) a -> m (a, s)
runReaderStateT r s m = runStateT (runReaderT m r) s

-- | If true, run the search, else yield nothing
whenAlt :: Alternative m => Bool -> m a -> m a
whenAlt b fa = if b then fa else empty

-- | Interleave a list of searches - fairly!
interleaveAll :: (MonadLogic m, Foldable f) => f (m a) -> m a
interleaveAll = foldr interleave empty

-- | Choose a value from a list and continue the search with it - fairly!
choose :: (MonadLogic m, Functor f, Foldable f) => f a -> (a -> m b) -> m b
choose fa f = interleaveAll (fmap f fa)

-- | A unique binder for enumerated lambdas
newtype TmUniq = TmUniq { unTmUniq :: Int }
  deriving stock (Show)
  deriving newtype (Eq, Ord, Enum, Num)

-- | A typeclass constraint for unification.
data StraintUniq = StraintUniq
  { suCls :: !ClsName
  , suArgs :: !(Seq TyUniq)
  } deriving stock (Eq, Ord, Show)

-- | Search will yield closed terms with globally unique binders
type TmFound = Tm TmUniq Index

-- | Local environment for search (usable with 'MonadReader')
data Env = Env
  { envDecls :: !DeclSet
  -- ^ Top-level declarations usable in term search
  , envCtx :: !(Seq TyUniq)
  -- ^ Local type constraints
  , envDepthLim :: !Int
  -- ^ Remaining depth for recursive search
  } deriving stock (Eq, Show)

-- | Global state for search (usable with 'MonadState')
data St = St
  { stTySrc :: !TyUniq
  -- ^ Next available unique vertex id
  , stTyMap :: !(UnionMap TyUniq TyVert)
  -- ^ Map of vertex id to data
  , stTmSrc :: !TmUniq
  -- ^ Next available unique term var binder
  } deriving stock (Eq, Show)

-- | Globally terminating errors for search (usable with 'MonadError')
newtype SearchErr =
    SearchErrMissingIndex Index
  -- ^ Indicates undefined index in a declaration (meaning decl is not actually closed!)
  -- This is a non-recoverable error - you need to fix the declarations.
  deriving stock (Eq, Show)

instance Exception SearchErr

-- | The inner layer of the search monad. Though this is package-private, we have to newtype it
-- so we can refer to it as we stream results.
newtype InnerM a = InnerM { unInnerM :: ReaderT Env (StateT St (Except SearchErr)) a }
  deriving newtype (Functor, Applicative, Monad, MonadReader Env, MonadState St, MonadError SearchErr)

runInnerM :: InnerM a -> Env -> St -> Either SearchErr (a, St)
runInnerM m r s = runExcept (runStateT (runReaderT (unInnerM m) r) s)

-- | The core search monad. 'LogicT' is a list transformer monad with support for fair interleavings.
newtype SearchM a = SearchM { unSearchM :: LogicT InnerM a }
  deriving newtype (Functor, Applicative, Monad, Alternative, MonadReader Env, MonadState St, MonadLogic, MonadError SearchErr)

toListWithIndex :: Seq a -> [(a, Index)]
toListWithIndex ss = zip (toList ss) (fmap Index [Seq.length ss - 1 .. 0])

-- | Lookup a bound type variable during type insertion
lookupCtx :: (MonadError SearchErr m, MonadReader (Seq x) m) => Index -> m x
lookupCtx i = do
  xs <- ask
  let j = Seq.length xs - unIndex i - 1
  case Seq.lookup j xs of
    Nothing -> throwError (SearchErrMissingIndex i)
    Just x -> pure x

-- | Instantiate the type variables in the scheme with the given strategy, bind them in the local
-- typing context, then insert the type into the union map. The strategy is used to instantiate with skolem vars
-- (non-unifiable / "externally-chosen" vars) at the top level or simple meta vars (plain old unifiable vars) below.
-- NOTE: The constraints returned are not unified with instance derivations. You have to do that after calling this.
insertScheme :: (MonadError SearchErr m, MonadState St m) => (TyVar -> TyVert) -> Scheme Index -> m (Seq StraintUniq, TyUniq, TyUnify)
insertScheme onVar (Scheme (Forall tvs (StraintTy cons ty))) = res where
  insertRaw v (St srcx umx zz) = (srcx, St (srcx + 1) (UM.insert srcx v umx) zz)
  acc (stx, ctxx) tv = let (u, sty) = insertRaw (onVar tv) stx in (sty, ctxx :|> u)
  res = do
    stStart <- get
    let (stMid, ctx) = foldl' acc (stStart, Seq.empty) tvs
    put stMid
    ius <- for cons $ \(Inst cn tys) -> do
      us <- traverse (fmap fst . insertTy ctx) tys
      pure (StraintUniq cn us)
    (u, v) <- insertTy ctx ty
    pure (ius, u, v)

-- | Used in 'insertScheme' to do the type insertion.
insertTy :: (MonadError SearchErr m, MonadState St m) => Seq TyUniq -> Ty Index -> m (TyUniq, TyUnify)
insertTy ctx ty = res where
  insertRaw v (St srcx umx zz) = (srcx, St (srcx + 1) (UM.insert srcx v umx) zz)
  insert v = fmap (,v) (state (insertRaw (TyVertNode v)))
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
    stMid <- get
    let ea = runReaderStateT ctx stMid (cata onTy ty)
    case ea of
      Left err -> throwError err
      Right ((k, v), stEnd) -> do
        put stEnd
        pure (k, v)

-- | Instantiates the scheme with metavars
insertMetaScheme :: Scheme Index -> SearchM (Seq StraintUniq, TyUniq, TyUnify)
insertMetaScheme = insertScheme TyVertMeta

-- | Allocate a fresh term binder
freshTmBinder :: SearchM TmUniq
freshTmBinder = state (\st -> let s = stTmSrc st in (s, st { stTmSrc = s + 1 }))

-- | Yield the merged vertex id if the given vertex aligns with the current goal,
-- otherwise yield nothing.
tryAlignTy :: TyUniq -> TyUniq -> SearchM TyUniq
tryAlignTy goalKey candKey = do
  tyMap <- gets stTyMap
  case recAlignTys goalKey candKey tyMap of
    Left _ -> empty
    Right (u, tyMap') -> do
      modify' (\st -> st { stTyMap = tyMap' })
      pure u

-- | Find solutions by looking in the context for vars that match exactly.
ctxFits :: TyUniq -> SearchM TmFound
ctxFits goalKey = do
  ctx <- asks envCtx
  choose (toListWithIndex ctx) $ \(candKey, idx) -> do
    _ <- tryAlignTy goalKey candKey
    pure (TmFree idx)

-- | If the goal is a type vertex (not a meta/skolem vertex), yield it.
lookupGoal :: TyUniq -> SearchM (TyUniq, TyUnify)
lookupGoal goalKey = do
  um <- gets stTyMap
  let (mp, um') = UM.find goalKey um
  case mp of
    Nothing -> empty
    Just (newKey, vert) -> do
      modify' (\st -> st { stTyMap = um' })
      case vert of
        TyVertNode val -> pure (newKey, val)
        _ -> empty

-- | Find solutions by instantiating decls and matching the goal exactly.
exactDeclFits :: TyUniq -> SearchM TmFound
exactDeclFits goalKey = do
  decls <- asks envDecls
  (_, goalVal) <- lookupGoal goalKey
  choose (Map.toList (dsMap decls)) $ \(name, decl) -> do
    -- Do a cheap check on the outside to see if it might align
    let candVal = project (schemeBody (declScheme decl))
    whenAlt (mightAlign goalVal candVal) $ do
      -- Ok, it might align. Add the type to the local search env and see if it really does.
      (candStraints, candKey, _) <- insertMetaScheme (declScheme decl)
      for_ candStraints tryUnifyStraint
      _ <- tryAlignTy goalKey candKey
      pure (TmKnown name)

-- | Find solutions to function-type goals by adding args to context and searching with the result type.
funIntroFits :: TyUniq -> SearchM TmFound
funIntroFits goalKey = do
  -- Only try this if we haven't hit the recursion depth limit
  depthLim <- asks envDepthLim
  if depthLim <= 0
    then empty
    else do
      (_, goalVal) <- lookupGoal goalKey
      case goalVal of
        TyFunF argKey retKey -> do
          -- If the goal looks like a function, try adding the arg to the context and searching for a match.
          retTm <- local (\env -> env { envCtx = envCtx env :|> argKey, envDepthLim = depthLim - 1 }) (searchUniq retKey)
          -- Success - allocate a fresh binder and return a lambda.
          b <- freshTmBinder
          pure (TmLam b retTm)
        _ -> empty

-- | Helper to return a multi-arg application
mkApp :: TmName -> Seq TmFound -> TmFound
mkApp n = go (TmKnown n) where
  go !t = \case
    Empty -> t
    s :<| ss -> go (TmApp t s) ss

-- | Find solutions by instantiating decl functions with matching return type
funElimFits :: TyUniq -> SearchM TmFound
funElimFits goalKey = do
  -- Only try this if we haven't hit the recursion depth limit
  depthLim <- asks envDepthLim
  if depthLim <= 0
    then empty
    else do
      decls <- asks envDecls
      (_, goalVal) <- lookupGoal goalKey
      choose (Map.toList (dsMap decls)) $ \(name, decl) -> do
        -- Partials are defined for any curried lambda - args will be nonempty
        choose (declPartials decl) $ \(Partial args retTy) -> do
          let candVal = project retTy
          -- Do a cheap check for possible alignment on the result type
          whenAlt (mightAlign goalVal candVal) $ do
            -- Now really check that the result type unifies:
            -- Add args to the typing context, insert the return type and unify.
            oldCtx <- asks envCtx
            addlCtx <- traverse (fmap fst . insertTy oldCtx) args
            let newCtx = oldCtx <> addlCtx
            (candKey, _) <- insertTy newCtx retTy
            _ <- tryAlignTy goalKey candKey
            -- It unifies. Now we know that if we can find args we can satsify the goal.
            argTms <- traverse (local (\env -> env { envDepthLim = depthLim - 1 }) . searchUniq) addlCtx
            pure (mkApp name argTms)
            -- TODO Technically we don't have to find a term for each arg independently,
            -- or even left to right. However, we need to tame the explosion of cases somehow.
            -- Here we just find them independently nest them in an application. We could have
            -- introduced lets with a bound application at then end (in ANF).
            -- Maybe it is worth trying all possibilities but guarding with logict's 'once'.

-- | Search for a term matching the current goal type using a number of interleaved strategies.
searchUniq :: TyUniq -> SearchM TmFound
searchUniq goalKey = res where
  -- TODO add the following search strategies:
  -- * Given function in context, see if the result of the function helps you solve the goal.
  --   If so, search for the arg of the function and return the application. (coq apply?)
  -- * Case split on all constructors of a datatype. (coq destruct?)
  -- Really, just look up the standard coq tactics and do what they do.
  fits = [ctxFits goalKey, exactDeclFits goalKey, funIntroFits goalKey, funElimFits goalKey]
  res = interleaveAll fits

-- TODO implement this!
tryUnifyStraint :: StraintUniq -> SearchM ()
tryUnifyStraint _su = pure ()

-- | Outermost search interface: Insert the given scheme and search for terms matching it.
searchScheme :: Scheme Index -> SearchM TmFound
searchScheme scheme = do
  (goalStraints, goalKey, _) <- insertScheme TyVertSkolem scheme
  for_ goalStraints tryUnifyStraint
  searchUniq goalKey

-- | General search parameters
data SearchConfig = SearchConfig
  { scDecls :: !DeclSet
  -- ^ Top-level declarations
  , scTarget :: !(Scheme Index)
  -- ^ Search goal
  , scDepthLim :: !Int
  -- ^ Recursion depth limit
  } deriving stock (Eq, Show)

-- | Initialize the search environment
initEnvSt :: SearchConfig -> (Env, St)
initEnvSt (SearchConfig decls _ depthLim) =
  let env = Env decls Seq.empty depthLim
      st = St 0 UM.empty 0
  in (env, st)

-- | A "suspended" search for incremental consumption
data SearchSusp a = SearchSusp
  { ssEnv :: !Env
  , ssSt :: !St
  , ssAct :: !(ListT InnerM a)
  }

-- | Yield the next result from a suspended search
nextSearchResult :: SearchSusp a -> Either SearchErr (Maybe (a, SearchSusp a))
nextSearchResult (SearchSusp env st act) =
  fmap (\(mx, st') -> fmap (second (SearchSusp env st')) mx) (runInnerM (ListT.uncons act) env st)

-- | Streams search results as an unconsable effectful list.
mkStream :: LogicT InnerM a -> ListT InnerM a
mkStream (LogicT f) = enclose (f onCons onEmpty) where
  enclose = ListT . (>>= \(ListT x) -> x)
  onCons = fmap . ListT.cons
  onEmpty = pure empty

-- | Search for terms of the goal type with incremental consumption.
runSearchSusp :: SearchConfig -> SearchSusp TmFound
runSearchSusp sc =
  let (env, st) = initEnvSt sc
      act = mkStream (unSearchM (searchScheme (scTarget sc)))
  in SearchSusp env st act

-- | Search for up to N terms of the goal type.
runSearchN :: SearchConfig -> Int -> Either SearchErr [TmFound]
runSearchN sc n =
  let (env, st) = initEnvSt sc
  in fmap fst (runInnerM (observeManyT n (unSearchM (searchScheme (scTarget sc)))) env st)
