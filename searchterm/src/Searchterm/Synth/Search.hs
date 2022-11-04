-- | Methods to enumerate terms of a given type
module Searchterm.Synth.Search
  ( TmUniq (..)
  , TmFound
  , SearchErr (..)
  , SearchConfig (..)
  , SearchSusp
  , nextSearchResult
  , takeSearchResults
  , runSearchSusp
  , runSearchN
  ) where

import Control.Applicative (Alternative (..))
import Control.Exception (Exception)
import Control.Monad.Except (MonadError (..))
import Control.Monad.Logic (MonadLogic (..))
import Control.Monad.Reader (MonadReader (..), ReaderT (..), asks)
import Control.Monad.State.Strict (MonadState (..), StateT (..), gets, modify')
import Data.Foldable (foldl', for_, toList)
import Data.Functor.Foldable (cata, project)
import qualified Data.Map.Strict as Map
import Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq
import Data.Traversable (for)
import Searchterm.Interface.Core (ClsName, Forall (Forall), Index (..), Inst (..), Strained (..), Tm (..), TmName, Ty,
                                 TyF (..), TyScheme (..), TyVar (..), tySchemeBody, Partial (..))
import Searchterm.Interface.Decl (Decl (..), DeclSet (..))
import Searchterm.Synth.Align (TyUnify, TyUniq (..), TyVert (..), mightAlign, recAlignTys)
import Searchterm.Synth.UnionMap (UnionMap)
import qualified Searchterm.Synth.UnionMap as UM
import Data.Tuple (swap)
import Searchterm.Synth.Monad (TrackSt (..), Track, runManyTrack)
import Searchterm.Interface.ParenPretty (prettyShow)
import Prettyprinter (Pretty (..))
import qualified Prettyprinter as P
import Control.Monad (when)
-- import qualified Debug.Trace as DT

-- | Trace a single message - swap definitions to turn on/off
traceM :: Applicative m => String -> m ()
-- traceM = DT.traceM
traceM _ = pure ()

-- | Trace entering and exiting a scope
traceScopeM :: (Monad m, Pretty a) => String -> m a -> m a
traceScopeM ctx act = do
  traceM ("Enter: " ++ ctx)
  a <- act
  traceM ("Exit: " ++ ctx ++ " with value: " ++ prettyShow a)
  pure a

-- | Tracing for search failures
traceEmptyM :: (Alternative m) => String -> m a
traceEmptyM msg = traceM ("Empty: " ++ msg) *> empty

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

-- | We need to make sure that traverse doesn't draw from the first element indefinitely, etc.
-- This uses fair bind (>>-) to re-implement traverse (slower but more fair).
fairTraverse :: MonadLogic m => (a -> m b) -> Seq a -> m (Seq b)
fairTraverse f = go Empty where
  go !acc = \case
    Empty -> pure acc
    a :<| as -> f a >>- \b -> go (acc :|> b) as

-- | The fair analog of 'traverse_'. This has different behavior than 'choose':
-- This will "sequentially" traverse all elements in "one" interleaving, whereas 'choose' will select
-- one per interleaving.
fairTraverse_ :: (MonadLogic m, Foldable f) => (a -> m ()) -> f a -> m ()
fairTraverse_ f = go . toList where
  go = \case
    [] -> pure ()
    a : as -> f a >>- \_ -> go as

-- boilerplate
toListWithIndex :: Seq a -> [(a, Index)]
toListWithIndex ss = zip (toList ss) (fmap Index [Seq.length ss - 1 .. 0])

-- | A unique binder for enumerated lambdas
newtype TmUniq = TmUniq { unTmUniq :: Int }
  deriving stock (Show)
  deriving newtype (Eq, Ord, Enum, Num, Pretty)

-- | A typeclass constraint for unification.
data StraintUniq = StraintUniq
  { suCls :: !ClsName
  , suArgs :: !(Seq TyUniq)
  } deriving stock (Eq, Ord, Show)

instance Pretty StraintUniq where
  pretty (StraintUniq cn ts) = P.hsep (pretty cn : fmap pretty (toList ts))

-- | Search will yield closed terms with globally unique binders
type TmFound = Tm TmUniq Index

-- | Type unification graph
type TyGraph = UnionMap TyUniq TyVert

-- | Local environment for search (usable with 'MonadReader')
data Env = Env
  { envDecls :: !DeclSet
  -- ^ Top-level declarations usable in term search
  , envCtx :: !(Seq TyUniq)
  -- ^ Local type constraints
  , envDepthLim :: !Int
  -- ^ Remaining depth for recursive search
  } deriving stock (Eq, Show)

-- | Global forward state for search (usable with 'MonadState')
data StFwd = StFwd
  { stFwdTySrc :: !TyUniq
  -- ^ Next available unique vertex id
  , stFwdTmSrc :: !TmUniq
  -- ^ Next available unique term var binder
  } deriving stock (Eq, Show)

type StBwd = TyGraph

type St = TrackSt StFwd StBwd

-- | Globally terminating errors for search (usable with 'MonadError')
newtype SearchErr =
    SearchErrMissingIndex Index
  -- ^ Indicates undefined index in a declaration (meaning decl is not actually closed!)
  -- This is a non-recoverable error - you need to fix the declarations.
  deriving stock (Eq, Show)

instance Exception SearchErr

-- | The core search monad. 'LogicT' is a list transformer monad with support for fair interleavings.
newtype SearchM a = SearchM { unSearchM :: Track Env StFwd StBwd SearchErr a }
  deriving newtype (Functor, Applicative, Monad, MonadReader Env, MonadState St, MonadError SearchErr, Alternative, MonadLogic)

-- | Use this to guard regions that will require some depth
guardDepth :: SearchM ()
guardDepth = do
  depthLim <- asks envDepthLim
  when (depthLim < 1) (traceEmptyM "Would hit depth limit")

-- | Use this to encapsulate regions with decreased depth
decDepth :: SearchM a -> SearchM a
decDepth act = do
  depthLim <- asks envDepthLim
  if depthLim <= 0
    then traceEmptyM "Hit depth limit"
    else local (\env -> env { envDepthLim = envDepthLim env - 1 }) act

-- | Lookup a bound type variable during type insertion
lookupCtx :: (MonadError SearchErr m, MonadReader (Seq x) m) => Index -> m x
lookupCtx i = do
  xs <- ask
  let j = Seq.length xs - unIndex i - 1
  case Seq.lookup j xs of
    Nothing -> throwError (SearchErrMissingIndex i)
    Just x -> pure x

-- | Instantiate the type variables and return their ids.
insertTyVars :: MonadState St m => (TyVar -> TyVert) -> Seq TyVar -> m (Seq TyUniq)
insertTyVars onVar tvs = res where
  insertRaw v (TrackSt (StFwd srcx zz) umx) = (srcx, TrackSt (StFwd (srcx + 1) zz) (UM.insert srcx v umx))
  acc (stx, ctxx) tv = let (u, sty) = insertRaw (onVar tv) stx in (sty, ctxx :|> u)
  res = state (\stStart -> swap (foldl' acc (stStart, Seq.empty) tvs))

insertStraint :: (MonadError SearchErr m, MonadState St m) => Seq TyUniq -> Inst Index -> m StraintUniq
insertStraint ctx (Inst cn tys) =  do
  us <- traverse (fmap fst . insertTy ctx) tys
  pure (StraintUniq cn us)

-- | Instantiate the type variables in the scheme with the given strategy, bind them in the local
-- typing context, then insert the type into the union map. The strategy is used to instantiate with skolem vars
-- (non-unifiable / "externally-chosen" vars) at the top level or simple meta vars (plain old unifiable vars) below.
-- NOTE: The constraints returned are not unified with instance derivations. You have to do that after calling this.
insertScheme :: (MonadError SearchErr m, MonadState St m) => (TyVar -> TyVert) -> TyScheme Index -> m (Seq StraintUniq, TyUniq, TyUnify)
insertScheme onVar (TyScheme (Forall tvs (Strained cons ty))) = res where
  res = do
    ctx <- insertTyVars onVar tvs
    ius <- for cons (insertStraint ctx)
    (u, v) <- insertTy ctx ty
    pure (ius, u, v)

-- | Used in 'insertScheme' to do the type insertion.
insertTy :: (MonadError SearchErr m, MonadState St m) => Seq TyUniq -> Ty Index -> m (TyUniq, TyUnify)
insertTy ctx ty = res where
  insertRaw v (TrackSt (StFwd srcx zz) umx) = (srcx, TrackSt (StFwd (srcx + 1) zz) (UM.insert srcx v umx))
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
insertMetaScheme :: TyScheme Index -> SearchM (Seq StraintUniq, TyUniq, TyUnify)
insertMetaScheme = insertScheme TyVertMeta

-- | Allocate a fresh term binder
freshTmBinder :: SearchM TmUniq
freshTmBinder = state $ \st ->
  let fwd = tsFwd st
      s = stFwdTmSrc fwd
  in (s, st { tsFwd = fwd { stFwdTmSrc = s + 1 } })

-- | Yield the merged vertex id if the given vertex aligns with the current goal,
-- otherwise yield nothing.
tryAlignTy :: TyUniq -> TyUniq -> SearchM TyUniq
tryAlignTy goalKey candKey = do
  tyMap <- gets tsBwd
  case recAlignTys goalKey candKey tyMap of
    Left e -> traceEmptyM ("Failed to align: " ++ show e)
    Right (u, tyMap') -> do
      modify' (\st -> st { tsBwd = tyMap' })
      pure u

-- | Find solutions by looking in the context for vars that match exactly.
ctxFits :: TyUniq -> SearchM TmFound
ctxFits goalKey = traceScopeM "Ctx fit" $ do
  ctx <- asks envCtx
  choose (toListWithIndex ctx) $ \(candKey, idx) -> do
    _ <- tryAlignTy goalKey candKey
    pure (TmFree idx)

-- | If the goal is a type vertex (not a meta/skolem vertex), yield it.
lookupGoal :: TyUniq -> SearchM (TyUniq, TyUnify)
lookupGoal goalKey = do
  um <- gets tsBwd
  let (mp, um') = UM.find goalKey um
  case mp of
    Nothing -> traceEmptyM ("Goal not found: " ++ show goalKey)
    Just (newKey, vert) -> do
      modify' (\st -> st { tsBwd = um' })
      case vert of
        TyVertNode val -> pure (newKey, val)
        _ -> traceEmptyM ("Goal not node: " ++ show goalKey ++ " " ++ show vert)

-- | Find solutions by instantiating decls and matching the goal exactly.
exactDeclFits :: TyUniq -> SearchM TmFound
exactDeclFits goalKey = traceScopeM "Exact decl fit" $ do
  decls <- asks envDecls
  (_, goalVal) <- lookupGoal goalKey
  traceM ("Exact decl goal val: " ++ prettyShow goalVal)
  choose (Map.toList (dsMap decls)) $ \(name, decl) -> do
    -- Do a cheap check on the outside to see if it might align
    let candVal = project (tySchemeBody (declType decl))
    whenAlt (mightAlign goalVal candVal) $ do
      -- Ok, it might align. Add the type to the local search env and see if it really does.
      (candStraints, candKey, _) <- insertMetaScheme (declType decl)
      for_ candStraints topUnifyStraint
      _ <- tryAlignTy goalKey candKey
      pure (TmKnown name)

-- | Find solutions to function-type goals by adding args to context and searching with the result type.
funIntroFits :: TyUniq -> SearchM TmFound
funIntroFits goalKey = traceScopeM "Fun intro fit" $ do
  guardDepth
  (_, goalVal) <- lookupGoal goalKey
  case goalVal of
    TyFunF argKey retKey -> do
      -- If the goal looks like a function, try adding the arg to the context and searching for a match.
      retTm <- local (\env -> env { envCtx = envCtx env :|> argKey }) (recSearchUniq retKey)
      -- Success - allocate a fresh binder and return a lambda.
      b <- freshTmBinder
      pure (TmLam b retTm)
    _ -> traceEmptyM "Non fun in fun intro"

-- | Helper to return a multi-arg application
mkApp :: TmName -> Seq TmFound -> TmFound
mkApp n = go (TmKnown n) where
  go !t = \case
    Empty -> t
    s :<| ss -> go (TmApp t s) ss

-- | Inserts a partial application into the graph.
-- Returns
--   constraints
--   additional context for the function type (argument type vars)
--   the key for the function
--   the type of the function
insertPartial :: TyScheme Index -> Partial Index -> SearchM (Seq StraintUniq, Seq TyUniq, TyUniq, TyUnify)
insertPartial (TyScheme (Forall tvs (Strained cons _))) (Partial args retTy) = do
  ctx <- insertTyVars TyVertMeta tvs
  ius <- traverse (insertStraint ctx) cons
  addlCtx <- traverse (fmap fst . insertTy ctx) args
  (funcKey, funcVal) <- insertTy ctx retTy
  pure (ius, addlCtx, funcKey, funcVal)

-- | Find solutions by instantiating decl functions with matching return type
funElimFits :: TyUniq -> SearchM TmFound
funElimFits goalKey = traceScopeM "Fun elim fit" $ do
  guardDepth
  decls <- asks envDecls
  (_, goalVal) <- lookupGoal goalKey
  choose (Map.toList (dsMap decls)) $ \(name, decl) -> do
    -- Partials are defined for any curried lambda - args will be nonempty
    choose (declPartials decl) $ \part@(Partial _ retTy) -> do
      let candVal = project retTy
      -- Do a cheap check for possible alignment on the result type
      whenAlt (mightAlign goalVal candVal) $ do
        traceM ("Possible partial align: " ++ prettyShow part)
        -- Now really check that the result type unifies:
        -- Insert the partial to get vars for args and returned function
        (straints, addlCtx, candKey, _) <- insertPartial (declType decl) part
        -- Unify the returned function with the goal (first, to help constraint search)
        _ <- tryAlignTy goalKey candKey
        -- Unify constraints (second, to help argument search)
        _ <- fairTraverse topUnifyStraint straints
        -- It unifies. Now we know that if we can find args we can satsify the goal.
        argTms <- fairTraverse recSearchUniq addlCtx
        pure (mkApp name argTms)
        -- TODO Technically we don't have to find a term for each arg independently,
        -- or even left to right. However, we need to tame the explosion of cases somehow.
        -- Here we just find them independently nest them in an application. We could have
        -- introduced lets with a bound application at then end (in ANF).
        -- Maybe it is worth trying all possibilities but guarding with logict's 'once'.

-- | Search for a term matching the current goal type using a number of interleaved strategies.
topSearchUniq :: TyUniq -> SearchM TmFound
topSearchUniq goalKey = res where
  -- TODO add the following search strategies:
  -- * Given function in context, see if the result of the function helps you solve the goal.
  --   If so, search for the arg of the function and return the application. (coq apply?)
  -- * Case split on all constructors of a datatype. (coq destruct?)
  -- Really, just look up the standard coq tactics and do what they do.
  fits = [ctxFits goalKey, exactDeclFits goalKey, funIntroFits goalKey, funElimFits goalKey]
  res = traceScopeM ("Key search: " ++ show (unTyUniq goalKey)) (interleaveAll fits)

recSearchUniq :: TyUniq -> SearchM TmFound
recSearchUniq = decDepth . topSearchUniq

topUnifyStraint :: StraintUniq -> SearchM StraintUniq
topUnifyStraint su@(StraintUniq cn _ts) = traceScopeM ("Straint unify: " ++ prettyShow su) $ do
  deps <- asks (dsDeps . envDecls)
  case Map.lookup cn deps of
    Nothing -> traceEmptyM ("No instances for " ++ prettyShow cn)
    Just xs -> do
      choose xs $ \_is -> do
        -- TODO insert the scheme and try to unify!
        us <- fairTraverse (uncurry tryAlignTy) (error "TODO")
        pure (StraintUniq cn us)

recUnifyStraint :: StraintUniq -> SearchM StraintUniq
recUnifyStraint = decDepth . topUnifyStraint

-- | Outermost search interface: Insert the given scheme and search for terms matching it.
searchScheme :: TyScheme Index -> SearchM TmFound
searchScheme scheme = traceScopeM ("Scheme search: " ++ prettyShow scheme) $ do
  (goalStraints, goalKey, _) <- insertScheme TyVertSkolem scheme
  for_ goalStraints topUnifyStraint
  topSearchUniq goalKey

-- | General search parameters
data SearchConfig = SearchConfig
  { scDecls :: !DeclSet
  -- ^ Top-level declarations
  , scTarget :: !(TyScheme Index)
  -- ^ Search goal
  , scDepthLim :: !Int
  -- ^ Recursion depth limit
  } deriving stock (Eq, Show)

-- | Initialize the search environment
initEnvSt :: SearchConfig -> (Env, St)
initEnvSt (SearchConfig decls _ depthLim) =
  let env = Env decls Seq.empty depthLim
      st = TrackSt (StFwd 0 0) UM.empty
  in (env, st)

-- | A "suspended" search for incremental consumption
data SearchSusp a = SearchSusp
  { ssEnv :: !Env
  , ssSt :: !St
  , ssAct :: !(SearchM a)
  }

-- | Yield the next result from a suspended search
-- This is probably going to perform worse than just using 'runSearchN' -
-- 'msplit' is reportedly slow.
nextSearchResult :: SearchSusp a -> Either SearchErr (Maybe (a, SearchSusp a))
nextSearchResult (SearchSusp env st act) =
  let ea = runManyTrack 1 (unSearchM (msplit act)) env st
  in flip fmap ea $ \(xs, st') ->
    case xs of
      [] -> Nothing
      x:_ ->
        case x of
          Nothing -> Nothing
          Just (a, act') -> Just (a, SearchSusp env st' act')

-- | Take a given number of search results
takeSearchResults :: SearchSusp a -> Int -> ([a], Either SearchErr (Maybe (SearchSusp a)))
takeSearchResults = go [] where
  go !xs !susp !i =
    if i == 0
      then (reverse xs, Right (Just susp))
      else case nextSearchResult susp of
          Left err -> (reverse xs, Left err)
          Right mp -> case mp of
            Nothing -> (reverse xs, Right Nothing)
            Just (a, susp') -> go (a:xs) susp' (i - 1)

-- | Search for terms of the goal type with incremental consumption.
runSearchSusp :: SearchConfig -> SearchSusp TmFound
runSearchSusp sc =
  let (env, st) = initEnvSt sc
  in SearchSusp env st (searchScheme (scTarget sc))

-- | Search for up to N terms of the goal type.
runSearchN :: SearchConfig -> Int -> Either SearchErr [TmFound]
runSearchN sc n =
  let (env, st) = initEnvSt sc
  in fmap fst (runManyTrack n (unSearchM (searchScheme (scTarget sc))) env st)
