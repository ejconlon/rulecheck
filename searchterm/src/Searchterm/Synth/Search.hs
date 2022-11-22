{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

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
import Data.Foldable (foldl', toList, asum)
import Data.Functor.Foldable (cata, project)
import qualified Data.Map.Strict as Map
import Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq
import Data.Traversable (for)
import Searchterm.Interface.Core (ClsName, Forall (Forall), Index (..), Inst (..), Strained (..), Tm (..), Ty,
                                 TyF (..), TyScheme (..), TyVar (..), tySchemeBody, Partial (..), InstScheme (..), PatPair (..), ConPat (..), Pat (..))
import Searchterm.Interface.Decl (Decl (..), DeclSet (..), ConSig (..))
import Searchterm.Synth.Align (TyUnify, TyUniq (..), TyVert (..), mightAlign, recAlignTys)
import Searchterm.Synth.UnionMap (UnionMap)
import qualified Searchterm.Synth.UnionMap as UM
import Data.Tuple (swap)
import Searchterm.Synth.Monad (TrackSt (..), Track, runManyTrack)
import Searchterm.Interface.ParenPretty (prettyShow)
import Prettyprinter (Pretty (..))
import qualified Prettyprinter as P
import Control.Monad (unless, void)
import Searchterm.Interface.Names (unsafeIndexSeqWith)
-- import qualified Debug.Trace as DT
-- import Text.Pretty.Simple (pShow)
-- import qualified Data.Text as T
-- import qualified Data.Text.Lazy as TL

-- | Trace a single message - swap definitions to turn on/off
traceM :: Applicative m => String -> m ()
-- traceM = DT.traceM
traceM _ = pure ()

-- | Trace entering and exiting a scope
traceScopeM :: (MonadLogic m, Pretty a) => String -> m a -> m a
-- traceScopeM ctx act = do
--   traceM ("Enter: " ++ ctx)
--   ifte act
--     (\a -> a <$ traceM ("Exit: " ++ ctx ++ " with value: " ++ prettyShow a))
--     (traceM ("Exit: " ++ ctx ++ " (empty)") *> empty)
traceScopeM _ = id

-- | Tracing for search failures
traceEmptyM :: (Alternative m) => String -> m a
-- traceEmptyM msg = traceM ("Empty: " ++ msg) *> empty
traceEmptyM _ = empty

-- | Log on success/failure
traceTryM :: MonadLogic m => String -> m a -> m a
-- traceTryM ctx act = ifte act
--   (\a -> a <$ traceM ("Try succeeded: " ++ ctx) )
--   (traceM ("Try failed: " ++ ctx) *> empty)
traceTryM _ = id

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
toListWithIndex ss =
  let len = Seq.length ss
  in zip (toList ss) (fmap (\i -> Index (len - i - 1)) [0..])

-- | A unique binder for enumerated lambdas
newtype TmUniq = TmUniq { unTmUniq :: Int }
  deriving stock (Show)
  deriving newtype (Eq, Ord, Enum, Num)

instance Pretty TmUniq where
  pretty (TmUniq i) = "?tmu@" <> pretty i

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
  , envCtx :: !(Seq (TmUniq, TyUniq))
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

newtype StBwd = StBwd
  { stBwdTyGraph :: TyGraph
  -- ^ The type unification graph
  } deriving stock (Eq, Show)

type St = TrackSt StFwd StBwd

-- | Globally terminating errors for search (usable with 'MonadError')
data SearchErr =
    SearchErrMissingIndex !Index
    -- ^ Indicates undefined index in a declaration (meaning decl is not actually closed!)
    -- This is a non-recoverable error - you need to fix the declarations or fix the insertion code.
  | SearchErrBadConstraintName !ClsName !ClsName
    -- ^ Indicates inconsistent derivations in the decl set (inst name and class name mismatch).
    -- This is a non-recoverable error - you need to fix the declarations.
  | SearchErrBadConstraintLen !ClsName !Int !Int
    -- ^ Indicates inconsistent derivations in the decl set (inst args and class args len mismatch).
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
  unless (depthLim >= 1) (traceEmptyM "Would hit depth limit")

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
  insertRaw v (TrackSt (StFwd srcx zz) (StBwd umx)) =
    (srcx, TrackSt (StFwd (srcx + 1) zz) (StBwd (UM.insert srcx v umx)))
  acc (stx, ctxx) tv = let (u, sty) = insertRaw (onVar tv) stx in (sty, ctxx :|> u)
  res = state (\stStart -> swap (foldl' acc (stStart, Seq.empty) tvs))

-- | Insert the given constraint scheme with fresh type metavariables.
-- Returns inserted (dependent constraints, given constraint)
insertStraintScheme :: (MonadError SearchErr m, MonadState St m) => InstScheme Index -> m (Seq StraintUniq, StraintUniq)
insertStraintScheme (InstScheme (Forall tvs (Strained cons inst))) = do
  ctx <- insertTyVars TyVertMeta tvs
  ius <- for cons (insertStraint ctx)
  iu <- insertStraint ctx inst
  pure (ius, iu)

-- | Insert the given constraint with the given type variable context.
insertStraint :: (MonadError SearchErr m, MonadState St m) => Seq TyUniq -> Inst Index -> m StraintUniq
insertStraint ctx _inst@(Inst cn tys) =  do
  -- traceM ("**** INSERT STRAINT")
  -- traceM ("Insert straint inst: " ++ prettyShow inst)
  -- traceM ("Insert straint ctx: " ++ show (fmap prettyShow (toList ctx)))
  -- traceM ("Insert straint ctx (show): " ++ show ctx)
  us <- traverse (fmap fst . insertTy ctx) tys
  let su = StraintUniq cn us
  -- traceM ("Inserted StraintUniq: " ++ prettyShow su)
  pure su

-- | Instantiate the type variables in the scheme with the given strategy, bind them in the local
-- typing context, then insert the type into the union map. The strategy is used to instantiate with skolem vars
-- (non-unifiable / "externally-chosen" vars) at the top level or simple meta vars (plain old unifiable vars) below.
-- NOTE: The constraints returned are not unified with instance derivations. You have to do that after calling this.
insertTyScheme :: (MonadError SearchErr m, MonadState St m) => (TyVar -> TyVert) -> TyScheme Index -> m (Seq StraintUniq, Seq TyUniq, TyUniq, TyUnify)
insertTyScheme onVar _ts@(TyScheme (Forall tvs (Strained cons ty))) = do
  -- traceM ("*** INSERT TY SCHEME")
  -- traceM ("Ty: " ++ prettyShow ts)
  ctx <- insertTyVars onVar tvs
  ius <- for cons (insertStraint ctx)
  (u, v) <- insertTy ctx ty
  pure (ius, ctx, u, v)

-- | Used in 'insertScheme' to do the type insertion.
insertTy :: (MonadError SearchErr m, MonadState St m) => Seq TyUniq -> Ty Index -> m (TyUniq, TyUnify)
insertTy ctx ty = res where
  insertRaw v (TrackSt (StFwd srcx zz) (StBwd umx)) =
    (srcx, TrackSt (StFwd (srcx + 1) zz) (StBwd (UM.insert srcx v umx)))
  insert v = fmap (,v) (state (insertRaw (TyVertNode v)))
  onTy = \case
    TyFreeF i -> do
      u <- lookupCtx i
      pure (u, TyFreeF u)
    TyConF tn ps -> do
      us <- fmap (fmap fst) (sequence ps)
      insert (TyConF tn us)
    TyFunF am bm -> do
      au <- fmap fst am
      bu <- fmap fst bm
      insert (TyFunF au bu)
  res = do
    -- traceM ("**** INSERT Ty")
    -- traceM ("Insert ty TY: " ++ prettyShow ty)
    -- traceM ("Insert ty TY (show): " ++ show ty)
    -- traceM ("Insert ty ctx: " ++ show (fmap prettyShow (toList ctx)))
    -- traceM ("Insert ty ctx (show): " ++ show ctx)
    stMid <- get
    let ea = runReaderStateT ctx stMid (cata onTy ty)
    case ea of
      Left err -> throwError err
      Right ((k, v), stEnd) -> do
        put stEnd
        -- traceM ("INSERTED TY: " ++ show k ++ " " ++ show v)
        pure (k, v)

-- | Instantiates the scheme with metavars
-- Returns
--  constraints
--  type var context
--  the instantiated type key
--  the instantiated type value
insertMetaScheme :: TyScheme Index -> SearchM (Seq StraintUniq, Seq TyUniq, TyUniq, TyUnify)
insertMetaScheme = insertTyScheme TyVertMeta

-- | Inserts a partial application into the graph.
-- Returns
--   constraints
--   additional context for the function type (argument type vars)
--   the key for the function
--   the type of the function
insertPartial :: TyScheme Index -> Partial Index -> SearchM (Seq StraintUniq, Seq TyUniq, TyUniq, TyUnify)
insertPartial _ts@(TyScheme (Forall tvs (Strained cons _))) (Partial args retTy) = do
  -- traceM ("**** INSERT PARTIAL")
  -- traceM ("TS: " ++ prettyShow ts)
  ctx <- insertTyVars TyVertMeta tvs
  -- traceM ("CTX: " ++ show ctx)
  ius <- traverse (insertStraint ctx) cons
  addlCtx <- traverse (fmap fst . insertTy ctx) args
  (funcKey, funcVal) <- insertTy ctx retTy
  pure (ius, addlCtx, funcKey, funcVal)

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
  tyGraph <- gets (stBwdTyGraph . tsBwd)
  case recAlignTys goalKey candKey tyGraph of
    Left e -> traceEmptyM ("Failed to align: " ++ show e)
    Right (u, tyGraph') -> do
      modify' (\st -> st { tsBwd = (tsBwd st) { stBwdTyGraph = tyGraph' } })
      pure u

-- | Find solutions by looking in the context for vars that match exactly.
ctxFits :: TyUniq -> SearchM TmFound
ctxFits goalKey = traceScopeM "Ctx fit" $ do
  ctx <- asks envCtx
  choose (toListWithIndex ctx) $ \((_, candKey), idx) -> do
    _ <- traceTryM ("Align ctx fit: " ++ show candKey) (tryAlignTy goalKey candKey)
    pure (TmFree idx)

-- | Find solutions by looking in the context for vars that match exactly.
litFits :: TyUniq -> SearchM TmFound
litFits goalKey = traceScopeM "Lit fit" $ do
  (_, goalVal) <- lookupGoal goalKey
  case goalVal of
    TyConF tyn xs | Seq.null xs -> do
      mvals <- asks (Map.lookup tyn . dsLits . envDecls)
      case mvals of
        Nothing -> empty
        Just vals -> asum (fmap (pure . TmLit) vals)
    _ -> empty

-- | If the goal is a type vertex (not a meta/skolem vertex), yield it.
lookupGoal :: TyUniq -> SearchM (TyUniq, TyUnify)
lookupGoal goalKey = do
  tyGraph <- gets (stBwdTyGraph . tsBwd)
  let (mp, tyGraph') = UM.find goalKey tyGraph
  case mp of
    Nothing -> traceEmptyM ("Goal not found: " ++ show goalKey)
    Just (newKey, vert) -> do
      modify' (\st -> st { tsBwd = (tsBwd st) { stBwdTyGraph = tyGraph' } })
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
      (candStraints, _, candKey, _) <- insertMetaScheme (declType decl)
      fairTraverse_ topUnifyStraint candStraints
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
      x <- freshTmBinder
      retTm <- local (\env -> env { envCtx = envCtx env :|> (x, argKey) }) (recSearchUniq retKey)
      -- Success - allocate a fresh binder and return a lambda.
      b <- freshTmBinder
      pure (TmLam b retTm)
    _ -> traceEmptyM "Non fun in fun intro"

-- | Helper to return a multi-arg application
mkApp :: TmFound -> Seq TmFound -> TmFound
mkApp = go where
  go !t = \case
    Empty -> t
    s :<| ss -> go (TmApp t s) ss

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
        ctx <- asks envCtx
        traceM ("Current ctx: " ++ show ctx)
        -- Now really check that the result type unifies:
        -- Insert the partial to get vars for args and returned function
        (straints, addlCtx, candKey, _) <- insertPartial (declType decl) part
        -- Unify the returned function with the goal (first, to help constraint search)
        _ <- traceTryM "Align partial" (tryAlignTy goalKey candKey)
        -- Unify constraints (second, to help argument search)
        fairTraverse_ topUnifyStraint straints
        -- It unifies. Now we know that if we can find args we can satsify the goal.
        searchLetApp (TmKnown name) addlCtx

-- | Search for an application of the given types.
-- We search left to right, adding arguments to the context (in let binds) in the hope that
-- it's possible to re-use some elements of the context to solve later args.
searchLetApp :: TmFound -> Seq TyUniq -> SearchM TmFound
searchLetApp fnTm us = go id Empty (toList us) where
  go !outFn !argNames = \case
    [] -> do
      ctx <- asks envCtx
      pure (outFn (mkApp fnTm (fmap (TmFree . unsafeIndexSeqWith (\x (b, _) -> b == x) ctx) argNames)))
    a:rest -> do
      traceM ("SLA search for " ++ show a)
      recSearchUniq a >>- \b -> do
        x <- freshTmBinder
        traceM ("SLA found " ++ show b ++ " as " ++ show x)
        local (\env -> env { envCtx = envCtx env :|> (x, a) }) (go (outFn . TmLet x b) (argNames :|> x) rest)

-- Generates a lambdacase expression
destructFits :: TyUniq -> SearchM TmFound
destructFits goalKey = traceScopeM "Destruct fit" $ do
  guardDepth
  (_, goalVal) <- lookupGoal goalKey
  case goalVal of
    -- Try to satisfy a function goal with data arg by destructing
    TyFunF argKey retKey -> do
      (_, argVal) <- lookupGoal argKey
      case argVal of
        -- Can only destruct type constructors
        TyConF tn _ -> do
          decls <- asks envDecls
          case Map.lookup tn (dsCons decls) of
            -- And can only destruct if we know the data constructors
            Just cons -> searchCase argKey retKey cons
            _ -> empty
        _ -> empty
    _ -> empty

-- | Search for patterns matching the given LHS/RHS with the given constructor
searchPatPair :: TyUniq -> TyUniq -> ConSig -> SearchM (PatPair TmUniq TmFound)
searchPatPair argKey retKey (ConSig nm ty bs) = do
  -- Insert the type of the LHS
  (_, lhsCtx, lhsKey, _) <- insertMetaScheme ty
  -- Unify the LHS with the arg
  _ <- tryAlignTy argKey lhsKey
  -- Create tm and ty binders for arguments
  newCtx <- for bs $ \b -> do
    x <- freshTmBinder
    (y, _) <- insertTy lhsCtx b
    pure (x, y)
  let tmBinders = fmap fst newCtx
  traceM ("Searching for pat pairs: " ++ show ty ++ " " ++ show newCtx)
  -- Add them to the context and search for body terms
  local (\env -> env { envCtx = envCtx env <> newCtx }) $ do
    ctx <- asks envCtx
    traceM ("ctx " ++ show ctx)
    flip fmap (recSearchUniq retKey) $ \body ->
      PatPair (Pat (ConPat nm tmBinders)) body

-- | Search for a lambda case term matching the given argKey -> retKey function type
-- using the given constructors.
searchCase :: TyUniq -> TyUniq -> Seq ConSig -> SearchM TmFound
searchCase argKey retKey cons = res where
  res = do
    -- Allocate fresh binder for lambda arg
    argBind <- freshTmBinder
    -- With the lambda arg in scope...
    local (\env -> env { envCtx = envCtx env :|> (argBind, argKey) }) $
      -- search for each of the branches
      go argBind Empty (toList cons)
  go argBind !pairs = \case
    [] ->
      -- found them all? return the lambda + case tm
      pure (TmLam argBind (TmCase (TmFree (Index 0)) pairs))
    con:rest -> do
      -- unify a pat pair for each con and fairly continue
      searchPatPair argKey retKey con >>- \p -> go argBind (pairs :|> p) rest

-- | Search for a term matching the current goal type using a number of interleaved strategies.
topSearchUniq :: TyUniq -> SearchM TmFound
topSearchUniq goalKey = res where
  fits =
    [ ctxFits goalKey
    -- ^ Solve with a variable in the context
    , exactDeclFits goalKey
    -- ^ Solve with a known term
    , funIntroFits goalKey
    -- ^ Solve a function goal by adding arg to context and searching
    -- for a term of the return type.
    , funElimFits goalKey
    -- ^ Solve by looking at partial applications of known terms s.t.
    -- the return type matches the goal, then search for args for the application.
    -- TODO Also look at partial applications of functions in the context.
    , destructFits goalKey
    -- ^ Solve a function goal by pattern matching on the argument type.
    , litFits goalKey
    -- ^ Solve a goal by emitting literals
    ]
  res = traceScopeM ("Key search: " ++ show (unTyUniq goalKey)) (interleaveAll fits)

recSearchUniq :: TyUniq -> SearchM TmFound
recSearchUniq = decDepth . topSearchUniq

topUnifyStraint :: StraintUniq -> SearchM ()
topUnifyStraint su@(StraintUniq cn ts) = traceScopeM ("Straint unify: " ++ prettyShow su) $ do
  deps <- asks (dsDeps . envDecls)
  case Map.lookup cn deps of
    Nothing -> traceEmptyM ("No instances for " ++ prettyShow cn)
    Just xs -> do
      choose xs $ \is -> do
        (pus, _su'@(StraintUniq cn' ts')) <- insertStraintScheme is
        unless (cn == cn') (throwError (SearchErrBadConstraintName cn cn'))
        let tsLen = Seq.length ts
            tsLen' = Seq.length ts'
        unless (tsLen == tsLen') (throwError (SearchErrBadConstraintLen cn tsLen tsLen'))
        -- graph <- gets tsBwd
        -- traceM ("Graph " ++ T.unpack (TL.toStrict (pShow graph)))
        -- traceM ("Root StrainUniq: " ++ prettyShow su)
        -- traceM ("Cand InstScheme: " ++ prettyShow is)
        -- traceM ("Cand StrainUniq: " ++ prettyShow su')
        -- traceM ("Trying to unify: " ++ show (fmap prettyShow ts) ++ " // " ++ show (fmap prettyShow ts'))
        -- Unify the child constraint
        fairTraverse_ (void . uncurry tryAlignTy) (Seq.zip ts ts')
        -- Unify each of the parent constraints too
        fairTraverse_ recUnifyStraint pus

recUnifyStraint :: StraintUniq -> SearchM ()
recUnifyStraint = decDepth . topUnifyStraint

-- | Outermost search interface: Insert the given scheme and search for terms matching it.
searchScheme :: TyScheme Index -> Bool -> SearchM TmFound
searchScheme scheme useSkolem = traceScopeM ("Scheme search: " ++ prettyShow scheme) $ do
  let mkVert = if useSkolem then TyVertSkolem else TyVertMeta
  (goalStraints, _, goalKey, _) <- insertTyScheme mkVert scheme
  fairTraverse_ topUnifyStraint goalStraints
  topSearchUniq goalKey

-- | General search parameters
data SearchConfig = SearchConfig
  { scDecls :: !DeclSet
  -- ^ Top-level declarations
  , scTarget :: !(TyScheme Index)
  -- ^ Search goal
  , scDepthLim :: !Int
  -- ^ Recursion depth limit
  , scUseSkolem :: !Bool
  -- ^ True if want to use skolem vars at top level
  } deriving stock (Eq, Show)

-- | Initialize the search environment
initEnvSt :: SearchConfig -> (Env, St, Bool)
initEnvSt (SearchConfig decls _ depthLim useSkolem) =
  let env = Env decls Seq.empty depthLim
      st = TrackSt (StFwd 0 0) (StBwd UM.empty)
  in (env, st, useSkolem)

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
  let (env, st, useSkolem) = initEnvSt sc
  in SearchSusp env st (searchScheme (scTarget sc) useSkolem)

-- | Search for up to N terms of the goal type.
runSearchN :: SearchConfig -> Int -> Either SearchErr [TmFound]
runSearchN sc n =
  let (env, st, useSkolem) = initEnvSt sc
  in fmap fst (runManyTrack n (unSearchM (searchScheme (scTarget sc) useSkolem)) env st)
