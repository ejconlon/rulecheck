{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Rulecheck.Synth where

import Control.Exception (Exception)
import Control.Monad (foldM, when)
import Control.Monad.Except (Except, MonadError (..), runExcept)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (MonadReader (..), ReaderT (..), asks)
import Control.Monad.State.Strict (MonadState (..), StateT (..))
import Data.Foldable (foldl', for_, toList)
import Data.Functor.Foldable (cata, project)
import Data.Functor.Foldable.TH (makeBaseFunctor)
import Data.IORef (modifyIORef', newIORef, readIORef, writeIORef)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq
import Data.String (IsString)
import Data.Text (Text)
import qualified IntLike.Map as ILM
import Rulecheck.UnionFind (MergeRes (..))
import Rulecheck.UnionMap (UnionMap)
import qualified Rulecheck.UnionMap as UM

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
data AlignTyErr =
    AlignTyErrConHead !TyName !TyName
  | AlignTyErrConArity !TyName !Int !Int
  | AlignTyErrMismatch
  deriving stock (Eq, Ord, Show)

instance Exception AlignTyErr

-- | Align (match) two types by lining up all the holes
alignTys :: TyF x a -> TyF y b -> Either AlignTyErr (TyF (x, y) (a, b))
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
            else Left (AlignTyErrConArity n la lb)
        else Left (AlignTyErrConHead n m)
    (TyFunF q1 r1, TyFunF q2 r2) -> Right (TyFunF (q1, q2) (r1, r2))
    _ -> Left AlignTyErrMismatch

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
type TmUnify = TmF Index TyUniq

-- saves me from deriving bitraverse?
bitraverseTyF :: Applicative m => (w -> m v) -> TyF w w -> m (TyF v v)
bitraverseTyF f = \case
  TyFreeF w -> fmap TyFreeF (f w)
  TyConF tn ws -> fmap (TyConF tn) (traverse f ws)
  TyFunF wl wr -> TyFunF <$> f wl <*> f wr

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
    [ ("myZero", Scheme Empty tyInt, Just (TmKnown "0"))
    , ("myOne", Scheme Empty tyInt, Just (TmKnown "1"))
    , ("myPlus", Scheme Empty tyIntFun2, Just (TmLam "a" (TmLam "b" (TmApp (TmApp (TmKnown "+") tmA) tmB))))
    ]

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
  , stTyReps :: !(Map TyUniq TmUniq)
  , stTmSrc :: !TmUniq
  , stTms :: !(Map TmUniq TmUnify)
  } deriving stock (Eq, Show)

newtype SearchM a = SearchM { unSearchM :: ReaderT Env (StateT St IO) a }
  deriving newtype (Functor, Applicative, Monad, MonadReader Env, MonadState St, MonadIO)

runSearchM :: SearchM a -> Env -> St -> IO (a, St)
runSearchM m env = runStateT (runReaderT (unSearchM m) env)

data AlignErr =
    AlignErrMissing !TyUniq
  | AlignErrEmbed !AlignTyErr
  | AlignErrSkol !TyVar
  deriving stock (Eq, Ord, Show)

instance Exception AlignErr

type AlignState = UnionMap TyUniq TyVert

newtype AlignM a = AlignM { unAlignM :: StateT AlignState (Except AlignErr) a }
  deriving newtype (Functor, Applicative, Monad, MonadError AlignErr, MonadState AlignState)

runAlignM :: AlignM a -> AlignState -> Either AlignErr (a, AlignState)
runAlignM m s = runExcept (runStateT (unAlignM m) s)

alignVertsM :: TyVert -> TyVert -> AlignM TyVert
alignVertsM vl vr = do
  case (vl, vr) of
    (TyVertMeta _, _) -> pure vr
    (_, TyVertMeta _) -> pure vl
    (TyVertSkolem tv, _) -> throwError (AlignErrSkol tv)
    (_, TyVertSkolem tv) -> throwError (AlignErrSkol tv)
    (TyVertGround tl, TyVertGround tr) -> do
      case alignTys tl tr of
        Left err -> throwError (AlignErrEmbed err)
        Right tw -> fmap TyVertGround (bitraverseTyF (uncurry alignUniqM) tw)

alignUniqM :: TyUniq -> TyUniq -> AlignM TyUniq
alignUniqM ul ur = do
  um <- get
  (res, um') <- UM.merge alignVertsM ul ur um
  put um'
  case res of
    MergeResMissing u -> throwError (AlignErrMissing u)
    MergeResUnchanged u -> pure u
    MergeResChanged u _ -> pure u

recAlignTys :: TyUniq -> TyUniq -> UnionMap TyUniq TyVert -> Either AlignErr (TyUniq, UnionMap TyUniq TyVert)
recAlignTys ul0 ur0 = runAlignM (alignUniqM ul0 ur0)

toListWithIndex :: Seq a -> [(a, Index)]
toListWithIndex ss = zip (toList ss) (fmap Index [Seq.length ss - 1 .. 0])

innerSearch :: (Env -> SearchM ()) -> (TmUniq -> SearchM ()) -> SearchM ()
innerSearch _nominate _answer = do
  Env decls ctx _ _ goalKey goalVal depthLim <- ask
  -- Go through all variables in context to find something that might fit the hole
  for_ (toListWithIndex ctx) $ \(candKey, _idx) -> do
    um <- asks envTys
    case recAlignTys goalKey candKey um of
      Left _ -> pure ()
      Right (_u, _) -> do
        -- TODO fill in var
        error "TODO"
  -- Go through all known decls to find something that might fit this hole exactly
  for_ (Map.toList decls) $ \(_name, decl) ->
    -- Only consider those that will fit our remaining depth
    when (declDepth decl <= depthLim) $ do
      -- Do a cheap check on the outside to see if it might align
      let candVal = project (schemeBody (declScheme decl))
      when (mightAlign goalVal candVal) $ do
        liftIO (putStrLn ("Might align: " ++ show candVal))  -- XXX
        insertingTy (declScheme decl) $ \candKey -> do
          um <- asks envTys
          case recAlignTys goalKey candKey um of
            Left _ -> pure ()
            Right (_u, _um') -> do
              -- * add term to map
              -- * if parents is empty (this is top), emit an answer
              --   this answer will need to be checked later
              --   because we'll only know if search successfully filled out all subterms
              -- * lookup the ty and see if it has a term in the map, otherwise
              --   nominate an env (with appropriate goals, depth, and parents)
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
  ((k, v), (m', src')) <- runReaderStateM ctx msrc $ flip cata ty $ \case
    TyFreeF i -> do
      u <- lookupCtx i
      let v = TyFreeF u
      (m, src) <- get
      put (ILM.insert src (TyVertGround v) m, src + 1)
      pure (src, v)
    TyConF tn ps -> do
      us <- fmap (fmap fst) (sequence ps)
      let v = TyConF tn us
      (m, src) <- get
      put (ILM.insert src (TyVertGround v) m, src + 1)
      pure (src, v)
    TyFunF am bm -> do
      au <- fmap fst am
      bu <- fmap fst bm
      let v = TyFunF au bu
      (m, src) <- get
      put (ILM.insert src (TyVertGround v) m, src + 1)
      pure (src, v)
  let env = Env decls Seq.empty (UM.fromMap m') [] k v depthLim
      st = St src' Map.empty 0 Map.empty
  pure (env, st)

outerSearch :: Map TmName Decl -> Scheme Index -> Int -> IO ([TmUniq], Map TmUniq TmUnify)
outerSearch decls scheme depthLim = go where
  go = do
    (env, st) <- initEnvSt decls scheme depthLim
    nomRef <- newIORef ([] :: [Env])
    ansRef <- newIORef ([] :: [TmUniq])
    stRef <- newIORef st
    let m = innerSearch (nominate nomRef) (answer ansRef)
    _ <- loop nomRef stRef env m
    finals <- liftIO (readIORef ansRef)
    meanings <- fmap stTms (readIORef stRef)
    pure (finals, meanings)
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

exampleSearch :: IO ([TmUniq], Map TmUniq TmUnify)
exampleSearch = do
  let scheme = Scheme mempty (TyCon "Int" mempty)
  decls <- either (\p -> fail ("Decl err: " ++ show p)) pure exampleDecls
  outerSearch decls scheme 5
