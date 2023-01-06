module Searchterm.Interface.Names
  ( toListWithIndex
  , indexSeqWith
  , indexSeq
  , unsafeIndexSeqWith
  , unsafeIndexSeq
  , lookupSeq
  , unsafeLookupSeq
  , NamedErr (..)
  , namedStrained
  , NamelessErr (..)
  , namelessStrained
  , namelessType
  , namelessInst
  , namelessTerm
  , namelessClosedTerm
  , AlphaTm (..)
  , mapAlphaTm
  , closeAlphaTm
  , AlphaTyScheme
  , closeAlphaTyScheme
  , renameTerm
  ) where

import Control.Exception (Exception)
import Control.Monad (void)
import Control.Monad.Except (Except, MonadError (..), runExcept)
import Control.Monad.Reader (MonadReader (..), ReaderT (..), local)
import Control.Monad.State.Strict (MonadState (..), State, modify', runState)
import Data.Foldable (toList)
import Data.Functor.Foldable (cata)
import Data.Maybe (fromMaybe)
import Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq
import Data.Typeable (Typeable)
import GHC.Stack (HasCallStack)
import Searchterm.Interface.Core (ConPat (..), Forall (..), Index (..), Inst (..), InstScheme, Pat (..),
                                  PatPair (..), Strained (..), Tm (..), TmF (..), TmName (..), Ty, TyScheme, TyVar)

toListWithIndex :: Seq a -> [(a, Index)]
toListWithIndex ss =
  let len = Seq.length ss
  in zip (toList ss) (fmap (\i -> Index (len - i - 1)) [0..])

indexSeqWith :: (b -> a -> Bool) -> Seq a -> b -> Maybe Index
indexSeqWith f s a = fmap (\lvl -> Index (Seq.length s - lvl - 1)) (Seq.findIndexR (f a) s)

indexSeq :: Eq a => Seq a -> a -> Maybe Index
indexSeq = indexSeqWith (==)

unsafeIndexSeqWith :: HasCallStack => (b -> a -> Bool) -> Seq a -> b -> Index
unsafeIndexSeqWith f s a = fromMaybe (error "unsafeIndexSeqWith") (indexSeqWith f s a)

unsafeIndexSeq :: (HasCallStack, Eq a) => Seq a -> a -> Index
unsafeIndexSeq s a = fromMaybe (error "unsafeIndexSeq") (indexSeqWith (==) s a)

lookupSeq :: Seq a -> Index -> Maybe a
lookupSeq s (Index i) = Seq.lookup (Seq.length s - i - 1) s

unsafeLookupSeq :: HasCallStack => Seq a -> Index -> a
unsafeLookupSeq s ix = fromMaybe (error "unsafeLookupSeq") (lookupSeq s ix)

data NamedErr =
    NamedErrMissing !Index
  | NamedErrMismatch !Int !Int
  deriving stock (Eq, Ord, Show)

instance Exception NamedErr

namedStrained :: Seq w -> Forall v (Strained Index (Ty Index)) -> Either NamedErr (Forall w (Strained w (Ty w)))
namedStrained ws (Forall vs (Strained is ty)) = res where
  res =
    let wl = Seq.length ws
        vl = Seq.length vs
    in if wl /= vl
      then Left (NamedErrMismatch wl vl)
      else Forall ws <$> (Strained <$> traverse namedInst is <*> namedTy ty)
  namedInst (Inst cn tys) = Inst cn <$> traverse namedTy tys
  namedTy = traverse (\i -> maybe (Left (NamedErrMissing i)) Right (lookupSeq ws i))

newtype NamelessErr v = NamelessErrMissing (Seq v)
  deriving stock (Eq, Ord, Show)

instance (Show v, Typeable v) => Exception (NamelessErr v)

namelessStrained :: (Traversable f, Eq v) => Forall v (Strained v (f v)) -> Either (NamelessErr v) (Forall v (Strained Index (f Index)))
namelessStrained (Forall tvs x) = Forall tvs <$> bindStr x where
  bindStr (Strained cons fy) = Strained <$> traverse bindCon cons <*> traverse bind fy
  bind a = maybe (Left (NamelessErrMissing (Seq.singleton a))) Right (indexSeq tvs a)
  bindCon (Inst cn tys) = Inst cn <$> traverse (traverse bind) tys

namelessType :: TyScheme TyVar -> Either (NamelessErr TyVar) (TyScheme Index)
namelessType = namelessStrained

namelessInst :: InstScheme TyVar -> Either (NamelessErr TyVar) (InstScheme Index)
namelessInst = namelessStrained

type M b v a = ReaderT (Seq b) (State (Seq v)) a

runM :: M b v a -> Forall v a
runM m = let (a, s) = runState (runReaderT m Seq.empty) Seq.empty in Forall s a

indexM :: Eq v => (v -> b -> Bool) -> v -> M b v Index
indexM isBound v = do
  ctx <- ask
  case indexSeqWith isBound ctx v of
    Just ix -> pure ix
    Nothing -> do
      let bump (Index i) = Index (i + Seq.length ctx)
      xtra <- get
      case indexSeq xtra v of
        Just ix -> pure (bump ix)
        Nothing -> do
          modify' (v :<|)
          pure (bump (Index (Seq.length xtra) + 1))

nlTm :: Eq v => (v -> Maybe TmName) -> (v -> b -> Bool) -> TmF b v (M b v (Tm b Index)) -> M b v (Tm b Index)
nlTm isKnown isBound = \case
    TmFreeF a ->
      case isKnown a of
        Nothing -> TmFree <$> indexM isBound a
        Just n -> pure (TmKnown n)
    TmLitF l -> pure (TmLit l)
    TmKnownF n -> pure (TmKnown n)
    TmAppF mx my -> TmApp <$> mx <*> my
    TmLamF b mbody -> TmLam b <$> local (:|> b) mbody
    TmLetF b marg mbody -> TmLet b <$> marg <*> local (:|> b) mbody
    TmCaseF scrut pairs -> TmCase <$> scrut <*> traverse nlPat pairs

nlPat :: PatPair b (M b v (Tm b Index)) -> M b v (PatPair b (Tm b Index))
nlPat (PatPair p@(Pat (ConPat _ bs)) mtm) = PatPair p <$> local (<> bs) mtm

namelessTerm :: Eq v => (v -> Maybe TmName) -> Tm v v -> Forall v (Tm v Index)
namelessTerm isKnown = runM . cata (nlTm isKnown (==))

namelessClosedTerm :: Eq v => (v -> Maybe TmName) -> Tm v v -> Either (NamelessErr v) (Tm v Index)
namelessClosedTerm isKnown tm =
  let (Forall xtra tm') = namelessTerm isKnown tm
  in case xtra of
    Empty -> Right tm'
    _ -> Left (NamelessErrMissing xtra)

type N v a = ReaderT (Seq v) (Except NamedErr) a

runN :: N w a -> Either NamedErr a
runN = runExcept . flip runReaderT Empty

-- | Rename a closed de Bruijn indexed term
renameTerm :: (v -> w) -> Tm v Index -> Either NamedErr (Tm w w)
renameTerm f = runN . cata goTm where
  goTm = \case
    TmFreeF ix -> do
      s <- ask
      case lookupSeq s ix of
        Nothing -> throwError (NamedErrMissing ix)
        Just w -> pure (TmFree w)
    TmLitF l -> pure (TmLit l)
    TmKnownF n -> pure (TmKnown n)
    TmAppF mx my -> TmApp <$> mx <*> my
    TmLamF b mbody ->
      let c = f b
      in TmLam c <$> local (:|> c) mbody
    TmLetF b marg mbody ->
      let c = f b
      in TmLet c <$> marg <*> local (:|> c) mbody
    TmCaseF scrut pairs -> TmCase <$> scrut <*> traverse goPat pairs
  goPat (PatPair (Pat (ConPat tn bs)) mtm) =
    let cs = fmap f bs
    in PatPair (Pat (ConPat tn cs)) <$> local (<> cs) mtm

newtype AlphaTm = AlphaTm { unAlphaTm :: Tm () Index }
  deriving stock (Show)
  deriving newtype (Eq, Ord)

mapAlphaTm :: Tm b Index -> AlphaTm
mapAlphaTm = AlphaTm . cata goTm where
  goTm = \case
    TmFreeF i -> TmFree i
    TmLitF l -> TmLit l
    TmKnownF n -> TmKnown n
    TmAppF l r -> TmApp l r
    TmLamF _ x -> TmLam () x
    TmLetF _ arg body -> TmLet () arg body
    TmCaseF scrut pairs -> TmCase scrut (fmap goPat pairs)
  goPat (PatPair (Pat (ConPat cn bs)) tm) = PatPair (Pat (ConPat cn (void bs))) tm

closeAlphaTm :: Eq v => (v -> Maybe TmName) -> Tm v v -> Either (NamelessErr v) AlphaTm
closeAlphaTm isKnown = fmap mapAlphaTm . namelessClosedTerm isKnown

type AlphaTyScheme = Forall () (Strained Index (Ty Index))

closeAlphaTyScheme :: TyScheme TyVar -> Either (NamelessErr TyVar) AlphaTyScheme
closeAlphaTyScheme = fmap forget . namelessType where
  forget (Forall bs st) = Forall (void bs) st
