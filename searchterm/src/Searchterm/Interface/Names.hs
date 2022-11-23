module Searchterm.Interface.Names
  ( indexSeqWith
  , indexSeq
  , unsafeIndexSeqWith
  , unsafeIndexSeq
  , lookupSeq
  , unsafeLookupSeq
  , NamelessErr (..)
  , namelessType
  , namelessInst
  , namelessTerm
  , namelessClosedTerm
  , AlphaTm (..)
  , mapAlphaTm
  , closeAlphaTm
  , AlphaTyScheme
  , closeAlphaTyScheme
  ) where

import Control.Exception (Exception)
import Control.Monad.Reader (MonadReader (..), ReaderT (..), local)
import Control.Monad.State.Strict (MonadState (..), State, modify', runState)
import Data.Functor.Foldable (cata)
import Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq
import Data.Typeable (Typeable)
import Searchterm.Interface.Core (Forall (..), Index (..), Inst (..), InstScheme (..), Strained (..), Tm (..), TmF (..),
                                 TmName (..), TyScheme (..), TyVar, PatPair (..), Pat (..), ConPat (..), Ty)
import Control.Monad (void)
import Data.Maybe (fromMaybe)
import GHC.Stack (HasCallStack)
import Prettyprinter (Pretty)

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

newtype NamelessErr v = NamelessErrMissing (Seq v)
  deriving stock (Eq, Ord, Show)

instance (Show v, Typeable v) => Exception (NamelessErr v)

namelessStrained :: Traversable f => Forall TyVar (Strained TyVar (f TyVar)) -> Either (NamelessErr TyVar) (Forall TyVar (Strained Index (f Index)))
namelessStrained (Forall tvs x) = Forall tvs <$> bindStr x where
  bindStr (Strained cons fy) = Strained <$> traverse bindCon cons <*> traverse bind fy
  bind a = maybe (Left (NamelessErrMissing (Seq.singleton a))) Right (indexSeq tvs a)
  bindCon (Inst cn tys) = Inst cn <$> traverse (traverse bind) tys

namelessType :: TyScheme TyVar -> Either (NamelessErr TyVar) (TyScheme Index)
namelessType = fmap TyScheme . namelessStrained . unTyScheme

namelessInst :: InstScheme TyVar -> Either (NamelessErr TyVar) (InstScheme Index)
namelessInst = fmap InstScheme . namelessStrained . unInstScheme

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

newtype AlphaTm = AlphaTm { unAlphaTm :: Tm () Index }
  deriving stock (Show)
  deriving newtype (Eq, Ord, Pretty)

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

newtype AlphaTyScheme = AlphaTyScheme { unAlphaTyScheme :: Forall () (Strained Index (Ty Index)) }
  deriving stock (Show)
  deriving newtype (Eq, Ord, Pretty)

closeAlphaTyScheme :: TyScheme TyVar -> Either (NamelessErr TyVar) AlphaTyScheme
closeAlphaTyScheme = fmap forget . namelessType where
  forget (TyScheme (Forall bs st)) = AlphaTyScheme (Forall (void bs) st)
