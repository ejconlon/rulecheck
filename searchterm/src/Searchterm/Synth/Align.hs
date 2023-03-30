{-# LANGUAGE OverloadedStrings #-}

-- | "Alignment" is one layer of unification.
module Searchterm.Synth.Align where

import Control.Exception (Exception)
import Control.Monad.Except (Except, MonadError (..), runExcept)
import Control.Monad.State.Strict (MonadState (..), StateT (..))
import Data.Either (isRight)
import qualified Data.Sequence as Seq
import Prettyprinter (Pretty (..))
import Searchterm.Interface.Core (ConTy (..), TyF (..), TyName, TyVar, bitraverseTyF, KindAnno (..), Kind (..))
import Searchterm.Interface.ParenPretty (ParenPretty (..), parenAtom)
import Searchterm.Synth.UnionFind (MergeRes (..))
import Searchterm.Synth.UnionMap (UnionMap)
import qualified Searchterm.Synth.UnionMap as UM
import Data.Sequence (Seq(..))

-- | Something that can go wrong when aligning two types
data AlignTyErr =
    AlignTyErrConHead !TyName !TyName
  -- ^ They mismatch on constructor head
  | AlignTyErrConArity !(Maybe TyName) !Int !Int
  -- ^ They mismatch on constructor arity
  | AlignTyErrMismatch
  -- ^ They totally mismatch on type shape (e.g. fun vs con vs var)
  deriving stock (Eq, Ord, Show)

instance Exception AlignTyErr

-- | Align two ty con heads
alignConHead :: ConTy a -> ConTy b -> Either AlignTyErr (ConTy (a, b))
alignConHead ea eb =
  case ea of
    ConTyKnown na ->
      case eb of
        ConTyKnown nb -> if na == nb then Right (ConTyKnown na) else Left (AlignTyErrConHead na nb)
        -- ConTyFree _ -> Right (ConTyKnown na)
    -- ConTyFree va -> Right (fmap (va,) eb)

-- | Align (match) two types by lining up all the holes
alignTys :: TyF x a -> TyF y b -> Either AlignTyErr (TyF (x, y) (a, b))
alignTys one two =
  case (one, two) of
    (TyFreeF x, TyFreeF y) -> Right (TyFreeF (x, y))
    (TyConF n as, TyConF m bs) -> do
      hd <- alignConHead n m
      let la = Seq.length as
          lb = Seq.length bs
      if la == lb
        then Right (TyConF hd (Seq.zip as bs))
        else Left (AlignTyErrConArity (case hd of { ConTyKnown na -> Just na {-; ConTyFree _ -> Nothing-}}) la lb)
    (TyFunF q1 r1, TyFunF q2 r2) -> Right (TyFunF (q1, q2) (r1, r2))
    _ -> Left AlignTyErrMismatch

-- | Do two types have the potential to align?
-- It's useful to have a quick version of alignment that looks at less of a type
-- so we can detect guaranteed misalignments before doing expensive work.
mightAlign :: TyF x a -> TyF y b -> Bool
mightAlign one two =
  case (one, two) of
    (TyFreeF _, _) -> True
    (_, TyFreeF _) -> True
    (TyConF n as, TyConF m bs) -> isRight (alignConHead n m) && Seq.length as == Seq.length bs
    (TyFunF _ _, TyFunF _ _) -> True
    _ -> False

-- | A unique id for the vertices of our type unification graph
newtype TyUniq = TyUniq { unTyUniq :: Int }
  deriving stock (Show)
  deriving newtype (Eq, Ord, Enum, Num)

instance Pretty TyUniq where
  pretty (TyUniq i) = "?tyu@" <> pretty i

instance ParenPretty TyUniq where
  parenPretty _ = parenAtom

-- | Types in the unification graph have holes that point to other nodes in the graph
type TyUnify = TyF TyUniq TyUniq

-- | A vertex in the type unification graph
data TyVert =
    TyVertMeta !TyVar
  -- ^ A meta var vertex (unifies with everything)
  | TyVertSkolem !TyVar
  -- ^ A skolem var vertex (unifies with nothing - essentially externally-chosen)
  | TyVertNode !TyUnify
  -- ^ A vertex with some type structure
  deriving stock (Eq, Ord, Show)

-- | Something that can go wrong when aligning two vertices
data AlignErr =
    AlignErrEmbed !AlignTyErr
  -- ^ When types don't align
  | AlignErrSkol !TyVar
  -- ^ When trying to align with a skolem var
  | AlignErrKind !Kind !Kind
  -- ^ When kinds don't align
  deriving stock (Eq, Ord, Show)

instance Exception AlignErr

type AlignState = UnionMap TyUniq (KindAnno TyVert)

-- | The alignment monad - just keeps track of the type graph and lets us abort with error
newtype AlignM a = AlignM { unAlignM :: StateT AlignState (Except AlignErr) a }
  deriving newtype (Functor, Applicative, Monad, MonadError AlignErr, MonadState AlignState)

runAlignM :: AlignM a -> AlignState -> Either AlignErr (a, AlignState)
runAlignM m s = runExcept (runStateT (unAlignM m) s)

-- | Align two vertices (by value) (recursing as new constraints are uncovered)
-- This will never be called on two vertices with the same original id.
alignVertM :: TyVert -> TyVert -> AlignM TyVert
alignVertM vl vr = do
  case (vl, vr) of
    -- Meta vars always align
    (TyVertMeta _, _) -> pure vr
    (_, TyVertMeta _) -> pure vl
    -- Skolem vars never align (will never be called on same id, so don't
    -- have to worry about skolem aligning with itself)
    (TyVertSkolem tv, _) -> throwError (AlignErrSkol tv)
    (_, TyVertSkolem tv) -> throwError (AlignErrSkol tv)
    -- Types are more interesting
    (TyVertNode tl, TyVertNode tr) -> do
      case alignTys tl tr of
        -- They may not align at all, and that's ok - just abort
        Left err -> throwError (AlignErrEmbed err)
        -- When they do align, we may have additional constraints
        Right tw -> fmap TyVertNode (bitraverseTyF (uncurry alignUniqM) tw)

-- Stupid utility function to zip effectfully
seqZipWithM :: Applicative m => (a -> b -> m c) -> Seq a -> Seq b -> m (Seq c)
seqZipWithM f = go where
  go as bs =
    case (as, bs) of
      (Empty, _) -> pure Empty
      (_, Empty) -> pure Empty
      (a :<| as', b :<| bs') -> (:<|) <$> f a b <*> go as' bs'

-- | Aligns two kinds
alignKindM :: Kind -> Kind -> AlignM Kind
alignKindM k1 k2 =
  case (k1, k2) of
    (KindTy, KindTy) -> pure k1
    (KindTyCon x1, KindTyCon x2) | Seq.length x1 == Seq.length x2 ->
      fmap KindTyCon (seqZipWithM alignKindM x1 x2)
    _ -> throwError (AlignErrKind k1 k2)

-- | Aligns two kind-annotated vertices
alignAnnoM :: KindAnno TyVert -> KindAnno TyVert -> AlignM (KindAnno TyVert)
alignAnnoM (KindAnno v1 k1) (KindAnno v2 k2) = do
  k3 <- alignKindM k1 k2
  v3 <- alignVertM v1 v2
  pure (KindAnno v3 k3)

-- | Aligns two vertices (by id)
alignUniqM :: TyUniq -> TyUniq -> AlignM TyUniq
alignUniqM ul ur = do
  -- Merge them the union map by value
  res <- UM.stateMerge alignAnnoM ul ur
  case res of
    -- Calling this with an unknown vertex id indicates a logic bug higher up
    MergeResMissing u -> error ("Missing vertex: " ++ show u)
    MergeResUnchanged u -> pure u
    MergeResChanged u _ -> pure u

-- | Align two vertices (by id) and yield the solution with updated graph
recAlignTys :: TyUniq -> TyUniq -> UnionMap TyUniq (KindAnno TyVert) -> Either AlignErr (TyUniq, UnionMap TyUniq (KindAnno TyVert))
recAlignTys ul0 ur0 = runAlignM (alignUniqM ul0 ur0)
