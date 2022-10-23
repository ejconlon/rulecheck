-- | "Alignment" is one layer of unification.
module Rulecheck.Synth.Align where

import Control.Exception (Exception)
import Control.Monad.Except (Except, MonadError (..), runExcept)
import Control.Monad.State.Strict (MonadState (..), StateT (..))
import qualified Data.Sequence as Seq
import Rulecheck.Synth.Core (TyF (..), TyName, TyVar, bitraverseTyF)
import Rulecheck.Synth.UnionFind (MergeRes (..))
import Rulecheck.Synth.UnionMap (UnionMap)
import qualified Rulecheck.Synth.UnionMap as UM

-- | Something that can go wrong when aligning two types
data AlignTyErr =
    AlignTyErrConHead !TyName !TyName
  -- ^ They mismatch on constructor head
  | AlignTyErrConArity !TyName !Int !Int
  -- ^ They mismatch on constructor arity
  | AlignTyErrMismatch
  -- ^ They totally mismatch on type shape (e.g. fun vs con vs var)
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
-- It's useful to have a quick version of alignment that looks at less of a type
-- so we can detect guaranteed misalignments before doing expensive work.
mightAlign :: TyF x a -> TyF y b -> Bool
mightAlign one two =
  case (one, two) of
    (TyFreeF _, TyFreeF _) -> True
    (TyConF n as, TyConF m bs) -> n == m && Seq.length as == Seq.length bs
    (TyFunF _ _, TyFunF _ _) -> True
    _ -> False

-- | A unique id for the vertices of our type unification graph
newtype TyUniq = TyUniq { unTyUniq :: Int }
  deriving stock (Show)
  deriving newtype (Eq, Ord, Enum, Num)

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
  -- ^ When types don't align (normal error)
  | AlignErrSkol !TyVar
  -- ^ When trying to align with a skolem var (normal error)
  deriving stock (Eq, Ord, Show)

instance Exception AlignErr

type AlignState = UnionMap TyUniq TyVert

-- | The alignment monad - just keeps track of the type graph and lets us abort with error
newtype AlignM a = AlignM { unAlignM :: StateT AlignState (Except AlignErr) a }
  deriving newtype (Functor, Applicative, Monad, MonadError AlignErr, MonadState AlignState)

runAlignM :: AlignM a -> AlignState -> Either AlignErr (a, AlignState)
runAlignM m s = runExcept (runStateT (unAlignM m) s)

-- | Align two vertices (by value) (recursing as new constraints are uncovered)
-- This will never be called on two vertices with the same original id.
alignVertsM :: TyVert -> TyVert -> AlignM TyVert
alignVertsM vl vr = do
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

-- | Aligns two vertices (by id)
alignUniqM :: TyUniq -> TyUniq -> AlignM TyUniq
alignUniqM ul ur = do
  -- Merge them the union map by value
  res <- UM.stateMerge alignVertsM ul ur
  case res of
    -- Calling this with an unknown vertex id indicates a logic bug higher up
    MergeResMissing u -> error ("Missing vertex: " ++ show u)
    MergeResUnchanged u -> pure u
    MergeResChanged u _ -> pure u

-- | Align two vertices (by id) and yield the solution with updated graph
recAlignTys :: TyUniq -> TyUniq -> UnionMap TyUniq TyVert -> Either AlignErr (TyUniq, UnionMap TyUniq TyVert)
recAlignTys ul0 ur0 = runAlignM (alignUniqM ul0 ur0)
