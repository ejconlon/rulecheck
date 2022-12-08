{-# LANGUAGE OverloadedStrings #-}

-- | "Alignment" is one layer of unification.
module Searchterm.Synth.Align where

import Control.Exception (Exception)
import Control.Monad.Except (Except, MonadError (..), runExcept)
import Control.Monad.State.Strict (MonadState (..), StateT (..))
import Data.Either (isRight)
import qualified Data.Sequence as Seq
import Prettyprinter (Pretty (..))
import Searchterm.Interface.Core (ConTy (..), TyF (..), TyName, TyVar, bitraverseTyF)
import Searchterm.Interface.ParenPretty (ParenPretty (..), parenAtom)
import Searchterm.Synth.UnionFind (MergeRes (..))
import Searchterm.Synth.UnionMap (UnionMap)
import qualified Searchterm.Synth.UnionMap as UM
-- import Debug.Trace (traceM)

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
        ConTyFree _ -> Right (ConTyKnown na)
    ConTyFree va -> Right (fmap (va,) eb)

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
        else Left (AlignTyErrConArity (case hd of { ConTyKnown na -> Just na ; _ -> Nothing}) la lb)
    (TyFunF q1 r1, TyFunF q2 r2) -> Right (TyFunF (q1, q2) (r1, r2))
    _ -> Left AlignTyErrMismatch

-- | Do two types have the potential to align?
-- Note that this is *slightly* different than just checking if 'alignTys' yields
-- a 'Right' value - this is more permissive in that free vars don't have to exactly
-- line up, as prior unification steps will handle that.
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

data UnifyStyle =
    UnifyStyleMeta
  -- ^ Meta vars can unify with anything
  | UnifyStyleSkolem
  -- ^ But skolem vars can only unify with themselves
  -- Essentially, these are externally-chosen
  deriving stock (Eq, Ord, Show, Enum, Bounded)

data UnifyVar = UnifyVar
  { uvStyle :: !UnifyStyle
  -- ^ Unification style of this var
  , uvName :: !TyVar
  -- ^ Nice stringy var name for printing - not unique
  } deriving stock (Eq, Ord, Show)

-- | A vertex in the type unification graph
data TyVert =
    TyVertVar !UnifyVar
  -- ^ A var vertex
  | TyVertNode !TyUnify
  -- ^ A type vertex
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
  -- traceM $ "alignVertsM: " ++ show vl ++ " " ++ show vr
  case (vl, vr) of
    -- Meta vars always align
    (TyVertVar (UnifyVar UnifyStyleMeta _), _) -> pure vr
    (_, TyVertVar (UnifyVar UnifyStyleMeta _)) -> pure vl
    -- Skolem vars never align (will never be called on same id, so don't
    -- have to worry about skolem aligning with itself)
    (TyVertVar (UnifyVar UnifyStyleSkolem tv), _) -> throwError (AlignErrSkol tv)
    (_, TyVertVar (UnifyVar UnifyStyleSkolem tv)) -> throwError (AlignErrSkol tv)
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
  -- traceM $ "alignUniqM args: " ++ show ul ++ " " ++ show ur
  -- Merge them the union map by value
  res <- UM.stateMerge alignVertsM ul ur
  -- traceM $ "alignUniqM res: " ++ show res
  case res of
    -- Calling this with an unknown vertex id indicates a logic bug higher up
    MergeResMissing u -> error ("Missing vertex: " ++ show u)
    MergeResUnchanged u -> pure u
    MergeResChanged u _ -> pure u

-- | Align two vertices (by id) and yield the solution with updated graph
recAlignTys :: TyUniq -> TyUniq -> UnionMap TyUniq TyVert -> Either AlignErr (TyUniq, UnionMap TyUniq TyVert)
recAlignTys ul0 ur0 = runAlignM (alignUniqM ul0 ur0)
