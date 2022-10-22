module Rulecheck.Synth.Align where

import Control.Exception (Exception)
import Control.Monad.Except (Except, MonadError (..), runExcept)
import Control.Monad.State.Strict (MonadState (..), StateT (..))
import qualified Data.Sequence as Seq
import Rulecheck.Synth.Core (TyF (..), TyName, TyVar, bitraverseTyF)
import Rulecheck.Synth.UnionFind (MergeRes (..))
import Rulecheck.Synth.UnionMap (UnionMap)
import qualified Rulecheck.Synth.UnionMap as UM

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

newtype TyUniq = TyUniq { unTyUniq :: Int }
  deriving stock (Show)
  deriving newtype (Eq, Ord, Enum, Num)

type TyUnify = TyF TyUniq TyUniq

data TyVert =
    TyVertMeta !TyVar
  | TyVertSkolem !TyVar
  | TyVertNode !TyUnify
  deriving stock (Eq, Ord, Show)

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
    (TyVertNode tl, TyVertNode tr) -> do
      case alignTys tl tr of
        Left err -> throwError (AlignErrEmbed err)
        Right tw -> fmap TyVertNode (bitraverseTyF (uncurry alignUniqM) tw)

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
