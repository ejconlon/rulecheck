module Rulecheck.Synth.DepMap
  ( DepMap
  , dependsOn
  , revDependsOn
  , fromList
  , empty
  , track
  , complete
  , IncompleteErr (..)
  , completeRec
  ) where

import Control.Exception (Exception)
import Control.Monad (foldM)
import Data.Coerce (Coercible)
import Data.Foldable (foldl')
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Typeable (Typeable)
import qualified IntLike.Map as ILM
import IntLike.MultiMap (IntLikeMultiMap)
import qualified IntLike.MultiMap as ILMM
import IntLike.Set (IntLikeSet)
import qualified IntLike.Set as ILS

-- | Map of depends-on relation
data DepMap k v = DepMap
  { dependsOn :: IntLikeMultiMap k v
  , revDependsOn :: IntLikeMultiMap v k
  } deriving stock (Eq, Show)

fromList :: (Coercible k Int, Coercible v Int) => [(k, v)] -> DepMap k v
fromList = foldl' (flip (uncurry track)) empty

empty :: DepMap k v
empty = DepMap ILMM.empty ILMM.empty

-- | Declare that k depends-on v
track :: (Coercible k Int, Coercible v Int) => k -> v -> DepMap k v -> DepMap k v
track k v (DepMap fwd bwd) = DepMap (ILMM.insert k v fwd) (ILMM.insert v k bwd)

-- | Declare that v is done and emit any keys where all the dependencies are now finished
complete :: (Coercible k Int, Coercible v Int) => v -> DepMap k v -> ([k], DepMap k v)
complete v dm@(DepMap fwd bwd) = res where
  res = case ILM.lookup v bwd of
    Nothing -> ([], dm)
    Just ks ->
      let (out, fwd') = foldl' (uncurry deleting) ([], fwd) (ILS.toList ks)
          bwd' = ILM.delete v bwd
      in (out, DepMap fwd' bwd')
  deleting ys x k = case ILM.lookup k x of
    Nothing -> (ys, x)
    Just vs ->
      let vs' = ILS.delete v vs
      in if ILS.null vs'
        then (k:ys, ILM.delete k x)
        else (ys, ILM.insert k vs' x)

-- | Error for marking things complete which themselves have dependencies
data IncompleteErr k = IncompleteErr !k !(IntLikeSet k)
  deriving stock (Eq, Show)

instance (Show k, Typeable k) => Exception (IncompleteErr k)

-- | Mark an item complete and propagate recursively
completeRec :: (Coercible k Int) => k -> DepMap k k -> Either (IncompleteErr k) (Seq k, DepMap k k)
completeRec k dm@(DepMap fwd _) = res where
  res = case ILM.lookup k fwd of
    Just vs -> Left (IncompleteErr k vs)
    Nothing ->
      let (ks, dm') = complete k dm
      in foldM completing (Seq.fromList ks, dm') ks
  completing (ksx, dmx) kx =
    case completeRec kx dmx of
      Left err -> Left err
      Right (ksy, dmy) -> Right (ksx <> ksy, dmy)
