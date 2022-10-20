module Rulecheck.DepMap
  ( DepMap
  , dependsOn
  , isDependencyOf
  , fromList
  , empty
  , track
  , complete
  ) where

import Data.Coerce (Coercible)
import Data.Foldable (foldl')
import qualified IntLike.Map as ILM
import IntLike.MultiMap (IntLikeMultiMap)
import qualified IntLike.MultiMap as ILMM
import qualified IntLike.Set as ILS

data DepMap k v = DepMap
  { dependsOn :: IntLikeMultiMap k v
  , isDependencyOf :: IntLikeMultiMap v k
  } deriving stock (Eq, Show)

fromList :: (Coercible k Int, Coercible v Int) => [(k, v)] -> DepMap k v
fromList = foldl' (flip (uncurry track)) empty

empty :: DepMap k v
empty = DepMap ILMM.empty ILMM.empty

track :: (Coercible k Int, Coercible v Int) => k -> v -> DepMap k v -> DepMap k v
track k v (DepMap fwd bwd) = DepMap (ILMM.insert k v fwd) (ILMM.insert v k bwd)

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
