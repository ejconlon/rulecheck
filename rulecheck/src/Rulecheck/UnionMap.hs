module Rulecheck.UnionMap
  ( UnionMap
  , fromMap
  , empty
  , size
  , insert
  , stateInsert
  , find
  , stateFind
  , MergeFun
  , merge
  , stateMerge
  ) where

import Control.Monad.State.Strict (State, modify', state)
import Data.Coerce (Coercible)
import IntLike.Map (IntLikeMap)
import qualified IntLike.Map as ILM
import Rulecheck.UnionFind (MergeRes (..), UnionFind)
import qualified Rulecheck.UnionFind as UF

data UnionMap k v = UnionMap
  { umFind :: !(UnionFind k)
  , umValues :: !(IntLikeMap k v)
  } deriving stock (Eq, Show)

fromMap :: Coercible k Int => IntLikeMap k v -> UnionMap k v
fromMap m = UnionMap (UF.fromSet (ILM.keysSet m)) m

empty :: UnionMap k v
empty = UnionMap UF.empty ILM.empty

size :: UnionMap k v -> Int
size = UF.size . umFind

insert :: Coercible k Int => k -> v -> UnionMap k v -> UnionMap k v
insert k v (UnionMap uf m) = UnionMap (UF.insert k uf) (ILM.insert k v m)

stateInsert :: Coercible k Int => k -> v -> State (UnionMap k v) ()
stateInsert k v = modify' (insert k v)

find :: (Eq k, Coercible k Int) => k -> UnionMap k v -> (Maybe (k, v), UnionMap k v)
find k (UnionMap uf m) =
  let (mx, uf') = UF.find k uf
      u' = UnionMap uf' m
      mp = fmap (\x -> (x, ILM.partialLookup x m)) mx
  in (mp, u')

stateFind :: (Eq k, Coercible k Int) => k -> State (UnionMap k v) (Maybe (k, v))
stateFind k = state (find k)

-- Must be symmetric, reflexive, etc
type MergeFun e v = v -> v -> Either e v

merge :: (Ord k, Coercible k Int) => MergeFun e v -> k -> k -> UnionMap k v -> (Either e (MergeRes k), UnionMap k v)
merge f a b (UnionMap uf m) =
  let (res, uf') = UF.merge a b uf
  in case res of
    MergeResChanged knew kold ->
      let vnew = ILM.partialLookup knew m
          vold = ILM.partialLookup kold m
      in case f vnew vold of
        Left e -> (Left e, UnionMap uf' m)
        Right vmerge ->
          let m' = ILM.insert knew vmerge (ILM.delete kold m)
          in (Right res, UnionMap uf' m')
    _ -> (Right res, UnionMap uf' m)

stateMerge :: (Ord k, Coercible k Int) => MergeFun e v -> k -> k -> State (UnionMap k v) (Either e (MergeRes k))
stateMerge f a b = state (merge f a b)
