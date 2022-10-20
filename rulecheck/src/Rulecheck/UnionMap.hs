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

import Control.Monad.State.Strict (MonadState (..), State, StateT, modify', state)
import Control.Monad.Trans (lift)
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
type MergeFun m v = v -> v -> m v

merge :: (Ord k, Coercible k Int, Monad m) => MergeFun m v -> k -> k -> UnionMap k v -> m (MergeRes k, UnionMap k v)
merge f a b (UnionMap uf m) =
  let (res, uf') = UF.merge a b uf
  in case res of
    MergeResChanged knew kold -> do
      let vnew = ILM.partialLookup knew m
          vold = ILM.partialLookup kold m
      vmerge <- f vnew vold
      let m' = ILM.insert knew vmerge (ILM.delete kold m)
      pure (res, UnionMap uf' m')
    _ -> pure (res, UnionMap uf' m)

stateMerge :: (Ord k, Coercible k Int, Monad m) => MergeFun m v -> k -> k -> StateT (UnionMap k v) m (MergeRes k)
stateMerge f a b = do
  um <- get
  (res, um') <- lift (merge f a b um)
  put um'
  pure res
