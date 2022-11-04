module Searchterm.Synth.Monad
  ( RSE
  , runRSE
  , TrackSt (..)
  , Track
  , observeManyTrack
  , runManyTrack
  ) where

import Control.Monad.Except (Except, MonadError, runExcept)
import Control.Monad.State.Strict (StateT (..), MonadState (..), modify', gets)
import Control.Monad.Reader (ReaderT (..), MonadReader)
import Control.Monad.Logic (LogicT, MonadLogic (..), observeManyT)
import Control.Applicative (Alternative (..))
import Data.Bifunctor (second)

-- | RSE == Reader, State, Error
newtype RSE r s e a = RSE { unRSE :: ReaderT r (StateT s (Except e)) a }
  deriving newtype (Functor, Applicative, Monad, MonadReader r, MonadState s, MonadError e)

runRSE :: RSE r s e a -> r -> s -> Either e (a, s)
runRSE m r s = runExcept (runStateT (runReaderT (unRSE m) r) s)

-- | Backtracking state - the x component goes foreward, the y component backtracks
data TrackSt x y = TrackSt
  { tsFwd :: !x
  , tsBwd :: !y
  } deriving stock (Eq, Show)

newtype Track r x y e a = Track { unTrack :: LogicT (RSE r (TrackSt x y) e) a }
  deriving newtype (Functor, Applicative, Monad, MonadReader r, MonadState (TrackSt x y), MonadError e)

observeManyTrack :: Int -> Track r x y e a -> RSE r (TrackSt x y) e [a]
observeManyTrack n = observeManyT n . unTrack . reset

runManyTrack :: Int -> Track r x y e a -> r -> TrackSt x y -> Either e ([a], TrackSt x y)
runManyTrack n m = runRSE (observeManyTrack n m)

restore :: y -> Track r x y e a -> Track r x y e a
restore saved x = modify' (\st -> st { tsBwd = saved }) *> x

finalize :: y -> Track r x y e a -> Track r x y e a
finalize saved x = Track (unTrack x <|> unTrack (restore saved empty))

reset :: Track r x y e a -> Track r x y e a
reset x = gets tsBwd >>= \saved -> finalize saved x

-- | Custom implementation that backtracks part of state
instance Alternative (Track r x y e) where
  empty = Track empty
  x <|> y = do
    saved <- gets tsBwd
    finalize saved (Track (unTrack x <|> unTrack (restore saved y)))

-- | Custom implementation that backtracks part of state
instance MonadLogic (Track r x y e) where
  msplit x = do
    saved <- gets tsBwd
    msplitWith saved x
  interleave x y = do
    saved <- gets tsBwd
    interleaveWith saved x y

msplitWith :: y -> Track r x y e a -> Track r x y e (Maybe (a, Track r x y e a))
msplitWith saved x = do
  mp <- Track (msplit (unTrack x))
  restore saved (pure (fmap (second Track) mp))

interleaveWith :: y -> Track r x y e a -> Track r x y e a -> Track r x y e a
interleaveWith saved = go where
  go x y = do
    mp <- Track (msplit (unTrack x))
    case mp of
      Nothing -> finalize saved (restore saved y)
      Just (val, restX) -> do
        let restYX = go (restore saved y) (Track restX)
        Track (pure val <|> unTrack restYX)
