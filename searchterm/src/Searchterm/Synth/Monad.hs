module Searchterm.Synth.Monad
  ( TrackSt (..)
  , Track
  , observeManyTrack
  , runManyTrack
  ) where

import Control.Monad.Except (Except, MonadError, runExcept)
import Control.Monad.State.Strict (StateT (..), MonadState (..), modify', gets)
import Control.Monad.Reader (ReaderT (..), MonadReader (..))
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

restore :: r -> y -> Track r x y e a -> Track r x y e a
restore noted saved x = modify' (\st -> st { tsBwd = saved }) *> local (const noted) x

finalize :: r -> y -> Track r x y e a -> Track r x y e a
finalize noted saved x = Track (unTrack x <|> unTrack (restore noted saved empty))

reset :: Track r x y e a -> Track r x y e a
reset x = do
  noted <- ask
  saved <- gets tsBwd
  finalize noted saved x

-- | Custom implementation that backtracks part of state
instance Alternative (Track r x y e) where
  empty = Track empty
  x <|> y = do
    noted <- ask
    saved <- gets tsBwd
    Track (unTrack x <|> unTrack (restore noted saved y))

-- | Custom implementation that backtracks part of state
instance MonadLogic (Track r x y e) where
  msplit x = Track (fmap (fmap (second Track)) (msplit (unTrack x)))
  interleave x y = do
    noted <- ask
    saved <- gets tsBwd
    Track (interleave (unTrack x) (unTrack (restore noted saved y)))
