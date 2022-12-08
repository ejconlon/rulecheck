module Searchterm.Synth.Monad
  ( TrackSt (..)
  , Track
  , runManyTrack
  ) where

import Control.Applicative (Alternative (..))
import Control.Monad.Except (Except, MonadError, runExcept)
import Control.Monad.Logic (LogicT, MonadLogic (..), observeManyT)
import Control.Monad.Reader (MonadReader (..))
import Control.Monad.State.Strict (MonadState (..), StateT (..), gets, modify')
import Data.Bifunctor (second)

-- | Backtracking state - the x component goes foreward, the y component backtracks
data TrackSt x y = TrackSt
  { tsFwd :: !x
  , tsBwd :: !y
  } deriving stock (Eq, Show)

data TrackEnvSt r x y = TrackEnvSt
  { tesEnv :: !r
  , tesSt :: !(TrackSt x y)
  } deriving stock (Eq, Show)

newtype Track r x y e a = Track { unTrack :: LogicT (StateT (TrackEnvSt r x y) (Except e)) a }
  deriving newtype (Functor, Applicative, Monad, MonadError e)

instance MonadReader r (Track r x y e) where
  ask = Track (gets tesEnv)
  local f m = Track $ do
    r0 <- state (\st -> let r0 = tesEnv st in (r0, st { tesEnv = f r0 }))
    a <- unTrack m
    modify' (\st -> st { tesEnv = r0 })
    pure a

instance MonadState (TrackSt x y) (Track r x y e) where
  get = Track (gets tesSt)
  put ts = Track (modify' (\st -> st { tesSt = ts }))

observeManyTrack :: Int -> Track r x y e a -> StateT (TrackEnvSt r x y) (Except e) [a]
observeManyTrack n = observeManyT n . unTrack . reset

runManyTrack :: Int -> Track r x y e a -> r -> TrackSt x y -> Either e ([a], TrackSt x y)
runManyTrack n m r st = fmap (second tesSt) (runExcept (runStateT (observeManyTrack n m) (TrackEnvSt r st)))

restorePure :: TrackEnvSt r x y -> TrackEnvSt r x y -> TrackEnvSt r x y
restorePure (TrackEnvSt oldR (TrackSt _ oldBwd)) (TrackEnvSt _ (TrackSt newFwd _)) = TrackEnvSt oldR (TrackSt newFwd oldBwd)

restore :: TrackEnvSt r x y -> Track r x y e a -> Track r x y e a
restore oldTes x = Track (modify' (restorePure oldTes)) *> x

finalize :: TrackEnvSt r x y -> Track r x y e a -> Track r x y e a
finalize oldTes x = Track (unTrack x <|> unTrack (restore oldTes empty))

reset :: Track r x y e a -> Track r x y e a
reset x = do
  oldTes <- Track get
  finalize oldTes x

-- | Custom implementation that backtracks part of state
instance Alternative (Track r x y e) where
  empty = Track empty
  x <|> y = do
    oldTes <- Track get
    Track (unTrack x <|> unTrack (restore oldTes y))

-- -- | Custom implementation that backtracks part of state
-- instance MonadLogic (Track r x y e) where
--   msplit x = Track (fmap (fmap (second Track)) (msplit (unTrack x)))
--   interleave x y = do
--     oldTes <- Track get
--     Track (interleave (unTrack x) (unTrack (restore oldTes y)))

-- Break glass in case of emergency: Disable fairness!
instance MonadLogic (Track r x y e) where
  msplit x = Track (fmap (fmap (second Track)) (msplit (unTrack x)))
  interleave = (<|>)
  (>>-) = (>>=)
