module Test.Searchterm.State
  ( StateT (..)
  , State
  , MonadState (..)
  , gets
  , runS
  , testS
  , applyS
  , applyTestS
  , applyTestST
  ) where

import Control.Monad.State.Strict (MonadState (..), State, StateT (..), evalStateT, gets, runState)
import Control.Monad.Trans (lift)

runS :: Monad m => s -> StateT s m () -> m ()
runS = flip evalStateT

testS :: Monad m => (s -> m a) -> StateT s m a
testS p = get >>= lift . p

applyS :: Monad m => State s a -> StateT s m a
applyS = state . runState

applyTestS :: Monad m => State s a -> (a -> s -> m b) -> StateT s m b
applyTestS act check = do
  a <- applyS act
  s <- get
  lift (check a s)

applyTestST :: Monad m => StateT s m a -> (a -> s -> m b) -> StateT s m b
applyTestST act check = do
  a <- act
  s <- get
  lift (check a s)
