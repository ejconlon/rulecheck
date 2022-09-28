module Rulecheck.Monad
  ( RcState (..)
  , newRcState
  , RcEnv (..)
  , newRcEnv
  , RcM (..)
  , runRcM
  ) where

import GHC.Generics (Generic)
import LittleRIO (HasStateRef (..), SomeRef, RIO, newSomeRef, runRIO)
import LittleLogger (HasLogAction (..), LogAction, MonadLogger (..), defaultLogAction)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Catch (MonadCatch, MonadThrow)
import Control.Monad.Reader (MonadReader)
import Control.Monad.State.Strict (MonadState)
import Data.Generics.Product (field)
import GHC (GhcT)

data RcState = RcState
  {
  } deriving (Eq, Show)

newRcState :: RcState
newRcState = RcState

data RcEnv = RcEnv
  { rcLogAction :: !LogAction
  , rcStateRef :: !(SomeRef RcState)
  } deriving stock (Generic)

instance HasLogAction RcEnv where
  logActionL = field @"rcLogAction"

instance HasStateRef RcState RcEnv where
  stateRefL = field @"rcStateRef"

newRcEnv :: RcState -> IO RcEnv
newRcEnv s = RcEnv defaultLogAction <$> newSomeRef s

newtype RcM a = RcM { unRcM :: RIO RcEnv a }
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadThrow, MonadCatch, MonadUnliftIO, MonadReader RcEnv, MonadLogger, MonadState RcState)

runRcM :: RcM a -> RcEnv -> IO a
runRcM = flip runRIO . unRcM

newtype GhcRcM a = GhcRcM { unGhcRcM :: GhcT RcM a }
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadThrow, MonadCatch)

-- embedRcM :: RcM a -> GhcRcM a
-- embedRcM = GhcRcM . lift

-- instance MonadLogger GhcRcM where
--   monadLoggerLog loc src lvl msg = embedRcM (monadLoggerLog loc src lvl msg)
