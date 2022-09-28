module Rulecheck.Monad
  ( RcState (..)
  , newRcState
  , RcEnv (..)
  , newRcEnv
  , RcM (..)
  , runRcM
  , GhcM
  , raiseGhcM
  , lowerGhcM
  , rcGhcM
  , runGhcM
  ) where

import GHC.Generics (Generic)
import LittleRIO (HasStateRef (..), SomeRef, RIO, newSomeRef, runRIO, readSomeRef)
import LittleLogger (HasLogAction (..), LogAction, MonadLogger (..), defaultLogAction)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Catch (MonadCatch, MonadThrow, MonadMask)
import Control.Monad.Reader (MonadReader (..), asks)
import Control.Monad.State.Strict (MonadState (..))
import Data.Generics.Product (field)
import GHC (HscEnv, GhcMonad (..), DynFlags, runGhc)
import GHC.Driver.Session (HasDynFlags (..))
import GHC.Paths (libdir)

-- RcM - Our application monad

-- Put whatever application state you want here
data RcState = RcState
  {
  } deriving (Eq, Show, Generic)

-- Initialize application state here
newRcState :: RcState
newRcState = RcState

-- Put whatever application config you want here
data RcEnv = RcEnv
  { reLogAction :: !LogAction
  , reStateRef :: !(SomeRef RcState)
  } deriving stock (Generic)

instance HasLogAction RcEnv where
  logActionL = field @"reLogAction"

instance HasStateRef RcState RcEnv where
  stateRefL = field @"reStateRef"

-- Initialize application config here
newRcEnv :: RcState -> IO RcEnv
newRcEnv s = RcEnv defaultLogAction <$> newSomeRef s

newtype RcM a = RcM { unRcM :: RIO RcEnv a }
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadThrow, MonadCatch, MonadMask, MonadUnliftIO, MonadReader RcEnv, MonadLogger, MonadState RcState)

runRcM :: RcM a -> RcEnv -> IO a
runRcM = flip runRIO . unRcM

-- GhcM - the GHC monad

data GhcEnv r = GhcEnv
  { geLiftEnv :: !r
  , geDynFlags :: !DynFlags
  , geStateRef :: !(SomeRef HscEnv)
  } deriving stock (Generic)

instance HasLogAction r => HasLogAction (GhcEnv r) where
  logActionL = field @"geLiftEnv" . logActionL

instance HasStateRef HscEnv (GhcEnv r) where
  stateRefL = field @"geStateRef"

newtype GhcM r a = GhcM { unGhcM :: RIO (GhcEnv r) a }
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadThrow, MonadCatch, MonadMask, MonadUnliftIO, MonadReader (GhcEnv r), MonadState HscEnv, MonadLogger)

instance HasDynFlags (GhcM r) where
  getDynFlags = asks geDynFlags

instance GhcMonad (GhcM r) where
  getSession = get
  setSession = put

raiseGhcM :: RIO r a -> GhcM r a
raiseGhcM act = GhcM $ do
  r <- asks geLiftEnv
  liftIO (runRIO r act)

lowerGhcM :: GhcM r a -> RIO r a
lowerGhcM act = do
  r <- ask
  liftIO $ do
    runGhc (Just libdir) $ do
      dynFlags <- getDynFlags
      sess <- getSession
      sr <- newSomeRef sess
      let ge = GhcEnv r dynFlags sr
      ret <- liftIO (runRIO ge (unGhcM act))
      -- note sure if we really need to write the session back, but we can
      sess' <- readSomeRef sr
      setSession sess'
      pure ret

rcGhcM :: RcM a -> GhcM RcEnv a
rcGhcM = raiseGhcM . unRcM

runGhcM :: GhcM r a -> r -> IO a
runGhcM act r = runRIO r (lowerGhcM act)
