module Rulecheck.Monad
  ( GhcM
  , runGhcM
  , cradleGhcM
  ) where

import Control.Exception (throwIO)
import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader (..), asks)
import Control.Monad.State.Strict (MonadState (..))
import Data.Generics.Product (field)
import Data.Void (Void)
import GHC (DynFlags, GhcMonad (..), HscEnv, runGhc)
import GHC.Driver.Session (HasDynFlags (..))
import GHC.Generics (Generic)
import GHC.Paths (libdir)
import HIE.Bios (Cradle, CradleLoadResult (..), findCradle, getCompilerOptions, initSession, loadCradle,
                 loadImplicitCradle)
import LittleLogger (HasLogAction (..), LogAction, MonadLogger (..), defaultLogAction)
import LittleRIO (HasStateRef (..), RIO, SomeRef, newSomeRef, readSomeRef, runRIO)

data GhcEnv = GhcEnv
  { geLogAction :: !LogAction
  , geDynFlags :: !DynFlags
  , geStateRef :: !(SomeRef HscEnv)
  } deriving stock (Generic)

instance HasLogAction GhcEnv where
  logActionL = field @"geLogAction" . logActionL

instance HasStateRef HscEnv GhcEnv where
  stateRefL = field @"geStateRef"

newtype GhcM a = GhcM { unGhcM :: RIO GhcEnv a }
  deriving newtype (Functor, Applicative, Monad, MonadIO,
    MonadThrow, MonadCatch, MonadMask, MonadUnliftIO, MonadReader GhcEnv,
    MonadState HscEnv, MonadLogger, MonadFail)

instance HasDynFlags GhcM where
  getDynFlags = asks geDynFlags

instance GhcMonad GhcM where
  getSession = get
  setSession = put

runGhcM :: GhcM a -> IO a
runGhcM act = do
  runGhc (Just libdir) $ do
    dynFlags <- getDynFlags
    sess <- getSession
    sr <- newSomeRef sess
    let ge = GhcEnv defaultLogAction dynFlags sr
    ret <- liftIO (runRIO ge (unGhcM act))
    -- note sure if we really need to write the session back, but we can
    sess' <- readSomeRef sr
    setSession sess'
    pure ret

getCradle :: FilePath -> IO (Cradle Void)
getCradle filename = do
  maybeCradle <- findCradle filename
  case maybeCradle of
    Just c  -> loadCradle c
    Nothing -> loadImplicitCradle filename

cradleGhcM :: FilePath -> GhcM a -> IO a
cradleGhcM filename act = do
  cradle <- liftIO (getCradle filename)
  compileOptsResult <- liftIO (getCompilerOptions filename cradle)
  case compileOptsResult of
    CradleSuccess opts -> runGhcM (initSession opts *> act)
    CradleFail err -> throwIO err
    CradleNone -> fail "No cradle was loaded"
