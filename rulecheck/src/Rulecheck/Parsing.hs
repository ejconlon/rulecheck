module Rulecheck.Parsing
  ( ParseErr (..)
  , fakeFilePath
  , getRules
  , parseModule
  ) where

import Control.Monad.Catch (MonadThrow (..))
import Control.Exception (Exception)
import GHC.Plugins (unLoc, Located, HasDynFlags (..), mkRealSrcLoc, mkFastString)
import GHC.Hs (GhcPs, HsDecl(..), RuleDecls, HsModule(..))
import GHC.Parser.Lexer (ParseResult(..), P (..), mkPState, getErrorMessages)
import Data.Foldable (toList)
import GHC.Data.StringBuffer (stringToStringBuffer)
import qualified GHC.Parser as GP

newtype ParseErr =
    ParseErr [String]
  deriving stock (Eq, Show)

instance Exception ParseErr

runParser :: (MonadThrow m, HasDynFlags m) => FilePath -> String -> P a -> m a
runParser filename contents parser = do
  flags <- getDynFlags
  let location = mkRealSrcLoc (mkFastString filename) 1 1
      buffer = stringToStringBuffer contents
      parseState = mkPState flags buffer location
  case unP parser parseState of
    POk _ a -> pure a
    PFailed st -> throwM (ParseErr (fmap show (toList (getErrorMessages st flags))))

fakeFilePath :: FilePath
fakeFilePath = "<interactive>"

getRules :: HsModule -> [RuleDecls GhcPs]
getRules hsMod = concatMap getRuleDecl (hsmodDecls hsMod) where
  getRuleDecl decl | RuleD _ ruleDecls <- unLoc decl = [ruleDecls]
  getRuleDecl _ = []

parseModule :: (MonadThrow m, HasDynFlags m) => FilePath -> String -> m (Located HsModule)
parseModule fp contents = runParser fp contents GP.parseModule
