module Rulecheck.Parsing
  ( ParseErr (..)
  , fakeFilePath
  , getParsedRuleDecls
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

-- | Returns the rule declarations in this module. This resulting declarations
--   do not contain type information. This function should NOT be used to obtain the
--   `Rule` objects for fuzzing; for that, you should use `getTypecheckedRuleDecls`
--
--   This function may be useful for a quick inspection of the rules in arbitrary
--   Haskell files, as it operates on a parsed AST (and therefore does not require
--   importing modules, typechecking, etc.)
getParsedRuleDecls :: HsModule -> [RuleDecls GhcPs]
getParsedRuleDecls hsMod = concatMap getRuleDecl (hsmodDecls hsMod) where
  getRuleDecl decl | RuleD _ ruleDecls <- unLoc decl = [ruleDecls]
  getRuleDecl _ = []

parseModule :: (MonadThrow m, HasDynFlags m) => FilePath -> String -> m (Located HsModule)
parseModule fp contents = runParser fp contents GP.parseModule
