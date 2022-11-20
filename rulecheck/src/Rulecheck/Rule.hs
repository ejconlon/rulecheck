module Rulecheck.Rule
  ( Rule(..)
  , RuleSide(..)
  , ruleFromDecl
  , ruleSideDoc
  , rulePairDoc
  , ruleTestDoc
  , ruleModuleHeaderDoc
  , ruleModuleDoc
  ) where

import Control.Monad (foldM)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Char (isAlphaNum)
import Data.Foldable (foldl', toList)
import Data.Maybe (fromJust)
import Data.Set (Set)
import GHC (GhcMonad (..), GhcTc, HsExpr, Kind, LHsExpr, LRuleDecl, RuleBndr (RuleBndr), RuleDecl (..), unLoc)
import GHC.Data.FastString (fs_zenc, zString)
import GHC.Driver.Session (HasDynFlags)
import GHC.Types.Basic (RuleName)
import GHC.Types.Var (Var, varType)
import GHC.Utils.Outputable (Outputable (..), SDoc, parens, pprWithCommas, text, ($+$), (<+>))
import Prelude hiding ((<>))
import Rulecheck.Monad (GhcM)
import Rulecheck.Rendering (outputString)
import Rulecheck.Typecheck (getType)

data RuleSide = LHS | RHS

-- | A `Rule` is obtained from a Haskell RULES declaration (`LRuleDecl`);
--   it contains all of the information necessary to construct fuzzing test cases
data Rule = Rule
  { ruleName    :: RuleName
  , ruleArgs    :: [Var]
  , ruleLHS     :: HsExpr GhcTc
  , ruleRHS     :: HsExpr GhcTc

  -- | The type of `ruleLHS`, presumably this is also the type of `ruleRHS`!
  , ruleType :: Kind
  }

getSide :: RuleSide -> Rule -> HsExpr GhcTc
getSide LHS = ruleLHS
getSide RHS = ruleRHS

getRuleName :: LRuleDecl GhcTc -> RuleName
getRuleName = snd . unLoc . rd_name . unLoc

-- | Returns the left and right-hand-sides of the rule
getRuleBody :: LRuleDecl GhcTc -> (LHsExpr GhcTc, LHsExpr GhcTc)
getRuleBody decl =
  let
    decl' = unLoc decl
  in
    (rd_lhs decl', rd_rhs decl')

getRuleArguments :: LRuleDecl GhcTc -> [Var]
getRuleArguments decl =
  let
    args = rd_tmvs (unLoc decl)
  in
    map (getID . unLoc) args

  where
    getID (RuleBndr _ ident) = unLoc ident
    getID _                  = error "unimplemented"

-- | Obtains a `Rule` from the corresponding typechecked rule declaration
-- Note that this requires interacting with GHC to get the type of the LHS of the rule
-- See `getType` for more details.
ruleFromDecl :: LRuleDecl GhcTc -> GhcM Rule
ruleFromDecl decl =
  do
    let args       = getRuleArguments decl
    let (lhs, rhs) = getRuleBody decl
    let name = getRuleName decl
    session <- getSession
    lhsTyp  <- liftIO $ getType session lhs
    return $ Rule name args (unLoc lhs) (unLoc rhs) (fromJust lhsTyp)

sanitizeString :: String -> String
sanitizeString = map (\c -> if isAlphaNum c then c else '_')

-- | Names can and often do contain characters that are not safe for identifiers.
-- We just replace those characters with underscores.
sanitizeName :: RuleName -> String
sanitizeName = sanitizeString . zString . fs_zenc

-- | Just a string prefix for a rule side
sideString :: RuleSide -> String
sideString = \case
  LHS -> "lhs"
  RHS -> "rhs"

asTuple :: Outputable a => [a] -> SDoc
asTuple [] = text "()"
asTuple [el] = ppr el
asTuple elems = parens $ pprWithCommas ppr elems

toSDoc :: (Functor m, HasDynFlags m) => Outputable a => a -> m SDoc
toSDoc = fmap text . outputString

-- | Renders a single side of the rule like "fn_lhs_NAME :: ... \n fn_lhs_NAME ... = ..."
ruleSideDoc :: (Monad m, HasDynFlags m) => Rule -> RuleSide -> m SDoc
ruleSideDoc rule side = do
  let prefix = "fn_" ++ sideString side ++ "_"
      name = prefix ++ sanitizeName (ruleName rule)
      args = ruleArgs rule
  body      <- toSDoc (getSide side rule)
  argTypes  <- fmap asTuple (traverse (toSDoc . varType) args)
  resultTyp <- toSDoc (ruleType rule)
  let args' = asTuple args
  pure $!
    text name <+> text "::" <+> argTypes <+> text "->" <+> resultTyp $+$
    text name <+> args' <+> text "=" <+> body

-- | Renders the rule pair defn like "pair_NAME :: SomeTestableRule \n pair_NAME = ..."
rulePairDoc :: Rule -> SDoc
rulePairDoc rule =
  let prefixLhs = "fn_" ++ sideString LHS ++ "_"
      prefixRhs = "fn_" ++ sideString RHS ++ "_"
      bareName = sanitizeName (ruleName rule)
      nameLhs = prefixLhs ++ bareName
      nameRhs = prefixRhs ++ bareName
      nameRule = "rule_" ++ bareName
  in text nameRule <+> text ":: SomeTestableRule" $+$
     text nameRule <+> text "= SomeTestableRule" <+> parens (text "TestableRule" <+> text nameLhs <+> text nameRhs)

-- | Renders the rule test defn like "test_NAME :: TestTree \n test_NAME = ..."
ruleTestDoc :: Rule -> SDoc
ruleTestDoc rule =
  let bareName = sanitizeName (ruleName rule)
      nameRule = "rule_" ++ bareName
      nameTest = "test_" ++ bareName
      nameQuot = "\"" ++ bareName ++ "\""
  in text nameTest <+> text ":: TestTree" $+$
     text nameTest <+> text "= testSomeTestableRule" <+> text nameQuot <+> text nameRule

-- | Renders the test module header
ruleModuleHeaderDoc :: String -> Set String -> SDoc
ruleModuleHeaderDoc modName deps =
  let start = text "module" <+> text modName <+> text "where" $+$
        text "import Test.Tasty (TestTree)" $+$
        text "import Rulecheck.Testing (SomeTestableRule (..), TestableRule (..), testSomeTestableRule)"
  in foldl' (\x d -> x $+$ text "import qualified" <+> text d) start (toList deps)

-- | Renders the entire test module
ruleModuleDoc :: (Monad m, HasDynFlags m) => String -> Set String -> [Rule] -> m SDoc
ruleModuleDoc modName deps rules =
  let f r = do
        lhs <- ruleSideDoc r LHS
        rhs <- ruleSideDoc r RHS
        pure (lhs $+$ rhs $+$ rulePairDoc r $+$ ruleTestDoc r)
  in foldM (\x r -> fmap (x $+$) (f r)) (ruleModuleHeaderDoc modName deps) rules
