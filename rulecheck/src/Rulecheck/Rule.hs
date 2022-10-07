module Rulecheck.Rule
  ( Rule(..)
  , RuleSide(..)
  , ruleFromDecl
  , ruleSideDoc
  ) where

import Control.Monad.IO.Class (MonadIO (..))
import Data.Char (isAlphaNum)
import Data.Maybe (fromJust)
import GHC (GhcMonad (..), GhcTc, HsExpr, Kind, LHsExpr, LRuleDecl, RuleBndr (RuleBndr), RuleDecl (..), unLoc)
import GHC.Data.FastString (fs_zenc, zString)
import GHC.Types.Basic (RuleName)
import GHC.Types.Var (Var, varType)
import GHC.Utils.Outputable (Outputable (..), SDoc, parens, pprWithCommas, text, ($+$), (<+>))
import Prelude hiding ((<>))
import Rulecheck.Monad (GhcM)
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
  , ruleLHSType :: Kind
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
    getID _               = error "unimplemented" -- TODO

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
  RHS -> "lhs"

-- | Renders a single side of the rule
ruleSideDoc :: Rule -> RuleSide -> SDoc
ruleSideDoc rule side =
  let
    prefix    = "fn_" ++ sideString side ++ "_"
    name      = prefix ++ sanitizeName (ruleName rule)
    args      = ruleArgs rule
    body      = ppr $ getSide side rule
    argTypes  = asTuple $ map varType args
    resultTyp = ppr $ ruleLHSType rule
    args'     = asTuple args
  in
    text name <+> text "::" <+> argTypes <+> text "->" <+> resultTyp $+$
    text name <+> args' <+> text "=" <+> body

  where
    asTuple []     = undefined
    asTuple [el] = ppr el
    asTuple elems  = parens $ pprWithCommas ppr elems
