module Rulecheck.Rule
  ( Rule(..)
  , RuleSide(..)
  , ruleArgs
  , ruleLHS
  , ruleRHS
  , ruleLHSType
  , ruleFromDecl
  , sketchTestFunction
  ) where

import Prelude hiding ((<>))
import Control.Monad.IO.Class (MonadIO (..))
import Data.Maybe (fromJust)
import GHC
import GHC.Types.Var(Var, varType)
import GHC.Utils.Outputable
import Rulecheck.Typecheck (getType)
import Rulecheck.Monad (GhcM)

data RuleSide = LHS | RHS

-- | A `Rule` is obtained from a Haskell RULES declaration (`LRuleDecl`);
--   it contains all of the information necessary to construct fuzzing test cases
data Rule = Rule
  { ruleArgs    :: [Var]
  , ruleLHS     :: HsExpr GhcTc
  , ruleRHS     :: HsExpr GhcTc

  -- | The type of `ruleLHS`, presumably this is also the type of `ruleRHS`!
  , ruleLHSType :: Kind
  }

getSide :: RuleSide -> Rule -> HsExpr GhcTc
getSide LHS = ruleLHS
getSide RHS = ruleRHS

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
    getID (RuleBndr _ id) = unLoc id
    getID _               = error "unimplemented" -- TODO

-- | Obtains a `Rule` from the corresponding typechecked rule declaration
-- Note that this requires interacting with GHC to get the type of the LHS of the rule
-- See `getType` for more details.
ruleFromDecl :: LRuleDecl GhcTc -> GhcM a Rule
ruleFromDecl decl =
  do
    let args       = getRuleArguments decl
    let (lhs, rhs) = getRuleBody decl
    session <- getSession
    lhsTyp  <- liftIO $ getType session lhs
    return $ Rule args (unLoc lhs) (unLoc rhs) (fromJust lhsTyp)

-- | Presents a sketch on what one of the test functions should look like.
--   A more robust implementation would construct a valid AST
sketchTestFunction :: Rule -> RuleSide -> SDoc
sketchTestFunction rule side =
  let
    args      = ruleArgs rule
    body      = ppr $ getSide side rule
    argTypes  = asTuple $ map varType args
    resultTyp = ppr $ ruleLHSType rule
    args'     = asTuple args
  in
    text "test ::" <+> argTypes <+> text "->" <+> resultTyp $+$
    text "test" <+> args' <+> text "=" <+> body

  where
    asTuple []     = undefined
    asTuple [elem] = ppr elem
    asTuple elems  = parens $ pprWithCommas ppr elems
