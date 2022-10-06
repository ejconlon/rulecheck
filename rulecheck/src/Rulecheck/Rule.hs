module Rulecheck.Rule (Rule(..), ruleArgs, ruleLHS, ruleRHS, ruleLHSType, ruleFromDecl) where

import Control.Monad.IO.Class (MonadIO (..))
import Data.Maybe (fromJust)
import GHC
import GHC.Types.Var(Var, varType)
import Rulecheck.Typecheck (getType)
import Rulecheck.Monad (GhcM)

data Rule = Rule
  { ruleArgs    :: [Var]
  , ruleLHS     :: HsExpr GhcTc
  , ruleRHS     :: HsExpr GhcTc
  , ruleLHSType :: Kind
  }

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

ruleFromDecl :: LRuleDecl GhcTc -> GhcM a Rule
ruleFromDecl decl =
  do
    let args       = getRuleArguments decl
    let (lhs, rhs) = getRuleBody decl
    session <- getSession
    lhsTyp  <- liftIO $ getType session lhs
    return $ Rule args (unLoc lhs) (unLoc rhs) (fromJust lhsTyp)
