{-@ LANGUAGE NamedFieldPuns @-}
module Rulecheck.Rule
  ( BoxType(..)
  , Rule(..)
  , RuleSide(..)
  , getBoxType
  , getSide
  , ruleFromDecl
  , noTyVarsInSig
  ) where

import Control.Monad.IO.Class (MonadIO (..))
import Data.Maybe (fromJust)
import GHC (GhcMonad (getSession), GhcTc, HsExpr, HsRuleRn (HsRuleRn), LHsExpr, LRuleDecl, RuleBndr (RuleBndr),
            RuleDecl (rd_ext, rd_lhs, rd_name, rd_rhs, rd_tmvs), unLoc)
import GHC.Core.TyCon (isPrimTyCon, tyConRepName_maybe)
import GHC.Core.Type (Kind, Var (..), noFreeVarsOfType, splitTyConApp_maybe)
import GHC.Types.Basic (RuleName)
import GHC.Types.Name (occName, occNameString)
import GHC.Types.Name.Set (elemNameSet, unionNameSet)
import GHC.Types.Var (Var (varName))
import GHC.Utils.Outputable (Outputable (ppr), SDoc)
import Rulecheck.Monad (GhcM)
import Rulecheck.Typecheck (getType)

data RuleSide = LHS | RHS

-- | A `Rule` is obtained from a Haskell RULES declaration (`LRuleDecl`);
--   it contains all of the information necessary to construct fuzzing test cases
data Rule = Rule
  { ruleName          :: RuleName
  , ruleTermAndTyArgs :: [Var]
  , ruleTermArgs      :: [Var]
  , ruleLHS           :: HsExpr GhcTc
  , ruleRHS           :: HsExpr GhcTc

  -- | The type of `ruleLHS`, presumably this is also the type of `ruleRHS`!
  , ruleType :: Kind
  -- | Not strictly necessary, but useful for debugging
  , origRule :: SDoc
  }

noTyVarsInSig :: Rule -> Bool
noTyVarsInSig rule =
  noFreeVarsOfType (ruleType rule) && all (noFreeVarsOfType . varType) (ruleTermArgs rule)

getPrimitiveTypeName :: Kind -> Maybe String
getPrimitiveTypeName ty
  | Just (tyCon , _) <- splitTyConApp_maybe ty
  = if isPrimTyCon tyCon
      then
        do
          name <- tyConRepName_maybe tyCon
          return $ occNameString (occName name)
      else Nothing
  | otherwise = Nothing


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
    let (HsRuleRn lhsVars rhsVars) = rd_ext (unLoc decl)
    let explicitVars = unionNameSet lhsVars rhsVars
    let valueArgs = filter (\arg -> elemNameSet (varName arg) explicitVars) args
    session <- getSession
    lhsTyp  <- liftIO $ getType session lhs
    return $ Rule name args valueArgs (unLoc lhs) (unLoc rhs) (fromJust lhsTyp) (ppr decl)

data BoxType = BoxType
  {  boxedName   :: String
  ,  constructor :: String
  }

getBoxType :: Kind -> Maybe BoxType
getBoxType k = case getPrimitiveTypeName k of
  Just "$tcFloat#"  -> Just $ BoxType "Float" "F#"
  Just "$tcDouble#" -> Just $ BoxType "Double" "D#"
  Just "$tcAddr#"   -> Just $ BoxType "Ptr ()" "Ptr"
  _                 -> Nothing
