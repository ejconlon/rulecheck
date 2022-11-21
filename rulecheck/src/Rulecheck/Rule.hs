{-@ LANGUAGE NamedFieldPuns @-}
module Rulecheck.Rule
  ( BoxType(..)
  , Rule(..)
  , RuleSide(..)
  , getBoxType
  , getSide
  , ruleFromDecl
  , valArgs
  ) where

import Control.Monad.IO.Class (MonadIO (..))
import Data.Char (isAlphaNum)
import Data.Foldable (foldl', toList)
import Data.Maybe (fromJust)
import Data.List (intercalate)
import Data.Set (Set)
import GHC.Core.TyCon
import GHC.Core.Type
import GHC
import GHC.Data.FastString (fs_zenc, zString)
import GHC.Driver.Session (HasDynFlags)
import GHC.Types.Basic (RuleName)
import GHC.Types.Var
import GHC.Types.Name (nameModule_maybe, isValName, occName, occNameString)
import GHC.Types.Name.Set
import GHC.Utils.Outputable (Outputable (..), SDoc, arrow, parens, pprWithCommas, text, ($+$), (<+>), blankLine)
import Prelude hiding ((<>))
import Rulecheck.Monad (GhcM)
import Rulecheck.Typecheck (getType)

data RuleSide = LHS | RHS

-- | A `Rule` is obtained from a Haskell RULES declaration (`LRuleDecl`);
--   it contains all of the information necessary to construct fuzzing test cases
data Rule = Rule
  { ruleName    :: RuleName
  , ruleArgs    :: [Var] -- IMPORTANT! This also includes type variables
  , ruleLHS     :: HsExpr GhcTc
  , ruleRHS     :: HsExpr GhcTc

  -- | The type of `ruleLHS`, presumably this is also the type of `ruleRHS`!
  , ruleType :: Kind
  -- | Not strictly necessary, but useful for debugging
  , origRule :: SDoc
  }

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


-- Extract only the term arguments from the rule
valArgs :: Rule -> [Var]
valArgs = filter (isValName . varName) . ruleArgs

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
    -- mapM_ (go explicitVars) args
    let valueArgs = filter (\arg -> elemNameSet (varName arg) explicitVars) args
    session <- getSession
    lhsTyp  <- liftIO $ getType session lhs
    return $ Rule name valueArgs (unLoc lhs) (unLoc rhs) (fromJust lhsTyp) (ppr decl)
  -- where
  --   go explicitVars arg =
  --     liftIO $
  --       putStrLn $ (showSDocUnsafe $ ppr arg) ++ " in? " ++ show (elemNameSet (varName arg) explicitVars)

data BoxType = BoxType
  {  boxedName   :: String
  ,  constructor :: String
  }

getBoxType :: Kind -> Maybe BoxType
getBoxType k = case getPrimitiveTypeName k of
  Just "$tcFloat#"  -> Just $ BoxType "Float" "F#"
  Just "$tcDouble#" -> Just $ BoxType "Double" "D#"
  _                 -> Nothing
