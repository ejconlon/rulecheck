{-@ LANGUAGE NamedFieldPuns @-}
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
import Data.List (intercalate)
import Data.Set (Set)
import Debug.Trace (trace)
import GHC.Core.TyCon
import GHC.Core.Type
import GHC (GhcMonad (..), GhcTc, HsExpr, Kind, LHsExpr, LRuleDecl, RuleBndr (RuleBndr), RuleDecl (..), unLoc)
import GHC.Data.FastString (fs_zenc, zString)
import GHC.Driver.Session (HasDynFlags)
import GHC.Hs.Decls
import GHC.Types.Basic (RuleName)
import GHC.Types.Var (Var, varName, varType, isId)
import GHC.Types.Name (isValName, occName, occNameString)
import GHC.Types.Name.Set
import GHC.Utils.Outputable (Outputable (..), SDoc, arrow, parens, pprWithCommas, text, ($+$), (<+>), showSDocUnsafe)
import Prelude hiding ((<>))
import Rulecheck.Monad (GhcM)
import Rulecheck.Rendering (outputString)
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
    liftIO $ putStrLn $ showSDocUnsafe $ ppr explicitVars
    -- mapM_ (go explicitVars) args
    let valueArgs = filter (\arg -> elemNameSet (varName arg) explicitVars) args
    session <- getSession
    lhsTyp  <- liftIO $ getType session lhs
    return $ Rule name valueArgs (unLoc lhs) (unLoc rhs) (fromJust lhsTyp)
  -- where
  --   go explicitVars arg =
  --     liftIO $
  --       putStrLn $ (showSDocUnsafe $ ppr arg) ++ " in? " ++ show (elemNameSet (varName arg) explicitVars)

sanitizeString :: String -> String
sanitizeString = map (\c -> if isAlphaNum c then c else '_')

-- | Names can and often do contain characters that are not safe for identifiers.
-- We just replace those characters with underscores.
-- Also adds an index to the rule to ensure each name is unique
sanitizeName :: RuleName -> Int -> String
sanitizeName rn idx = sanitizeString (zString (fs_zenc rn)) ++ "_" ++ show idx

-- | Just a string prefix for a rule side
sideString :: RuleSide -> String
sideString = \case
  LHS -> "lhs"
  RHS -> "rhs"

asTuple :: Outputable a => [a] -> SDoc
asTuple [] = text "()"
asTuple [el] = ppr el
asTuple elems = parens $ pprWithCommas ppr elems

toSDoc :: (Functor m, HasDynFlags m) => Outputable a => Set String -> a -> m SDoc
toSDoc importedModuleNames = fmap text . outputString importedModuleNames

-- | Renders a single side of the rule like "fn_lhs_NAME :: ... \n fn_lhs_NAME ... = ..."
ruleSideDoc :: (Rule, Int) -> RuleSide -> SDoc
ruleSideDoc (rule, idx) side =
  let
    prefix    = "fn_" ++ sideString side ++ "_"
    name      = prefix ++ sanitizeName (ruleName rule) idx
    args      = valArgs rule
    body      = ppr (getSide side rule)
    argTypes  = asTuple (map (ppr . varType) args)
    resultTyp = ppr (ruleType rule)
    args'     = asTuple args
  in
    text name <+> text "::" <+> argTypes <+> arrow <+> resultTyp $+$
    text name <+> args' <+> text "=" <+> body

-- | Renders the rule pair defn like "pair_NAME :: SomeTestableRule \n pair_NAME = ..."
rulePairDoc :: (Rule, Int) -> SDoc
rulePairDoc (rule, idx) =
  let prefixLhs = "fn_" ++ sideString LHS ++ "_"
      prefixRhs = "fn_" ++ sideString RHS ++ "_"
      bareName = sanitizeName (ruleName rule) idx
      nameLhs = prefixLhs ++ bareName
      nameRhs = prefixRhs ++ bareName
      nameRule = "rule_" ++ bareName
  in text nameRule <+> text ":: SomeTestableRule" $+$
     text nameRule <+> text "= SomeTestableRule" <+> parens (text ruleFunc <+> text nameLhs <+> text nameRhs)
  where
    ruleFunc =
      case (map (getPrimitiveTypeName . varType) (ruleArgs rule) , getPrimitiveTypeName (ruleType rule)) of
        ([Just "$tcFloat#"] , Just "$tcFloat#") -> primFloatRule
        ([Just "$tcDouble#"] , Just "$tcDouble#") -> primDoubleRule
        other -> trace ("Names were " ++ show other) "TestableRule"

-- | Renders the rule test defn like "test_NAME :: TestTree \n test_NAME = ..."
ruleTestDoc :: (Rule, Int) -> SDoc
ruleTestDoc (rule, idx) =
  let bareName = sanitizeName (ruleName rule) idx
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
        text ("import Rulecheck.Testing(" ++ testingImports ++ ")")
  in foldl' (\x d -> x $+$ text "import" <+> text d) start (toList deps)

-- | Renders the entire test module
ruleModuleDoc :: String -> Set String -> [Rule] -> SDoc
ruleModuleDoc modName deps rules =
  foldl' (\x r -> x $+$ f r) (ruleModuleHeaderDoc modName deps) (zip rules [1..])
  where
    f :: (Rule, Int) -> SDoc
    f r = lhs $+$ rhs $+$ rulePairDoc r $+$ ruleTestDoc r
      where
        lhs = ruleSideDoc r LHS
        rhs = ruleSideDoc r RHS

testingImports :: String
testingImports =
  intercalate ", " $
    [ "SomeTestableRule(..)"
    , "TestableRule (..)"
    , "testSomeTestableRule"
    , primFloatRule
    , primDoubleRule
    ]

primFloatRule :: String
primFloatRule = "primFloatRule"

primDoubleRule :: String
primDoubleRule = "primDoubleRule"
