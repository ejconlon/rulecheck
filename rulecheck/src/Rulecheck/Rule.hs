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

toSDoc :: (Functor m, HasDynFlags m) => Outputable a => a -> m SDoc
toSDoc = fmap text . outputString

data BoxType = BoxType
  {  boxedName   :: String
  ,  constructor :: String
  }

getBoxType :: Kind -> Maybe BoxType
getBoxType k = case getPrimitiveTypeName k of
  Just "$tcFloat#"  -> Just $ BoxType "Float" "F#"
  Just "$tcDouble#" -> Just $ BoxType "Double" "D#"
  _                 -> Nothing

-- | Renders a single side of the rule like "fn_lhs_NAME :: ... \n fn_lhs_NAME ... = ..."
--
-- IMPORTANT! This function also does a bit of manipulation to convert boxed
-- types into unboxed versions and back. This is kind of a hack, necessary to
-- get around restrictions on usages of unboxed types (the type (Float#, Float#)
-- is not allowed, for example)
ruleSideDoc :: (Rule, Int) -> RuleSide -> SDoc
ruleSideDoc (rule, idx) side =
  let
    prefix    = "fn_" ++ sideString side ++ "_"
    name      = prefix ++ sanitizeName (ruleName rule) idx
    args      = valArgs rule
    argTypes  = asTuple (map (asBoxedType . varType) args)
    resultTyp = asBoxedType (ruleType rule)
  in
    text name <+> text "::" <+> argTypes <+> arrow <+> resultTyp $+$
    text name <+> asTuple (map asBoxedArg args) <+> text "=" <+> maybeBoxedBody
  where
    body = getSide side rule
    maybeBoxedBody :: SDoc
    maybeBoxedBody = case getBoxType (ruleType rule) of
      (Just (BoxType _ c)) -> text c <+> parens (ppr body)
      _                    -> ppr body
    asBoxedArg :: Var -> SDoc
    asBoxedArg v | Just (BoxType _ c) <- getBoxType (varType v)
                 = parens (text c <+> ppr v)
    asBoxedArg v = ppr v

    asBoxedType :: Kind -> SDoc
    asBoxedType k | Just (BoxType c _) <- getBoxType k = text c
    asBoxedType k = ppr k

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
     text nameRule <+> text "= SomeTestableRule" <+> parens (text "TestableRule" <+> text nameLhs <+> text nameRhs)

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
    f r = comment $+$ lhs $+$ rhs $+$ rulePairDoc r $+$ ruleTestDoc r $+$ blankLine $+$ blankLine
      where
        comment = text "{- Test for Rule: " $+$ origRule (fst r) $+$ text "-}"
        lhs = ruleSideDoc r LHS
        rhs = ruleSideDoc r RHS

testingImports :: String
testingImports =
  intercalate ", "
    [ "SomeTestableRule(..)"
    , "TestableRule (..)"
    , "testSomeTestableRule"
    ]
