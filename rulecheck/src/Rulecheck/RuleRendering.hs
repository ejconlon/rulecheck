-- | Logic for rendering rules and test modules

module Rulecheck.RuleRendering
  ( ruleInputDoc
  , ruleSideDoc
  , rulePairDoc
  , ruleTestDoc
  , ruleModuleHeaderDoc
  , ruleModuleDoc
  , TestModuleRenderOpts(..)
  , TestSuffix(..)
  ) where

import Data.Char ( isAlphaNum )
import Data.Foldable ( foldl', toList )
import Data.Set as Set ( Set )
import Data.List ( intercalate )
import Data.List.NonEmpty ( NonEmpty(..), nonEmpty )
import GHC.Core.Type ( Kind, Var(..), noFreeVarsOfType )
import GHC.Data.FastString ( fs_zenc, zString )
import GHC.Driver.Session ( HasDynFlags )
import GHC.Utils.Outputable
    ( Outputable(ppr),
      SDoc,
      text,
      ($+$),
      (<+>),
      arrow,
      blankLine,
      empty,
      parens,
      pprWithCommas )
import GHC.Types.Basic ( RuleName )
import GHC.Tc.Utils.TcType ( isTyVarClassPred )
import Rulecheck.Rendering ( outputString )
import Rulecheck.Rule
    ( getBoxType,
      getSide,
      BoxType(BoxType),
      Rule(ruleTermArgs, ruleName, origRule, ruleType,
           ruleTermAndTyArgs),
      RuleSide(..) )

data TestSuffix =
  TestSuffix Int Int -- RuleNum, TestNum

asTuple :: Outputable a => NonEmpty a -> SDoc
asTuple (el :| [])    = ppr el
asTuple (el :| rest)  = parens $ pprWithCommas ppr (el : rest)

asBoxedType :: Kind -> SDoc
asBoxedType k | Just (BoxType c _) <- getBoxType k = text c
asBoxedType k = ppr k

ruleInputDoc :: Rule -> SDoc
ruleInputDoc rule =
  if not (null args) && not (null constraintArgs)
    then constraintPart <+> text "=>" <+> argsPart
    else constraintPart <+> argsPart
  where
    args           = ruleTermArgs rule
    constraintArgs = filter (isTyVarClassPred . varType) $ ruleTermAndTyArgs rule
    constraintPart =
      maybe empty asTuple (nonEmpty (map varType constraintArgs))
    argsPart =
      maybe empty asTuple (nonEmpty (map (asBoxedType . varType) args))

-- | Renders a single side of the rule like "fn_lhs_NAME :: ... \n fn_lhs_NAME ... = ..."
--
-- IMPORTANT! This function also does a bit of manipulation to convert boxed
-- types into unboxed versions and back. This is kind of a hack, necessary to
-- get around restrictions on usages of unboxed types (the type (Float#, Float#)
-- is not allowed, for example)
ruleSideDoc :: (Rule, TestSuffix) -> RuleSide -> Maybe String -> SDoc
ruleSideDoc (rule, idx) side overrideTypeSig =
  let
    prefix         =  "fn_" ++ sideString side ++ "_"
    name           = prefix ++ sanitizeName (ruleName rule) idx
    args           = ruleTermArgs rule
    constraintArgs = filter (isTyVarClassPred . varType) $ ruleTermAndTyArgs rule

    maybeBoxedArgs =
      maybe empty asTuple (nonEmpty (map asBoxedArg args))
    def       = text name <+> maybeBoxedArgs <+> text "=" <+> maybeBoxedBody

    resultTyp  = asBoxedType (ruleType rule)
    defaultSig =
      if null args && null constraintArgs
      then resultTyp
      else ruleInputDoc rule <+> arr <+> resultTyp
      where
        arr = if null args then text "=>" else arrow
  in
    text (name ++ " ::") <+> maybe defaultSig text overrideTypeSig $+$ def
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


-- | Renders the rule pair defn like "pair_NAME :: SomeTestableRule \n pair_NAME = ..."
rulePairDoc :: (Rule, TestSuffix) -> SDoc
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
ruleTestDoc :: (Rule, TestSuffix) -> SDoc
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
        text "import Rulecheck.Generated.ArbitraryInstances" $+$
        text "import Test.Tasty (TestTree)" $+$
        text ("import Rulecheck.Testing(" ++ testingImports ++ ")")
  in foldl' (\x d -> x $+$ text "import" <+> text d) start (toList deps)

data TestModuleRenderOpts = TestModuleRenderOpts
  { testModName       :: String
  , testImports       :: Set String
  , testTypeOverrides :: RuleName -> Set String
  }

-- | Renders the entire test module
ruleModuleDoc :: TestModuleRenderOpts -> [Rule] -> SDoc
ruleModuleDoc opts rules =
  foldl' (\x r -> x $+$ testsForRule r) headerDoc (zip rules [1..])
  where
    headerDoc = ruleModuleHeaderDoc (testModName opts) (testImports opts)

    -- Renders tests for a rule
    -- Note that one rule may have multiple tests (i.e. for different typeclass instances)
    testsForRule :: (Rule, Int) -> SDoc
    testsForRule r =
      case toList sigs of
        [] -> mkTest r Nothing
        xs -> foldl' (\x sig -> x $+$ mkTest r (Just sig)) empty (zip xs [1..])
      where
        sigs = testTypeOverrides opts (ruleName (fst r))

    mkTest :: (Rule, Int) -> Maybe (String, Int) -> SDoc
    mkTest (rule, ruleNum) sigOpt =
      comment $+$ lhs $+$ rhs $+$ rulePairDoc (rule, suffix) $+$ ruleTestDoc (rule, suffix) $+$ blankLine $+$ blankLine
      where
        testNum = maybe 1 snd sigOpt
        suffix  = TestSuffix ruleNum testNum
        comment = text "{- Test for Rule: " $+$ origRule rule
          $+$ commentArgValues
          $+$ text "Result ::" <+> typeComment (ruleType rule)
          $+$ text "-}"
        commentArgValues = foldl' ($+$) empty (map go (ruleTermAndTyArgs rule)) where
          go arg =  text "Arg" <+> ppr arg <+> text "::" <+> typeComment (varType arg)
        typeComment typ = ppr typ <+> text ("(closed: " ++ show (noFreeVarsOfType typ) ++ ")")
        lhs = ruleSideDoc (rule, suffix) LHS (fmap fst sigOpt)
        rhs = ruleSideDoc (rule, suffix) RHS (fmap fst sigOpt)

testingImports :: String
testingImports =
  intercalate ", "
    [ "SomeTestableRule(..)"
    , "TestableRule (..)"
    , "testSomeTestableRule"
    ]


toSDoc :: (Functor m, HasDynFlags m) => Outputable a => a -> m SDoc
toSDoc = fmap text . outputString

sanitizeString :: String -> String
sanitizeString = map (\c -> if isAlphaNum c then c else '_')

-- | Names can and often do contain characters that are not safe for identifiers.
-- We just replace those characters with underscores.
-- Also adds an index to the rule to ensure each name is unique
sanitizeName :: RuleName -> TestSuffix -> String
sanitizeName name (TestSuffix ruleNum testNum) =
  sanitizeString (zString (fs_zenc name)) ++ "_" ++ show ruleNum ++ "_" ++ show testNum

-- | Just a string prefix for a rule side
sideString :: RuleSide -> String
sideString = \case
  LHS -> "lhs"
  RHS -> "rhs"
