{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Test.Rulecheck (testRulecheck) where

import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromJust, isJust)
import GHC.Plugins (unLoc, varType)
import Language.Haskell.TH.Syntax (Body (..), Clause (..), Dec (..), Exp (..), Lit (..), Pat (..), Q, mkName, newName,
                                   runQ)
import Rulecheck.Monad (cradleGhcM, runGhcM)
import Rulecheck.Parsing (fakeFilePath, getParsedRuleDecls, parseModule)
import Rulecheck.Rendering (convertAndRender, outputString)
import Rulecheck.Rule (Rule (..), RuleSide (..), ruleFromDecl, ruleSideDoc)
import Rulecheck.Typecheck (getNameUnsafe, getTypeForNameUnsafe, getTypecheckedRuleDecls, typecheck)
import Test.Tasty (DependencyType (..), TestTree, after, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

-- Example from here: https://markkarpov.com/tutorial/th.html
mkFunExp :: Q Exp
mkFunExp = do
  x <- newName "x"
  return $ LamE
    [VarP x]
    (InfixE (Just (VarE x)) (VarE (mkName "+")) (Just (LitE (IntegerL 1))))

mkFunDecls :: Q [Dec]
mkFunDecls = do
  funExp <- mkFunExp
  let funDec = FunD (mkName "foo") [Clause [] (NormalB funExp) []]
  pure [funDec]

testRender :: TestTree
testRender = testCase "render" $ do
  let expectedOutput = "foo = \\ x_0 -> (x_0 + 1)"
  decls <- runQ mkFunDecls
  actualOutput <- runGhcM (convertAndRender decls)
  actualOutput @?= expectedOutput

testParse :: TestTree
testParse = testCase "parse" $ do
  let contents = "module Foo where foo = \\ x_0 -> (x_0 + 1)"
  _pmod <- runGhcM $ do
    pmod <- parseModule fakeFilePath contents
    outputString pmod >>= liftIO . putStrLn
    pure pmod
  -- TODO assert something about the parsed module
  pure ()

demoDomainFile :: FilePath
demoDomainFile = "../demo-domain/src/DemoDomain.hs"

testGetParsedRuleDecls :: TestTree
testGetParsedRuleDecls = testCase "getParsedRuleDecls" $ do
  contents <- readFile demoDomainFile
  [expected1, expected2] <- runGhcM $ do
    pmod <- parseModule fakeFilePath contents
    let rules = getParsedRuleDecls (unLoc pmod)
    mapM outputString rules
  expected1 @?= "{-# RULES \"mul1\" forall x. x .* Const 1 = x #-}"
  expected2 @?= "{-# RULES \"div_id\" forall x. x ./ x = Const 1 #-}"

testGetNameUnsafe :: TestTree
testGetNameUnsafe = testCase "getName" $ do
  tcm <- cradleGhcM demoDomainFile (typecheck demoDomainFile)
  isJust (getNameUnsafe tcm ".*") @?= True
  isJust (getNameUnsafe tcm "NONEXISTANT") @?= False

testGetTypeForNameUnsafe :: TestTree
testGetTypeForNameUnsafe = testCase "getTypeForName" $ do
  expected <- cradleGhcM demoDomainFile$ do
    tcm      <- typecheck demoDomainFile
    typ      <- getTypeForNameUnsafe tcm ".*"
    outputString (fromJust typ)
  expected @?= "main:DemoDomain.Expr\n-> main:DemoDomain.Expr -> main:DemoDomain.Expr"

testGetRule :: TestTree
testGetRule = testCase "getRule" $ do
  cradleGhcM demoDomainFile $ do
    tcm      <- typecheck demoDomainFile
    let [rule1, _] = getTypecheckedRuleDecls tcm
    rule <- ruleFromDecl rule1
    let [arg] = ruleArgs rule
    arg'    <- outputString arg -- A unique mangled name
    argTyp  <- outputString (varType arg)
    lhs'    <- outputString (ruleLHS rule)
    rhs'    <- outputString (ruleRHS rule)
    retType <- outputString (ruleType rule)
    liftIO $ argTyp   @?= "main:DemoDomain.Expr"
    liftIO $ lhs' @?= (arg' ++ " main:DemoDomain../ " ++ arg')
    liftIO $ rhs' @?= "main:DemoDomain.Const 1"
    liftIO $ retType @?= "main:DemoDomain.Expr"

testRuleSideDoc :: TestTree
testRuleSideDoc = testCase "ruleSidedoc" $ do
  cradleGhcM demoDomainFile $ do
    tcm <- typecheck demoDomainFile
    let [rule1, _] = getTypecheckedRuleDecls tcm
    rule <- ruleFromDecl rule1
    let [arg] = ruleArgs rule
    arg' <- outputString arg -- A unique mangled name
    doc <- ruleSideDoc rule LHS
    tf <- outputString doc
    let [sig, body] = lines tf
    liftIO $ sig  @?= "fn_lhs_divzuid :: DemoDomain.Expr -> DemoDomain.Expr"
    liftIO $ body @?= ("fn_lhs_divzuid " ++ arg' ++ " = " ++ arg' ++ " DemoDomain../ " ++ arg')

testRulecheck :: TestTree
testRulecheck = testGroup "Rulecheck"
  [ testRender
  , testParse
  , testGetParsedRuleDecls
  , testGetRule
  -- Tests involving typechecking on the same module cannot be run concurrently
  , after AllFinish "getRule" testRuleSideDoc
  ]
