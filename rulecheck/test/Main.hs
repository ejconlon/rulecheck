module Main (main) where

import Control.Monad.IO.Class (liftIO)
import Data.Maybe
import GHC.Plugins (varType, unLoc)
import Test.Tasty (DependencyType(..), after, TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Language.Haskell.TH.Syntax (Q, Exp (..), Dec (..), Body (..), Pat (..), Lit (..), Clause (..), newName, runQ, mkName)
import Rulecheck.Rendering (convertAndRender, outputString)
import Rulecheck.Monad (runGhcM)
import Rulecheck.Parsing (getParsedRuleDecls, parseModule, fakeFilePath)
import Rulecheck.Typecheck
import Rulecheck.Rule

-- For debugging
-- import System.Log.Logger

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
  actualOutput <- runGhcM (convertAndRender decls) ()
  actualOutput @?= expectedOutput

testParse :: TestTree
testParse = testCase "parse" $ do
  let contents = "module Foo where foo = \\ x_0 -> (x_0 + 1)"
  _pmod <- flip runGhcM () $ do
    pmod <- parseModule fakeFilePath contents
    outputString pmod >>= liftIO . putStrLn
    pure pmod
  -- TODO assert something about the parsed module
  pure ()

testGetParsedRuleDecls :: TestTree
testGetParsedRuleDecls = testCase "getParsedRuleDecls" $ do
  contents <- readFile "../demo-domain/src/DemoDomain.hs"
  [expected1, expected2] <- flip runGhcM () $ do
    pmod <- parseModule fakeFilePath contents
    let rules =  getParsedRuleDecls (unLoc pmod)
    mapM outputString rules
  expected1 @?= "{-# RULES \"mul1\" forall x. x .* Const 1 = x #-}"
  expected2 @?= "{-# RULES \"div_id\" forall x. x ./ x = Const 1 #-}"

testGetNameUnsafe :: TestTree
testGetNameUnsafe = testCase "getName" $ do
  tcm <- runGhcM (typecheck "../demo-domain/src/DemoDomain.hs") ()
  isJust (getNameUnsafe tcm ".*") @?= True
  isJust (getNameUnsafe tcm "NONEXISTANT") @?= False

testGetTypeForNameUnsafe :: TestTree
testGetTypeForNameUnsafe = testCase "getTypeForName" $ do
  expected <- flip runGhcM () $ do
    tcm      <- typecheck "../demo-domain/src/DemoDomain.hs"
    typ      <- getTypeForNameUnsafe tcm ".*"
    outputString (fromJust typ)
  expected @?= "main:DemoDomain.Expr\n-> main:DemoDomain.Expr -> main:DemoDomain.Expr"

testGetRule :: TestTree
testGetRule = testCase "getRule" $ do
  flip runGhcM () $ do
    tcm      <- typecheck "../demo-domain/src/DemoDomain.hs"
    let [rule1, _] = getTypecheckedRuleDecls tcm
    rule <- ruleFromDecl rule1
    let [arg] = ruleArgs rule
    arg'    <- outputString arg -- A unique mangled name
    argTyp  <- outputString (varType arg)
    lhs'    <- outputString (ruleLHS rule)
    rhs'    <- outputString (ruleRHS rule)
    retType <- outputString (ruleLHSType rule)
    liftIO $ argTyp   @?= "main:DemoDomain.Expr"
    liftIO $ lhs' @?= (arg' ++ " main:DemoDomain../ " ++ arg')
    liftIO $ rhs' @?= "main:DemoDomain.Const 1"
    liftIO $ retType @?= "main:DemoDomain.Expr"

testSketchTestFunction :: TestTree
testSketchTestFunction = testCase "sketchTestFunction" $ do
  flip runGhcM () $ do
    tcm      <- typecheck "../demo-domain/src/DemoDomain.hs"
    let [rule1, _] = getTypecheckedRuleDecls tcm
    rule <- ruleFromDecl rule1
    let [arg] = ruleArgs rule
    arg'    <- outputString arg -- A unique mangled name
    tf      <- outputString (sketchTestFunction rule LHS)
    let [sig, body] = lines tf
    liftIO $ sig  @?= "test :: main:DemoDomain.Expr -> main:DemoDomain.Expr"
    liftIO $ body @?= ("test " ++ arg' ++ " = " ++ arg' ++ " main:DemoDomain../ " ++ arg')

main :: IO ()
main = defaultMain $ testGroup "Rulecheck"
  [ testRender
  , testParse
  , testGetParsedRuleDecls
  , testGetRule
  -- Tests involving typechecking on the same module cannot be run concurrently
  , after AllFinish "getRule" testSketchTestFunction
  ]
