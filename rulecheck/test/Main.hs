module Main (main) where

import Control.Monad.IO.Class (liftIO)
import Data.Maybe
import GHC (getLoc, TyThing(..))
import GHC.Plugins (varType, showSDocUnsafe, ppr, unLoc)
import Test.Tasty (DependencyType(..), TestTree, after, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Language.Haskell.TH.Syntax (Q, Exp (..), Dec (..), Body (..), Pat (..), Lit (..), Clause (..), newName, runQ, mkName)
import Rulecheck.Rendering (convertAndRender, outputString)
import Rulecheck.Monad (runGhcM)
import Rulecheck.Parsing (getRules, parseModule, fakeFilePath)
import Rulecheck.Typecheck

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

testGetRules :: TestTree
testGetRules = testCase "getRules" $ do
  contents <- readFile "../demo-domain/src/DemoDomain.hs"
  [expected1, expected2] <- flip runGhcM () $ do
    pmod <- parseModule fakeFilePath contents
    let rules =  getRules (unLoc pmod)
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

testGetTypecheckedRules :: TestTree
testGetTypecheckedRules = testCase "getTypecheckedRules" $ do
  -- For debugging
  -- updateGlobalLogger "hie-bios" $ setLevel DEBUG
  expected <- flip runGhcM () $ do
    tcm      <- typecheck "../demo-domain/src/DemoDomain.hs"
    let rules = getTypecheckedRules tcm
    mapM outputString rules
  mapM_ putStrLn expected

testGetRuleBody :: TestTree
testGetRuleBody = testCase "getRuleBody" $ do
  flip runGhcM () $ do
    tcm      <- typecheck "../demo-domain/src/DemoDomain.hs"
    let [rule1, _] = getTypecheckedRules tcm
    let [arg]     = getRuleArguments rule1
    let (lhs, rhs) = getRuleBody rule1
    arg' <- outputString arg
    lhs' <- outputString lhs
    rhs' <- outputString rhs
    -- If this fails, make this test less fragile
    liftIO $ lhs' @?= (arg' ++ " main:DemoDomain../ " ++ arg')
    liftIO $ rhs' @?= "main:DemoDomain.Const 1"

testGetRuleArguments :: TestTree
testGetRuleArguments = testCase "getRuleArguments" $ do
  flip runGhcM () $ do
    tcm      <- typecheck "../demo-domain/src/DemoDomain.hs"
    let [rule1, _] = getTypecheckedRules tcm
    let [rule1Arg] = getRuleArguments rule1
    ident <- outputString rule1Arg
    typ   <- outputString $ varType rule1Arg
    liftIO $ typ   @?= "main:DemoDomain.Expr"

main :: IO ()
main = defaultMain $ testGroup "Rulecheck"
  [ testRender
  , testParse
  , testGetRules
  -- Tests invoking typechecker must run sequentially
  , testGetRuleBody
  , after AllSucceed "getRuleBody" testGetRuleArguments
  , after AllSucceed "getRuleArguments" testGetNameUnsafe
  , after AllSucceed "getName" testGetTypeForNameUnsafe
  ]
