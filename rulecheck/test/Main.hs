module Main (main) where

import Control.Monad.IO.Class (liftIO)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Language.Haskell.TH.Syntax (Q, Exp (..), Dec (..), Body (..), Pat (..), Lit (..), Clause (..), newName, runQ, mkName)
import Rulecheck.Rendering (convertAndRender, outputString)
import Rulecheck.Monad (runGhcM)
import Rulecheck.Parsing (parseModule, fakeFilePath)

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
  pmod <- flip runGhcM () $ do
    pmod <- parseModule fakeFilePath contents
    outputString pmod >>= liftIO . putStrLn
    pure pmod
  -- TODO assert something about the parsed module
  pure ()

main :: IO ()
main = defaultMain $ testGroup "Rulecheck"
  [ testRender
  , testParse
  ]
