{-# LANGUAGE OverloadedStrings #-}

module Test.Searchterm.NewInterface (testNewInterface) where

import Test.Tasty (TestTree)
import Test.Tasty.HUnit ((@?=), testCase)
import Searchterm.Grammar.Gen.Abs (Line (..), Lit (..), FuncSig (..), Straints (..), TyName (..), InstName (..))
import Searchterm.Grammar.Manual.NewInterface (Grammatical (..))
import Data.Text (Text)
import Searchterm.Grammar.Gen.Par (myLexer)

assertParseLine :: Text -> Line -> IO ()
assertParseLine txt expected = do
  let retxt = printGram expected
  retxt @?= txt
  print (myLexer txt)
  case parseGram @Line txt of
    Left err -> fail ("Failed to parse: " <> err)
    Right actual -> actual @?= expected

testNewInterface :: TestTree
testNewInterface = testCase "new interface" $ do
  assertParseLine "module Foo.Bar" (LineMod "Foo.Bar")
  assertParseLine "literals Int 0 -1 2" (LineLit "Int" (fmap LitInteger ["0", "-1", "2"]))
  assertParseLine "literals Char 'a' '_'" (LineLit "Char" (fmap LitChar ['a', '_']))
  assertParseLine "literals String \"foo\" \"\"" (LineLit "String" (fmap LitString ["foo", ""]))
  assertParseLine "literals Double 0.1 -1.0" (LineLit "Double" (fmap LitScientific ["0.1", "-1.0"]))
  assertParseLine "foo :: Int" (LineFunc "foo" (FuncSigBase "Int"))
  assertParseLine "(<*>) :: Int -> Double" (LineFunc "(<*>)" (FuncSigArr (FuncSigBase "Int") (FuncSigBase "Double")))
  assertParseLine "x :: (a -> b) -> (c -> d)" (LineFunc "x" (FuncSigArr (FuncSigParen (FuncSigArr (FuncSigBase "a") (FuncSigBase "b"))) (FuncSigParen (FuncSigArr (FuncSigBase "c") (FuncSigBase "d")))))
  assertParseLine "instance Foo" (LineInst StraintsNone (InstName "Foo")) -- (TyNameBase "Int")))
