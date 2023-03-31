{-# LANGUAGE OverloadedStrings #-}

module Test.Searchterm.NewInterface (testNewInterface) where

import Test.Tasty (TestTree)
import Test.Tasty.HUnit ((@?=), testCase)
import Searchterm.Grammar.Gen.Abs (Line (..), Lit (..))
import Searchterm.Grammar.Manual.NewInterface (Grammatical (..))
import Data.Text (Text)

assertParseLine :: Text -> Line -> IO ()
assertParseLine txt expected = do
  let retxt = printGram expected
  retxt @?= txt
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
