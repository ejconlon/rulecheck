{-# LANGUAGE OverloadedStrings #-}

module Test.Searchterm.Interface (testInterface) where

import Control.Exception (throwIO)
import Data.Foldable (for_)
import qualified Data.Sequence as Seq
import Searchterm.Interface.Parser (parseLines, parseLinesIO, parseTerm)
import Searchterm.Interface.Printer (printLines)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Data.Text (Text)
import Searchterm.Interface.Core (TmVar, Tm (..), TmName, PatPair (..), ConPat (..), Pat (..))
import Prettyprinter (pretty)
import Searchterm.Interface.ParenPretty (docToText)

testInterface :: TestTree
testInterface = testGroup "interface"
  [ testBaseTxt
  , testParse
  ]

testBaseTxt :: TestTree
testBaseTxt = testCase "base.txt" $ do
  -- assert that we can parse, render, and parse again to get the same thing
  x <- parseLinesIO "../testdata/base.txt"
  let y = printLines x
  z <- either throwIO pure (parseLines "<test>" y)
  if Seq.length z == Seq.length x
    then for_ (Seq.zip z x) (uncurry (@?=))
    else fail "mismatch lengths"

assertParseTm :: Text -> Tm TmVar TmVar -> IO ()
assertParseTm expectedTxt expectedAst = do
  let actualTxt = docToText (pretty expectedAst)
  actualTxt @?= expectedTxt
  actualAst <- either throwIO pure (parseTerm expectedTxt)
  actualAst @?= expectedAst

mkPP :: TmName -> [TmVar] -> Tm TmVar TmVar -> PatPair TmVar (Tm TmVar TmVar)
mkPP cn = PatPair . Pat . ConPat cn . Seq.fromList

testParse :: TestTree
testParse = testCase "parse" $ do
  assertParseTm "foo" (TmFree "foo")

  assertParseTm "(case x of { Bar y -> x ; Baz z -> z })" $
    TmCase (TmFree "x") (Seq.fromList [mkPP "Bar" ["y"] (TmFree "x"), mkPP "Baz" ["z"] (TmFree "z")])

  assertParseTm "(let x = y in x)" (TmLet "x" (TmFree "y") (TmFree "x"))
