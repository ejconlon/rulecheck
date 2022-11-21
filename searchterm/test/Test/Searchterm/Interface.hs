{-# LANGUAGE OverloadedStrings #-}

module Test.Searchterm.Interface (testInterface) where

import Control.Exception (throwIO)
import Data.Foldable (for_)
import qualified Data.Sequence as Seq
import Searchterm.Interface.Parser (parseLines, parseLinesIO, parseTerm, parseLine)
import Searchterm.Interface.Printer (printLines)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Data.Text (Text)
import Searchterm.Interface.Core (TmVar, Tm (..), TmName, PatPair (..), ConPat (..), Pat (..), Lit (..))
import Prettyprinter (pretty)
import Searchterm.Interface.ParenPretty (docToText)
import Searchterm.Interface.Types (Line (..), LitLine (..))

testInterface :: TestTree
testInterface = testGroup "interface"
  [ testBaseTxt
  , testParseTm
  , testParseLine
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

testParseTm :: TestTree
testParseTm = testCase "parseTm" $ do
  assertParseTm "foo" (TmFree "foo")

  assertParseTm "(case x of { Bar y -> x ; Baz z -> z })" $
    TmCase (TmFree "x") (Seq.fromList [mkPP "Bar" ["y"] (TmFree "x"), mkPP "Baz" ["z"] (TmFree "z")])

  assertParseTm "(let x = y in x)" (TmLet "x" (TmFree "y") (TmFree "x"))

  assertParseTm "(\\x -> (case x of { Left y -> (showChar y) ; Right z -> (showInt z) }))" $
    TmLam "x"  $ TmCase (TmFree "x") $ Seq.fromList
      [ mkPP "Left" ["y"] (TmApp (TmFree "showChar") (TmFree "y"))
      , mkPP "Right" ["z"] (TmApp (TmFree "showInt") (TmFree "z"))
      ]

assertParseLine :: Text -> Line -> IO ()
assertParseLine expectedTxt expectedLine = do
  let actualTxt = docToText (pretty expectedLine)
  actualTxt @?= expectedTxt
  actualLine <- either throwIO pure (parseLine "<test>" expectedTxt)
  actualLine @?= expectedLine

testParseLine :: TestTree
testParseLine = testCase "parseLine" $ do
  assertParseLine "literals Int 0 -1 2" (LineLit (LitLine "Int" (Seq.fromList (fmap LitInteger [0, -1, 2]))))
  assertParseLine "literals Char 'a' '_'" (LineLit (LitLine "Char" (Seq.fromList (fmap LitChar ['a', '_']))))
  assertParseLine "literals String \"foo\" \"\"" (LineLit (LitLine "String" (Seq.fromList (fmap LitString ["foo", ""]))))
  assertParseLine "literals Double 0.1 -1.0" (LineLit (LitLine "Double" (Seq.fromList (fmap LitScientific [read "0.1", read "-1.0"]))))
