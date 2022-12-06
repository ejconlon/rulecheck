{-# LANGUAGE OverloadedStrings #-}

module Test.Searchterm.Interface (testInterface) where

import Control.Monad (when)
import Control.Exception (throwIO)
import Data.Foldable (for_)
import qualified Data.Sequence as Seq
import Searchterm.Interface.Parser (parseLines, parseLinesIO, parseTerm, parseLine, parseType)
import Searchterm.Interface.Printer (printLines)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Data.Text (Text)
import Searchterm.Interface.Core (TmVar, Tm (..), TmName, PatPair (..), ConPat (..), Pat (..), Lit (..), TyVar, TyScheme (..), Ty (..), Inst, Strained (..), Forall (..), ConTy (..))
import Prettyprinter (pretty)
import Searchterm.Interface.ParenPretty (docToText)
import Searchterm.Interface.Types (Line (..), LitLine (..))
import Data.Sequence (Seq(..))

testInterface :: TestTree
testInterface = testGroup "interface"
  [ testParseTm
  , testParseTy
  , testParseLine
  , testBaseTxt
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

  assertParseTm "([])" $ TmFree "([])"
  assertParseTm "(((:) x) y)" $ TmApp (TmApp (TmFree "(:)") (TmFree "x")) (TmFree "y")

  assertParseTm "(((,) a) b)" $ TmApp (TmApp (TmFree "(,)") (TmFree "a")) (TmFree "b")
  assertParseTm "(((((,,,) a) b) c) d)" $
    TmApp (TmApp (TmApp (TmApp (TmFree "(,,,)") (TmFree "a")) (TmFree "b")) (TmFree "c")) (TmFree "d")
  assertParseTm "()" (TmFree "()")

  assertParseTm "(case x of { Bar y -> x ; Baz z -> z })" $
    TmCase (TmFree "x") (Seq.fromList [mkPP "Bar" ["y"] (TmFree "x"), mkPP "Baz" ["z"] (TmFree "z")])

  assertParseTm "(let x = y in x)" (TmLet "x" (TmFree "y") (TmFree "x"))

  assertParseTm "(\\x -> (case x of { Left y -> (showChar y) ; Right z -> (showInt z) }))" $
    TmLam "x"  $ TmCase (TmFree "x") $ Seq.fromList
      [ mkPP "Left" ["y"] (TmApp (TmFree "showChar") (TmFree "y"))
      , mkPP "Right" ["z"] (TmApp (TmFree "showInt") (TmFree "z"))
      ]

  assertParseTm "(\\f -> (\\s -> (isEven (f s))))" $
    TmLam "f" (TmLam "s" (TmApp (TmFree "isEven") (TmApp (TmFree "f") (TmFree "s"))))

  assertParseTm "0" (TmLit (LitInteger 0))
  assertParseTm "-1" (TmLit (LitInteger (-1)))
  assertParseTm "0.1" (TmLit (LitScientific (read "0.1")))
  assertParseTm "-0.1" (TmLit (LitScientific (read "-0.1")))
  assertParseTm "'c'" (TmLit (LitChar 'c'))
  assertParseTm "\"foo\"" (TmLit (LitString "foo"))

assertParseTy :: Bool -> Text -> TyScheme TyVar -> IO ()
assertParseTy roundtrip expectedTxt expectedAst = do
  when roundtrip $ do
    let actualTxt = docToText (pretty expectedAst)
    actualTxt @?= expectedTxt
  actualAst <- either throwIO pure (parseType expectedTxt)
  actualAst @?= expectedAst

mkS :: [TyVar] -> [Inst TyVar] -> Ty TyVar -> TyScheme TyVar
mkS tvs insts body = TyScheme (Forall (Seq.fromList tvs) (Strained (Seq.fromList insts) body))

testParseTy :: TestTree
testParseTy = testCase "parseTy" $ do
  assertParseTy True "Int" (mkS [] [] (TyCon (ConTyKnown "Int") Empty))
  assertParseTy True "forall a. ([]) a" $
    mkS ["a"] [] (TyCon (ConTyKnown "([])") (Seq.singleton (TyFree "a")))
  assertParseTy False "forall a. [a]" $
    mkS ["a"] [] (TyCon (ConTyKnown "([])") (Seq.singleton (TyFree "a")))
  assertParseTy True "forall a b. (,) a b" $
    mkS ["a", "b"] [] (TyCon (ConTyKnown "(,)") (Seq.fromList [TyFree "a", TyFree "b"]))
  assertParseTy False "forall a b. (a, b)" $
    mkS ["a", "b"] [] (TyCon (ConTyKnown "(,)") (Seq.fromList [TyFree "a", TyFree "b"]))
  assertParseTy True "forall a b c d. (,,,) a b c d" $
    mkS ["a", "b", "c", "d"] [] $ TyCon (ConTyKnown "(,,,)") $
      Seq.fromList (fmap TyFree ["a", "b", "c", "d"])
  assertParseTy False "forall a b c d. (a, b, c, d)" $
    mkS ["a", "b", "c", "d"] [] $ TyCon (ConTyKnown "(,,,)") $
      Seq.fromList (fmap TyFree ["a", "b", "c", "d"])
  assertParseTy True "()" (mkS [] [] (TyCon (ConTyKnown "()") Empty))
  assertParseTy True "forall a b q. (a -> b) -> q a -> q b" $ mkS ["a", "b", "q"] [] $
    TyFun
      (TyFun (TyFree "a") (TyFree "b"))
      (TyFun
        (TyCon (ConTyFree "q") (Seq.singleton (TyFree "a")))
        (TyCon (ConTyFree "q") (Seq.singleton (TyFree "b"))))

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
