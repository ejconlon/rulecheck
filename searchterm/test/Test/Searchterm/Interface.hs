{-# LANGUAGE OverloadedStrings #-}

module Test.Searchterm.Interface (testInterface) where

import Control.Exception (throwIO)
import Control.Monad (when)
import Data.Foldable (for_)
import qualified Data.Sequence as Seq
import Data.Text (Text)
import Prettyprinter (pretty)
import Searchterm.Interface.Core (ConPat (..), Forall (..), Inst (..), Lit (..), Pat (..), PatPair (..),
                                  Strained (..), Tm (..), TmName, TmVar, Ty (..), TyScheme, TyVar, KindAnno (..), Kind (..), ClsName, Cls (..), InstScheme, ClsScheme)
import Searchterm.Interface.ParenPretty (docToText)
import Searchterm.Interface.Parser (parseLine, parseLines, parseLinesIO, parseTerm, parseType)
import Searchterm.Interface.Printer (printLines)
import Searchterm.Interface.Types (Line (..), LitLine (..), ModLine (..), FuncLine (FuncLine), ClsLine (..), InstLine (..))
import Searchterm.Util (dieOnParseErr)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Searchterm.Interface.Core (TyScheme(..))
import Searchterm.Interface.Core (ClsScheme(..))
import Searchterm.Interface.Core (InstScheme(..))

testInterface :: TestTree
testInterface = testGroup "interface"
  [ testParseTm
  , testParseTy
  , testParseLine
  , testBaseTxt
  , testPreludeTxt
  ]

assertParseFile :: FilePath -> IO ()
assertParseFile fn = do
  -- assert that we can parse, render, and parse again to get the same thing
  x <- parseLinesIO fn
  let y = printLines x
  z <- either throwIO pure (parseLines "<test>" y)
  if Seq.length z == Seq.length x
    then for_ (Seq.zip z x) (uncurry (@?=))
    else fail "mismatch lengths"

testBaseTxt :: TestTree
testBaseTxt = testCase "base.txt" $ do
  assertParseFile "../testdata/base.txt"

testPreludeTxt :: TestTree
testPreludeTxt = testCase "prelude.txt" $ do
  assertParseFile "../testdata/prelude.txt"

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
  assertParseTm "(((:) x) ([]))" $ TmApp (TmApp (TmFree "(:)") (TmFree "x")) (TmFree "([])")

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
  actualAst <- either dieOnParseErr pure (parseType expectedTxt)
  actualAst @?= expectedAst

mkF :: [KindAnno TyVar] -> [Inst TyVar] -> a -> Forall (KindAnno TyVar) (Strained TyVar a)
mkF tvs insts body = Forall (Seq.fromList tvs) (Strained (Seq.fromList insts) body)

mkTS :: [KindAnno TyVar] -> [Inst TyVar] -> Ty TyVar -> TyScheme TyVar
mkTS tvs insts body = TyScheme $ mkF tvs insts body

mkCS :: [KindAnno TyVar] -> [Inst TyVar] -> Cls TyVar -> ClsScheme TyVar
mkCS tvs insts body = ClsScheme $ mkF tvs insts body

mkIS :: [KindAnno TyVar] -> [Inst TyVar] -> Inst TyVar -> InstScheme TyVar
mkIS tvs insts body = InstScheme $ mkF tvs insts body

mkA :: [TyVar] -> [KindAnno TyVar]
mkA = fmap (`KindAnno` Nothing)

mkI :: ClsName -> [Ty TyVar] -> Inst TyVar
mkI cn tas = Inst cn (Seq.fromList tas)

testParseTy :: TestTree
testParseTy = testCase "parseTy" $ do
  assertParseTy True "Int" (mkTS [] [] (TyKnown "Int"))
  assertParseTy True "forall f a. f a" $
    mkTS (mkA ["f", "a"]) [] (tyAppFree "f" [TyFree "a"])
  assertParseTy True "forall a. ([]) a" $
    mkTS (mkA ["a"]) [] (tyAppKnown "([])" [TyFree "a"])
  assertParseTy False "forall a. [a]" $
    mkTS (mkA ["a"]) [] (tyAppKnown "([])" [TyFree "a"])
  assertParseTy True "forall a b. (,) a b" $
    mkTS (mkA ["a", "b"]) [] (tyAppKnown "(,)" [TyFree "a", TyFree "b"])
  assertParseTy False "forall a b. (a, b)" $
    mkTS (mkA ["a", "b"]) [] (tyAppKnown "(,)" [TyFree "a", TyFree "b"])
  assertParseTy True "forall a b c d. (,,,) a b c d" $
    mkTS (mkA ["a", "b", "c", "d"]) [] (tyAppKnown "(,,,)" (fmap TyFree ["a", "b", "c", "d"]))
  assertParseTy False "forall a b c d. (a, b, c, d)" $
    mkTS (mkA ["a", "b", "c", "d"]) [] (tyAppKnown "(,,,)" (fmap TyFree ["a", "b", "c", "d"]))
  assertParseTy True "()" (mkTS [] [] (TyKnown "()"))
  assertParseTy True "forall a. Show a => a -> String" $
    mkTS (mkA ["a"]) [mkI "Show" [TyFree "a"]] (TyFun (TyFree "a") (TyKnown "String"))
  assertParseTy True "forall (m :: Type -> Type) (a :: Type). a -> m a" $
    mkTS
      [KindAnno "m" (Just (KindTyCon (Seq.singleton KindTy))), KindAnno "a" (Just KindTy)]
      [] (TyFun (TyFree "a") (TyApp (TyFree "m") (Seq.singleton (TyFree "a"))))
  where
    tyAppKnown s xs = TyApp (TyKnown s) (Seq.fromList xs)
    tyAppFree s xs = TyApp (TyFree s) (Seq.fromList xs)

assertParseLineSame :: Text -> Line -> IO ()
assertParseLineSame expectedTxt expectedLine = do
  let actualTxt = docToText (pretty expectedLine)
  actualTxt @?= expectedTxt
  actualLine <- either dieOnParseErr pure (parseLine "<test>" expectedTxt)
  actualLine @?= expectedLine

assertParseLineCanon :: Text -> Text -> Line -> IO ()
assertParseLineCanon firstTxt expectedTxt expectedLine = do
  assertParseLineSame expectedTxt expectedLine
  firstLine <- either dieOnParseErr pure (parseLine "<test>" firstTxt)
  firstLine @?= expectedLine

testParseLine :: TestTree
testParseLine = testCase "parseLine" $ do
  assertParseLineSame "module Foo.Bar" $
    LineMod (ModLine "Foo.Bar")
  assertParseLineSame "literals Int 0 -1 2" $
    LineLit (LitLine "Int" (Seq.fromList (fmap LitInteger [0, -1, 2])))
  assertParseLineSame "literals Char 'a' '_'" $
    LineLit (LitLine "Char" (Seq.fromList (fmap LitChar ['a', '_'])))
  assertParseLineSame "literals String \"foo\" \"\"" $
    LineLit (LitLine "String" (Seq.fromList (fmap LitString ["foo", ""])))
  assertParseLineSame "literals Double 0.1 -1.0" $
    LineLit (LitLine "Double" (Seq.fromList (fmap LitScientific [read "0.1", read "-1.0"])))
  assertParseLineSame "foo :: Int" $
    LineFunc (FuncLine "foo" (mkTS [] [] (TyKnown "Int")))
  assertParseLineSame "(<*>) :: Int -> Double" $
    LineFunc (FuncLine "(<*>)" (mkTS [] [] (TyFun (TyKnown "Int") (TyKnown "Double"))))
  assertParseLineCanon "x :: a -> Int" "x :: forall a. a -> Int" $
    LineFunc (FuncLine "x" (mkTS (mkA ["a"]) [] (TyFun (TyFree "a") (TyKnown "Int"))))
  assertParseLineSame "class Foo a" $
    LineCls (ClsLine (mkCS (mkA ["a"]) [] (Cls "Foo" (Seq.singleton "a"))))
  assertParseLineSame "class Foo (f :: Type -> Type)" $
    LineCls (ClsLine (mkCS [KindAnno "f" (Just (KindTyCon (Seq.singleton KindTy)))] [] (Cls "Foo" (Seq.singleton "f"))))
  assertParseLineSame "instance Foo Int" $
    LineInst (InstLine (mkIS [] [] (Inst "Foo" (Seq.singleton (TyKnown "Int")))))
  assertParseLineSame "instance forall a. Foo a" $
    LineInst (InstLine (mkIS (mkA ["a"]) [] (Inst "Foo" (Seq.singleton (TyFree "a")))))
  assertParseLineSame "instance forall (f :: Type -> Type). Foo f" $
    LineInst (InstLine (mkIS [KindAnno "f" (Just (KindTyCon (Seq.singleton KindTy)))] [] (Inst "Foo" (Seq.singleton (TyFree "f")))))
