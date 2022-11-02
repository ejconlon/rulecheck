{-# LANGUAGE OverloadedStrings #-}

module Test.Rulecheck.Synth.Search (testSearch) where

import Data.Set (Set)
import qualified Data.Set as Set
import Rulecheck.Interface.Core (Tm, TmVar)
import Data.Text (Text)
import Rulecheck.Interface.Decl (DeclSet (..), namelessScheme, mkLineDecls)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)
import Test.Tasty.Providers (TestName)
import Rulecheck.Interface.Parser (parseScheme, parseTerm, parseLinesIO, parseLines)
import Rulecheck.Synth.Search (SearchConfig (..), SearchSusp, TmFound, runSearchSusp)
import Control.Exception (throwIO, Exception)
import Data.Foldable (toList)
import qualified Data.Text as T

rethrow :: Exception e => Either e a -> IO a
rethrow = either throwIO pure

data DeclSrc =
    DeclSrcFile !FilePath
  | DeclSrcList ![Text]
  deriving stock (Eq, Show)

loadDecls :: DeclSrc -> IO DeclSet
loadDecls src = do
  ls <- case src of
    DeclSrcFile fp -> parseLinesIO fp
    DeclSrcList ts -> rethrow (parseLines "<load>" (T.unlines ts))
  rethrow (mkLineDecls (toList ls))

maxSearchDepth :: Int
maxSearchDepth = 5

-- TODO find all terms
findAll :: Set (Tm TmVar TmVar) -> SearchSusp TmFound -> IO ()
findAll tms susp =
  if Set.null tms
    then pure ()
    else pure () -- TODO

testFinds :: TestName -> DeclSrc -> Text -> [Text] -> TestTree
testFinds n src schemeStr tmStrs = testCase n $ do
  ds <- loadDecls src
  schemeNamed <- either throwIO pure (parseScheme schemeStr)
  scheme <- either throwIO pure (namelessScheme schemeNamed)
  tms <- traverse (either throwIO pure . parseTerm) tmStrs
  let conf = SearchConfig ds scheme maxSearchDepth
      susp = runSearchSusp conf
      tmSet = Set.fromList tms
  findAll tmSet susp

basicDeclSrc :: DeclSrc
basicDeclSrc = DeclSrcList
  [ "zero :: Int"
  , "one :: Int"
  , "plus :: Int -> Int -> Int"
  ]

testSearch :: TestTree
testSearch = testGroup "Search"
  [ testFinds "basic" basicDeclSrc "Int" ["zero", "one", "plus zero one", "plus (plus one zero) zero"]
  ]
