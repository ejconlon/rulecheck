{-# LANGUAGE OverloadedStrings #-}

module Test.Rulecheck.Synth.Search (testSearch) where

import Control.Exception (Exception, throwIO)
import Data.Foldable (for_, toList)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Rulecheck.Interface.Core (Tm, TmVar)
import Rulecheck.Interface.Decl (DeclSet (..), mkLineDecls, namelessScheme)
import Rulecheck.Interface.Parser (parseLines, parseLinesIO, parseScheme, parseTerm)
import Rulecheck.Interface.Printer (printTerm)
import Rulecheck.Synth.Search (SearchConfig (..), SearchSusp, TmFound, nextSearchResult, runSearchSusp)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)
import Test.Tasty.Providers (TestName)

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

findAll :: Set (Tm TmVar TmVar) -> SearchSusp TmFound -> IO ()
findAll !tms !susp =
  if Set.null tms
    then pure ()
    else do
      mx <- rethrow (nextSearchResult susp)
      case mx of
        Nothing -> do
          putStrLn "Did not find terms:"
          for_ (toList tms) (TIO.putStrLn . printTerm)
          fail ("Missing " ++ show (Set.size tms) ++ " terms")
        Just (_tm, _susp') -> error "TODO"
          -- let tms' = Set.delete tm tms
          -- in findAll tms' susp'

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
  -- TODO implement more so this passes
  -- [ testFinds "basic" basicDeclSrc "Int" ["zero", "one", "plus zero one", "plus (plus one zero) zero"]
  [
  ]
