{-# LANGUAGE OverloadedStrings #-}

module Test.Rulecheck.Synth.Search (testSearch) where

import Data.Text (Text)
import Rulecheck.Interface.Decl (DeclSet (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)
import Test.Tasty.Providers (TestName)

data DeclSrc =
    DeclSrcFile !FilePath
  | DeclSrcList ![Text]
  deriving stock (Eq, Show)

loadDecls :: DeclSrc -> IO DeclSet
loadDecls = \case
  DeclSrcFile _fp -> error "TODO"
  DeclSrcList _ -> error "TODO"

maxSearchDepth :: Int
maxSearchDepth = 5

testFinds :: TestName -> DeclSrc -> Text -> [Text] -> TestTree
testFinds n _src _sc _tms = testCase n $ do
  -- TODO fill in
  pure ()

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
