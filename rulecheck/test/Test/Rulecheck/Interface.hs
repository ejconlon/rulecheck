module Test.Rulecheck.Interface (testInterface) where

import Control.Exception (throwIO)
import Data.Foldable (for_)
import qualified Data.Sequence as Seq
import Rulecheck.Interface.Parser (parseLines, parseLinesIO)
import Rulecheck.Interface.Printer (printLines)
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (testCase, (@?=))

testInterface :: TestTree
testInterface = testCase "interface" $ do
  -- assert that we can parse, render, and parse again to get the same thing
  x <- parseLinesIO "../testdata/base.txt"
  let y = printLines x
  z <- either throwIO pure (parseLines "<test>" y)
  if Seq.length z == Seq.length x
    then for_ (Seq.zip z x) (uncurry (@?=))
    else fail "mismatch lengths"
