module Searchterm.Grammar.Manual.NewInterface where

import Data.Text (Text)
import Searchterm.Grammar.Gen.Abs (Lines, Line)
import Searchterm.Grammar.Gen.Par (myLexer, pLines, pLine)
import qualified Data.Text as T
import Searchterm.Grammar.Gen.Print (printTree)

class Grammatical a where
  parseGram :: Text -> Either String a
  printGram :: a -> Text

instance Grammatical Lines where
  parseGram = pLines . myLexer
  printGram = T.pack . printTree

instance Grammatical Line where
  parseGram = pLine . myLexer
  printGram = T.pack . printTree
