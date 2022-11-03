module Rulecheck.Interface.Printer where
  -- ( printLines
  -- ) where

import Data.Foldable (toList)
import Data.Sequence (Seq)
import Data.Text (Text)
import Prettyprinter (Doc, Pretty, pretty)
import qualified Prettyprinter as P
import Rulecheck.Interface.Core (Tm, TyScheme)
import Rulecheck.Interface.ParenPretty (docToText)
import Rulecheck.Interface.Types (Line)

linesP :: Seq Line -> Doc ()
linesP = P.vcat . fmap pretty . toList

printLines :: Seq Line -> Text
printLines = docToText . linesP

printType :: Pretty a => TyScheme a -> Text
printType = docToText . pretty

printTerm :: (Pretty b, Pretty a) => Tm b a -> Text
printTerm = docToText . pretty
