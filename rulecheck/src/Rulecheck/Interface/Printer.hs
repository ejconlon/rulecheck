module Rulecheck.Interface.Printer where
  -- ( printLines
  -- ) where

import Data.Foldable (toList)
import Data.Sequence (Seq)
import Data.Text (Text)
import Prettyprinter (Doc, pretty)
import qualified Prettyprinter as P
import Rulecheck.Interface.Core (Scheme, Tm, TmVar, TyVar)
import Rulecheck.Interface.ParenPretty (docToText, parenToDoc)
import Rulecheck.Interface.Types (Line)

linesP :: Seq Line -> Doc ()
linesP = P.vcat . fmap pretty . toList

printLines :: Seq Line -> Text
printLines = docToText . linesP

printScheme :: Scheme TyVar -> Text
printScheme = docToText . pretty

printTerm :: Tm TmVar TmVar -> Text
printTerm = docToText . parenToDoc
