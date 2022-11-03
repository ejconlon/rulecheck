module Rulecheck.Interface.Printer where
  -- ( printLines
  -- ) where

import Data.Foldable (toList)
import Data.Sequence (Seq)
import Data.Text (Text)
import Prettyprinter (Doc, pretty)
import qualified Prettyprinter as P
import Rulecheck.Interface.Core (Tm, TmVar, TyScheme, TyVar)
import Rulecheck.Interface.ParenPretty (docToText)
import Rulecheck.Interface.Types (Line)

linesP :: Seq Line -> Doc ()
linesP = P.vcat . fmap pretty . toList

printLines :: Seq Line -> Text
printLines = docToText . linesP

printType :: TyScheme TyVar -> Text
printType = docToText . pretty

printTerm :: Tm TmVar TmVar -> Text
printTerm = docToText . pretty
