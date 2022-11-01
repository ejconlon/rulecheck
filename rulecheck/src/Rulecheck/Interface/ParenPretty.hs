-- | This exists so we can detect when parentheses are and aren't necessary
-- when pretty-printing! Sad, right?
module Rulecheck.Interface.ParenPretty
  ( docToText
  , Paren
  , ParenPretty (..)
  , parenAtom
  , parenDoc
  , parenList
  , parenToDoc
  , parenToText
  , ViaPrettyAtom (..)
  ) where

import Data.Foldable (toList)
import Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq
import Data.String (IsString (..))
import Data.Text (Text)
import Data.Void (Void, absurd)
import Prettyprinter (Doc, Pretty (..), defaultLayoutOptions, layoutSmart)
import qualified Prettyprinter as P
import Prettyprinter.Render.Text (renderStrict)

docToText :: Doc ann -> Text
docToText = renderStrict . layoutSmart defaultLayoutOptions

data Paren ann =
    ParenAtom !(Doc ann)
  | ParenList !Bool !(Seq (Paren ann))
  deriving stock (Show)

instance IsString (Paren ann) where
  fromString = ParenAtom . pretty

renderParen :: Paren ann -> Doc ann
renderParen = \case
  ParenAtom doc -> doc
  ParenList selfParen children ->
    let doc = P.hsep (fmap renderParen (toList children))
    in if selfParen && Seq.length children > 1 then P.parens doc else doc

class ParenPretty a where
  parenPretty :: [(String, Int)] -> a -> Paren ann

instance ParenPretty Void where
  parenPretty _ = absurd

instance ParenPretty () where
  parenPretty _ _ = ParenList False Empty

instance (ParenPretty a, ParenPretty b) => ParenPretty (a, b) where
  parenPretty s (a, b) = ParenList True (parenPretty (("tup", 0):s) a :<| parenPretty (("tup", 1):s) b :<| Empty)

instance ParenPretty a => ParenPretty [a] where
  parenPretty s = ParenList True . Seq.fromList . fmap (\(i, a) -> parenPretty (("list", i):s) a) . zip [0..]

instance ParenPretty a => ParenPretty (Seq a) where
  parenPretty s = ParenList True . Seq.mapWithIndex (\i a -> parenPretty (("seq", i):s) a)

parenAtom :: Pretty a => a -> Paren ann
parenAtom = ParenAtom . pretty

parenDoc :: Doc ann -> Paren ann
parenDoc = ParenAtom

parenList :: Foldable f => Bool -> f (Paren ann) -> Paren ann
parenList b = ParenList b . Seq.fromList . toList

parenToDoc :: ParenPretty a => a -> Doc ann
parenToDoc = renderParen . parenPretty []

parenToText :: ParenPretty a => a -> Text
parenToText = docToText . parenToDoc

newtype ViaPrettyAtom a = ViaPrettyAtom { unViaPrettyAtom :: a }

instance Pretty a => ParenPretty (ViaPrettyAtom a) where
  parenPretty _ = ParenAtom . pretty . unViaPrettyAtom

deriving via (ViaPrettyAtom Int) instance ParenPretty Int
deriving via (ViaPrettyAtom Integer) instance ParenPretty Integer
deriving via (ViaPrettyAtom Text) instance ParenPretty Text
deriving via (ViaPrettyAtom Char) instance ParenPretty Char
