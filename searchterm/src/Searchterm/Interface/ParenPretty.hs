-- | This exists so we can detect when parentheses are and aren't necessary
-- when pretty-printing! Sad, right?
module Searchterm.Interface.ParenPretty
  ( docToText
  , parenToDoc
  , Paren
  , ParenPretty (..)
  , parenAtom
  , parenDoc
  , parenList
  , parenPrettyToDoc
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

parenToDoc :: Paren ann -> Doc ann
parenToDoc = \case
  ParenAtom doc -> doc
  ParenList selfParen children ->
    let doc = P.hsep (fmap parenToDoc (toList children))
    in if selfParen && Seq.length children > 1 then P.parens doc else doc

class ParenPretty a where
  parenPretty :: [Maybe String] -> a -> Paren ann

instance ParenPretty Void where
  parenPretty _ = absurd

instance ParenPretty () where
  parenPretty _ _ = ParenList False Empty

instance (ParenPretty a, ParenPretty b) => ParenPretty (a, b) where
  parenPretty s (a, b) = ParenList True (parenPretty (Nothing:s) a :<| parenPretty (Nothing:s) b :<| Empty)

instance ParenPretty a => ParenPretty [a] where
  parenPretty s = ParenList True . Seq.fromList . fmap (parenPretty (Nothing:s))

instance ParenPretty a => ParenPretty (Seq a) where
  parenPretty s = ParenList True . fmap (parenPretty (Nothing:s))

parenAtom :: Pretty a => a -> Paren ann
parenAtom = ParenAtom . pretty

parenDoc :: Doc ann -> Paren ann
parenDoc = ParenAtom

parenList :: Foldable f => Bool -> f (Paren ann) -> Paren ann
parenList b = ParenList b . Seq.fromList . toList

parenPrettyToDoc :: ParenPretty a => a -> Doc ann
parenPrettyToDoc = parenToDoc . parenPretty []

newtype ViaPrettyAtom a = ViaPrettyAtom { unViaPrettyAtom :: a }

instance Pretty a => ParenPretty (ViaPrettyAtom a) where
  parenPretty _ = ParenAtom . pretty . unViaPrettyAtom

deriving via (ViaPrettyAtom Int) instance ParenPretty Int
deriving via (ViaPrettyAtom Integer) instance ParenPretty Integer
deriving via (ViaPrettyAtom Text) instance ParenPretty Text
deriving via (ViaPrettyAtom Char) instance ParenPretty Char
