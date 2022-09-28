module Rulecheck.Rendering where

import GHC.ThToHs (convertToHsDecls)
import GHC.Utils.Error (MsgDoc)
import GHC.Hs.Decls (LHsDecl)
import GHC.Hs.Extension (GhcPs)
import Language.Haskell.TH.Syntax (Dec)
import GHC.Plugins (UnhelpfulSpanReason (..), Origin (..), SrcSpan (..), Outputable (..))
import GHC.Utils.Outputable (SDoc, vcat)
import Data.Text (Text)

-- | Convert a sequence of TH declarations (abstract syntax) to HS declarations (concrete syntax)
convertThDecls :: [Dec] -> Either MsgDoc [LHsDecl GhcPs]
convertThDecls = convertToHsDecls Generated (UnhelpfulSpan UnhelpfulNoLocationInfo)

-- | Render a sequence of HS declarations to a printable document
renderHsDecls :: [LHsDecl GhcPs] -> SDoc
renderHsDecls = vcat . fmap ppr

-- | Render that a printable document to text
sdocToText :: SDoc -> Text
sdocToText = error "TODO"
