module Rulecheck.Rendering
  ( ConvertErr (..)
  , convertAndRender
  , outputString
  ) where

import GHC.ThToHs (convertToHsDecls)
import GHC.Utils.Error (MsgDoc)
import GHC.Hs.Decls (LHsDecl)
import GHC.Hs.Extension (GhcPs)
import Language.Haskell.TH.Syntax (Dec)
import GHC.Plugins (UnhelpfulSpanReason (..), Origin (..), SrcSpan (..), Outputable (..), HasDynFlags (..), PprStyle (..), reallyAlwaysQualify)
import GHC.Utils.Outputable (SDoc, vcat, renderWithStyle, initSDocContext)
import Control.Exception (Exception)
import Control.Monad.Catch (MonadThrow, throwM)

-- Convert a sequence of TH declarations (abstract syntax) to HS declarations (concrete syntax)
convertThDecls :: [Dec] -> Either MsgDoc [LHsDecl GhcPs]
convertThDecls = convertToHsDecls Generated (UnhelpfulSpan UnhelpfulNoLocationInfo)

-- Render a sequence of HS declarations to a printable document
renderHsDecls :: [LHsDecl GhcPs] -> SDoc
renderHsDecls = vcat . fmap ppr

-- Don't ask me why GHC puts \NUL in var names
stripNulls :: String -> String
stripNulls = filter (/= '\NUL')

-- Render that a printable document to string
renderSDoc :: (Functor m, HasDynFlags m) => SDoc -> m String
renderSDoc doc = flip fmap getDynFlags $ \dynFlags ->
  -- Uh this style seems to work...
  let sty = PprDump reallyAlwaysQualify
      ctx = initSDocContext dynFlags sty
  in stripNulls (renderWithStyle ctx doc)

-- | Error encountered in conversion
newtype ConvertErr =
    ConvertErr String
  deriving (Eq, Show)

instance Exception ConvertErr

-- | Converts a sequences of TH declarations to program string
convertAndRender :: (MonadThrow m, HasDynFlags m) => [Dec] -> m String
convertAndRender thDecls = case convertThDecls thDecls of
  Left errDoc -> do
    errStr <- renderSDoc errDoc
    throwM (ConvertErr errStr)
  Right hsDecls -> renderSDoc (renderHsDecls hsDecls)

outputString :: (Functor m, HasDynFlags m, Outputable a) => a -> m String
outputString = renderSDoc . ppr
