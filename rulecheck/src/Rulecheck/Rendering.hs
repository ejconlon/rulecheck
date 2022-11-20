module Rulecheck.Rendering
  ( ConvertErr (..)
  , renderSDoc
  , convertAndRender
  , outputString
  ) where

import Control.Exception (Exception)
import Control.Monad.Catch (MonadThrow, throwM)
import qualified Data.Set as S
import Debug.Trace (trace)
import GHC.Hs.Decls (LHsDecl)
import GHC.Hs.Extension (GhcPs)
import GHC.Plugins (HasDynFlags (..), Origin (..), Outputable (..), PprStyle (..), SrcSpan (..),
                    UnhelpfulSpanReason (..), alwaysQualify, neverQualify, occNameString, moduleName)
import GHC.ThToHs (convertToHsDecls)
import GHC.Utils.Error (MsgDoc)
import GHC.Utils.Outputable (PrintUnqualified(..), SDoc, initSDocContext, renderWithStyle, vcat)
import GHC.Unit.Module.Name
import Language.Haskell.TH.Syntax (Dec)

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
renderSDoc :: (Functor m, HasDynFlags m) => S.Set String -> SDoc -> m String
renderSDoc importedModuleNames doc = flip fmap getDynFlags $ \dynFlags ->
  -- Uh this style seems to work...
  let sty = PprDump (
        QueryQualify
          checkName
          (queryQualifyModule neverQualify)
          (queryQualifyPackage neverQualify)
        )
      ctx = initSDocContext dynFlags sty
  in stripNulls (renderWithStyle ctx doc)
  where
    checkName m occName | S.member (moduleNameString (moduleName m)) importedModuleNames
                        = queryQualifyName neverQualify m occName
    checkName m occName | otherwise =
      trace ("Qualifying " ++ moduleNameString (moduleName m) ++ "." ++ (occNameString occName)) $ queryQualifyName alwaysQualify m occName

-- | Error encountered in conversion
newtype ConvertErr =
    ConvertErr String
  deriving (Eq, Show)

instance Exception ConvertErr

-- | Converts a sequences of TH declarations to program string
convertAndRender :: (MonadThrow m, HasDynFlags m) => S.Set String -> [Dec] -> m String
convertAndRender importedModuleNames thDecls = case convertThDecls thDecls of
  Left errDoc -> do
    errStr <- renderSDoc importedModuleNames errDoc
    throwM (ConvertErr errStr)
  Right hsDecls -> renderSDoc importedModuleNames (renderHsDecls hsDecls)

outputString :: (Functor m, HasDynFlags m, Outputable a) => S.Set String -> a -> m String
outputString importedModuleNames = renderSDoc importedModuleNames . ppr
