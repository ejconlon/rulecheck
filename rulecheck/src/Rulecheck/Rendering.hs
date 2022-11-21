module Rulecheck.Rendering
  ( ConvertErr (..)
  , renderSDoc
  , convertAndRender
  , identifyRequiredImports
  , outputString
  ) where

import Control.DeepSeq
import Control.Exception (Exception)
import Control.Monad.Catch (MonadThrow, throwM)
import Control.Monad.IO.Class
import Data.Set as Set (Set, empty, insert)
import Data.IORef
import GHC.Hs.Decls (LHsDecl)
import GHC.Hs.Extension (GhcPs)
import GHC.Plugins
import GHC.ThToHs (convertToHsDecls)
import GHC.Utils.Error (MsgDoc)
import Language.Haskell.TH.Syntax (Dec)
import System.IO.Unsafe

-- Convert a sequence of TH declarations (abstract syntax) to HS declarations (concrete syntax)
convertThDecls :: [Dec] -> Either MsgDoc [LHsDecl GhcPs]
convertThDecls = convertToHsDecls Generated (UnhelpfulSpan UnhelpfulNoLocationInfo)

-- Render a sequence of HS declarations to a printable document
renderHsDecls :: [LHsDecl GhcPs] -> SDoc
renderHsDecls = vcat . fmap ppr

-- Don't ask me why GHC puts \NUL in var names
stripNulls :: String -> String
stripNulls = filter (/= '\NUL')

-- | `identifyRequiredImports doc` determines what modules should be imported
--   to resolve all of the `Name`s in `doc`.
--
--   The way of doing this is a bit counter-intuitive: it attempts to generate
--   an output string for `doc`, and attaches a handler for checking whether
--   to qualify a given name. The handler records each name it receives into a set.
--   The rendered document is discarded and the set is returned. Because the handler
--   interface is pure, unsafePerformIO is required here.
identifyRequiredImports :: (MonadIO m, HasDynFlags m) => SDoc -> m (Set String)
identifyRequiredImports doc =
  do
    ref      <- liftIO $ newIORef Set.empty
    dynFlags <- getDynFlags
    let sty = mkUserStyle (QueryQualify
          (checkName ref)
          (queryQualifyModule neverQualify)
          (queryQualifyPackage neverQualify)) AllTheWay
    let ctx = initSDocContext dynFlags sty
    deepseq (renderWithStyle ctx doc) (liftIO $ readIORef ref)
  where
    checkName ref m occ =
      let
        modName = moduleNameString (moduleName m)
        result  = queryQualifyName neverQualify m occ
      in
        unsafePerformIO (modifyIORef' ref (Set.insert modName) >> return result)

-- Render that a printable document to string
renderSDoc :: (Functor m, HasDynFlags m) => SDoc -> m String
renderSDoc doc = flip fmap getDynFlags $ \dynFlags ->
  -- Uh this style seems to work...
  let sty = mkUserStyle neverQualify AllTheWay
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
