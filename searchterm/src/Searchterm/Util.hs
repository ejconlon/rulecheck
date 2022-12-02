module Searchterm.Util (DeclSrc(..), loadDecls, rethrow) where

import Control.Exception (Exception, throwIO)
import Data.Foldable (toList)
import Searchterm.Interface.Parser (parseLines, parseLinesIO)
import Searchterm.Interface.Decl (DeclSet, mkLineDecls)
import Data.Text (Text)
import qualified Data.Text as T

data DeclSrc =
    DeclSrcFile !FilePath
  | DeclSrcList ![Text]
  deriving stock (Eq, Show)

rethrow :: Exception e => Either e a -> IO a
rethrow = either throwIO pure

loadDecls :: DeclSrc -> IO DeclSet
loadDecls src = do
  ls <- case src of
    DeclSrcFile fp -> parseLinesIO fp
    DeclSrcList ts -> rethrow (parseLines "<load>" (T.unlines ts))
  rethrow (mkLineDecls (toList ls))
