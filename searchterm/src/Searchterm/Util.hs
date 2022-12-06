module Searchterm.Util where

import Control.Exception
import Data.Foldable (toList)
import Searchterm.Interface.Parser
import Searchterm.Interface.Decl (DeclSet, mkLineDecls)
import Data.Text (Text)
import qualified Data.Text as T
import Text.Megaparsec

rethrow :: Exception e => Either e a -> IO a
rethrow = either throwIO pure

data DeclSrc =
    DeclSrcFile !FilePath
  | DeclSrcList ![Text]
  | DeclSrcPlus !DeclSrc !DeclSrc
  deriving stock (Eq, Show)

loadDeclLines :: DeclSrc -> IO (Seq Line)
loadDeclLines = \case
  DeclSrcFile fp -> parseLinesIO fp
  DeclSrcList ts -> rethrow (parseLines "<load>" (T.unlines ts))
  DeclSrcPlus a b -> (<>) <$> loadDeclLines a <*> loadDeclLines b

loadDecls :: DeclSrc -> IO DeclSet
loadDecls src = do
  ls <- loadDeclLines src
  rethrow (mkLineDecls (toList ls))
withDieOnParseErr :: IO a -> IO a
withDieOnParseErr act = catch act dieOnParseErr

dieOnParseErr :: ParseErr -> a
dieOnParseErr err = error $ "Err was " ++ errorBundlePretty err

loadDecls :: DeclSrc -> IO DeclSet
loadDecls src = do
  ls <- case src of
    DeclSrcFile fp -> parseLinesIO fp
    DeclSrcList ts -> rethrow (parseLines "<load>" (T.unlines ts))
  rethrow (mkLineDecls (toList ls))
