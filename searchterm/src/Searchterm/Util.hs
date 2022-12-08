module Searchterm.Util where

import Control.Exception (Exception, catch, throwIO)
import Control.Monad.Reader (MonadReader (..), Reader, runReader)
import Data.Foldable (toList)
import Data.Functor.Foldable (cata)
import Data.Sequence (Seq(..))
import qualified Data.Sequence as Seq
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Stack (HasCallStack, callStack, prettyCallStack)
import Searchterm.Interface.Names (unsafeLookupSeq)
import Searchterm.Interface.Core
import Searchterm.Interface.Decl (DeclSet, mkLineDecls)
import Searchterm.Interface.Parser (ParseErr, parseLines, parseLinesIO)
import Searchterm.Interface.Types (Line)
import Searchterm.Synth.Search
import Text.Megaparsec (errorBundlePretty)

type TmInline = Tm TmUniq TmUniq

data BoundVal =
    BoundValNonLet !TmUniq
  | BoundValLet !TmInline

type InlineSt = Seq BoundVal

inlineLets :: TmFound -> TmInline
inlineLets = flip runReader Empty . go where
  go :: TmFound -> Reader InlineSt TmInline
  go = cata goTm
  localVar :: BoundVal -> Reader InlineSt TmInline -> Reader InlineSt TmInline
  localVar = localVars . Seq.singleton
  localVars :: Seq BoundVal -> Reader InlineSt TmInline -> Reader InlineSt TmInline
  localVars vs = local (<> vs)
  lookupVar :: Index -> Reader InlineSt TmInline
  lookupVar a = do
    zs <- ask
    pure $ case unsafeLookupSeq zs a of
      BoundValNonLet u -> TmFree u
      BoundValLet t -> t
  goTm :: TmF TmUniq Index (Reader InlineSt TmInline) -> Reader InlineSt TmInline
  goTm = \case
    TmFreeF a -> lookupVar a
    TmLitF l -> pure (TmLit l)
    TmKnownF n -> pure (TmKnown n)
    TmAppF wl wr -> TmApp <$> wl <*> wr
    TmLamF b w -> TmLam b <$> localVar (BoundValNonLet b) w
    TmLetF _ arg body -> arg >>= \a -> localVar (BoundValLet a) body
    TmCaseF scrut pairs -> TmCase <$> scrut <*> traverse goPair pairs
  goPair :: PatPair TmUniq (Reader InlineSt TmInline) -> Reader InlineSt (PatPair TmUniq TmInline)
  goPair (PatPair pat w) = fmap (PatPair pat) (localVars (Seq.fromList (fmap BoundValNonLet (toList pat))) w)

rethrow :: (HasCallStack, Exception e) => Either e a -> IO a
rethrow = either throwErr pure where
  throwErr err = do
    putStrLn $ "Exception at " ++ prettyCallStack callStack
    throwIO err

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
