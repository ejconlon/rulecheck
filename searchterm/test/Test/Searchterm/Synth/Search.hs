{-# LANGUAGE OverloadedStrings #-}

module Test.Searchterm.Synth.Search where -- (testSearch) where

import Control.Exception (Exception, throwIO)
import Control.Monad (unless)
import Data.Foldable (for_, toList)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Searchterm.Interface.Core (Index (..), TmName (..), TmVar (..), TmF (..), Tm (..))
import Searchterm.Interface.Decl (DeclSet (..), mkLineDecls)
import Searchterm.Interface.Names (AlphaTm (..), closeAlphaTm, mapAlphaTm, namelessType, unsafeLookupSeq)
import Searchterm.Interface.Parser (parseLines, parseLinesIO, parseTerm, parseType)
import Searchterm.Interface.Printer (printTerm)
import Searchterm.Synth.Search (SearchConfig (..), SearchSusp, TmFound, nextSearchResult, runSearchSusp, TmUniq)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)
import Test.Tasty.Providers (TestName)
import Control.Monad.Reader (runReader, asks, MonadReader (..), Reader)
import Data.Sequence (Seq(..))
import Data.Maybe (fromMaybe)
import Data.Functor.Foldable (cata)

rethrow :: Exception e => Either e a -> IO a
rethrow = either throwIO pure

data DeclSrc =
    DeclSrcFile !FilePath
  | DeclSrcList ![Text]
  deriving stock (Eq, Show)

loadDecls :: DeclSrc -> IO DeclSet
loadDecls src = do
  ls <- case src of
    DeclSrcFile fp -> parseLinesIO fp
    DeclSrcList ts -> rethrow (parseLines "<load>" (T.unlines ts))
  rethrow (mkLineDecls (toList ls))

maxSearchDepth :: Int
maxSearchDepth = 5

maxSearchResults :: Int
maxSearchResults = 1000

printAlphaTm :: AlphaTm -> Text
printAlphaTm = printTerm . fmap (TmVar . T.pack . ("?" ++) . show . unIndex) . unAlphaTm

reportMissing :: Set AlphaTm -> IO ()
reportMissing tms =
  unless (null tms) $ do
    putStrLn "Did not find terms:"
    for_ (toList tms) (TIO.putStrLn . printAlphaTm)
    fail ("Missing " ++ show (Set.size tms) ++ " terms")

reportIllegal :: AlphaTm -> IO ()
reportIllegal tm = fail ("Found illegal term: " ++ T.unpack (printAlphaTm tm))

inlineLets :: TmFound -> TmFound
inlineLets = flip runReader Empty . cata goTm where
  goTm :: TmF TmUniq Index (Reader (Seq (Maybe TmFound)) TmFound) -> Reader (Seq (Maybe TmFound)) TmFound
  goTm = \case
    TmFreeF a -> do
      mx <- asks (`unsafeLookupSeq` a)
      pure (fromMaybe (TmFree a) mx)
    TmLitF l -> pure (TmLit l)
    TmKnownF n -> pure (TmKnown n)
    TmAppF wl wr -> TmApp <$> wl <*> wr
    TmLamF b w -> TmLam b <$> local (:|> Nothing) w
    TmLetF _ arg body -> arg >>= \a -> local (:|> Just a) body
    TmCaseF scrut pairs -> TmCase <$> scrut <*> traverse sequence pairs

findAll :: Int -> Set AlphaTm -> Set AlphaTm -> SearchSusp TmFound -> IO ()
findAll !lim !yesTms !noTms !susp =
  if lim <= 0 || Set.null yesTms
    then reportMissing yesTms
    else do
      mx <- rethrow (nextSearchResult susp)
      case mx of
        Nothing -> reportMissing yesTms
        Just (tm, susp') -> do
          -- TIO.putStrLn (docToText (pretty tm))
          let tmNoLet = inlineLets tm
          let tm' = mapAlphaTm tmNoLet
          -- TIO.putStrLn (printAlphaTm tm')
          if Set.member tm' noTms
            then reportIllegal tm'
            else do
              let tms' = Set.delete tm' yesTms
              findAll (lim - 1) tms' noTms susp'

testFinds :: TestName -> DeclSrc -> Text -> [Text] -> [Text] -> TestTree
testFinds n src tyStr yesTmStrs noTmStrs = testCase n $ do
  ds <- loadDecls src
  tsNamed <- rethrow (parseType tyStr)
  ts <- rethrow (namelessType tsNamed)
  yesTms <- traverse (rethrow . parseTerm) yesTmStrs
  noTms <- traverse (rethrow . parseTerm) noTmStrs
  let isKnown (TmVar v) = let k = TmName v in if Map.member k (dsMap ds) then Just k else Nothing
  yesCtms <- traverse (rethrow . closeAlphaTm isKnown) yesTms
  noCtms <- traverse (rethrow . closeAlphaTm isKnown) noTms
  let yesTmSet = Set.fromList yesCtms
      noTmSet = Set.fromList noCtms
      conf = SearchConfig ds ts maxSearchDepth
      susp = runSearchSusp conf
  findAll maxSearchResults yesTmSet noTmSet susp

basicDeclSrc :: DeclSrc
basicDeclSrc = DeclSrcList
  [ "zero :: Int"
  , "one :: Int"
  , "plus :: Int -> Int -> Int"
  ]

strainSimpleDeclSrc :: DeclSrc
strainSimpleDeclSrc = DeclSrcList
  [ "instance Foo FooThing"
  , "instance Bar BarThing"
  , "foo :: FooThing"
  , "bar :: BarThing"
  , "quux :: (Foo c, Bar b) => c -> b -> Int"
  ]

strainRecDeclSrc :: DeclSrc
strainRecDeclSrc = DeclSrcList
  [ "instance Wonky a => Foo (FooThing a)"
  , "instance Wonky Char"
  , "foo :: FooThing Char"
  , "bar :: FooThing String"
  , "quux :: Foo c => c -> Int"
  ]

destructDeclSrc :: DeclSrc
destructDeclSrc = DeclSrcList
  [ "data Either a b"
  , "Left :: a -> Either a b"
  , "Right :: b -> Either a b"
  , "constructors Either Left Right"
  , "showChar :: Char -> String"
  , "showInt :: Int -> String"
  ]

litsDeclSrc :: DeclSrc
litsDeclSrc = DeclSrcList
  [ "literals Int 0 -1 2"
  , "literals Int 3"
  ]

testSearch :: TestTree
testSearch = testGroup "Search"
  [ testFinds "ctx" (DeclSrcList []) "Int -> Int"
    ["(\\x -> x)"]
    []
  , testFinds "basic" basicDeclSrc "Int"
    ["zero", "one", "(plus zero one)", "((plus one) ((plus one) zero))"]
    ["(plus zero)", "plus", "(zero plus)"]
  , testFinds "strain simple" strainSimpleDeclSrc "Int"
    ["(quux foo bar)"]
    ["foo"]
  , testFinds "strain rec" strainRecDeclSrc "Int"
    ["(quux foo)"]
    ["(quux bar)"]
  , testFinds "destruct" destructDeclSrc "Either Char Int -> String"
    ["(\\x -> (case x of { Left y -> (showChar y) ; Right z -> (showInt z) }))"]
    ["showChar"]
  , testFinds "literals" litsDeclSrc "Int"
    ["0", "-1", "2", "3"]
    ["4"]
  -- , testFinds "GenString"
  --     (DeclSrcList
  --      [ "instance IsString String"
  --      , "fromString :: IsString c => String0 -> c"
  --      , "primString :: String0"
  --      ]
  --     ) "String" ["(fromString primString)"] []
  -- TODO more tests!!! But the pattern is clear...
  ]
