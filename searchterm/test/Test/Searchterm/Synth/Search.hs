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
import Searchterm.Interface.Core (Index (..), TmName (..), TmVar (..))
import Searchterm.Interface.Decl (DeclSet (..), mkLineDecls)
import Searchterm.Interface.Names (AlphaTm (..), closeAlphaTm, mapAlphaTm, namelessType)
import Searchterm.Interface.Parser (parseLines, parseLinesIO, parseTerm, parseType)
import Searchterm.Interface.Printer (printTerm)
import Searchterm.Synth.Search (SearchConfig (..), SearchSusp, TmFound, nextSearchResult, runSearchSusp)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)
import Test.Tasty.Providers (TestName)

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

findAll :: Int -> Set AlphaTm -> SearchSusp TmFound -> IO ()
findAll !lim !tms !susp =
  if lim <= 0 || Set.null tms
    then reportMissing tms
    else do
      mx <- rethrow (nextSearchResult susp)
      case mx of
        Nothing -> reportMissing tms
        Just (tm, susp') -> do
          let tm' = mapAlphaTm tm
          -- TIO.putStrLn (printAlphaTm tm')
          let tms' = Set.delete tm' tms
          findAll (lim - 1) tms' susp'

testFinds :: TestName -> DeclSrc -> Text -> [Text] -> TestTree
testFinds n src tyStr tmStrs = testCase n $ do
  ds <- loadDecls src
  tsNamed <- rethrow (parseType tyStr)
  ts <- rethrow (namelessType tsNamed)
  tms <- traverse (rethrow . parseTerm) tmStrs
  let isKnown (TmVar v) = let k = TmName v in if Map.member k (dsMap ds) then Just k else Nothing
  ctms <- traverse (rethrow . closeAlphaTm isKnown) tms
  let tmSet = Set.fromList ctms
  let conf = SearchConfig ds ts maxSearchDepth
      susp = runSearchSusp conf
  findAll maxSearchResults tmSet susp

basicDeclSrc :: DeclSrc
basicDeclSrc = DeclSrcList
  [ "zero :: Int"
  , "one :: Int"
  , "plus :: Int -> Int -> Int"
  ]

strainDeclSrc :: DeclSrc
strainDeclSrc = DeclSrcList
  [ "class Foo"
  , "class Bar"
  , "data FooThing"
  , "instance Foo FooThing"
  , "data BarThing"
  , "instance Bar BarThing"
  , "foo:: FooThing"
  , "bar :: BarThing"
  , "quux :: (Foo c, Bar b) => c -> b -> Int"
  ]

testSearch :: TestTree
testSearch = testGroup "Search"
  [ testFinds "basic" basicDeclSrc "Int"
    ["zero", "one", "(plus zero one)", "(plus (plus one zero) zero)"]
  -- , testFinds "strain" strainDeclSrc "Int"
  --   ["(quux foo bar)"]
  ]
