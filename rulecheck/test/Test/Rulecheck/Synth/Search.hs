{-# LANGUAGE OverloadedStrings #-}

module Test.Rulecheck.Synth.Search (testSearch) where

import Control.Exception (Exception, throwIO)
import Control.Monad (unless)
import Data.Foldable (for_, toList)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Rulecheck.Interface.Core (Index (..), TmName (..), TmVar (..))
import Rulecheck.Interface.Decl (DeclSet (..), mkLineDecls)
import Rulecheck.Interface.Names (AlphaTm (..), closeAlphaTm, mapAlphaTm, namelessType)
import Rulecheck.Interface.Parser (parseLines, parseLinesIO, parseTerm, parseType)
import Rulecheck.Interface.Printer (printTerm)
import Rulecheck.Synth.Search (SearchConfig (..), TmFound, runSearchN)
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
maxSearchResults = 100

printAlphaTm :: AlphaTm -> Text
printAlphaTm = printTerm . fmap (TmVar . T.pack . ("?" ++) . show . unIndex) . unAlphaTm

reportMissing :: Set AlphaTm -> IO ()
reportMissing tms =
  unless (null tms) $ do
    putStrLn "Did not find terms:"
    for_ (toList tms) (TIO.putStrLn . printAlphaTm)
    fail ("Missing " ++ show (Set.size tms) ++ " terms")

findAll :: Int -> Set AlphaTm -> [TmFound] -> IO ()
findAll !lim !tms !res =
  if lim <= 0 || Set.null tms
    then reportMissing tms
    else do
      case res of
        [] -> reportMissing tms
        tm:res' -> do
          let tm' = mapAlphaTm tm
          -- TIO.putStrLn (printAlphaTm tm')
          let tms' = Set.delete tm' tms
          findAll (lim - 1) tms' res'
      -- TODO use incremental search when it works
      -- mx <- rethrow (nextSearchResult susp)
      -- case mx of
      --   Nothing -> reportMissing tms
      --   Just (tm, susp') -> do
      --     let tm' = mapAlphaTm tm
      --     let tms' = Set.delete tm' tms
      --     findAll (lim - 1) tms' susp'

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
  res <- rethrow (runSearchN conf maxSearchResults)
  findAll maxSearchResults tmSet res

basicDeclSrc :: DeclSrc
basicDeclSrc = DeclSrcList
  [ "zero :: Int"
  , "one :: Int"
  , "plus :: Int -> Int -> Int"
  ]

testSearch :: TestTree
testSearch = testGroup "Search"
  [ testFinds "basic" basicDeclSrc "Int" ["zero", "one", "((plus zero) one)"] -- "((plus ((plus one) zero)) zero)"]
  ]
