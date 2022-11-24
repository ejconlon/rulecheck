{-# LANGUAGE OverloadedStrings #-}

module Test.Searchterm.Synth.Search (testSearch) where

import Control.Exception (Exception, throwIO)
import Control.Monad ((<=<), unless, void)
import Data.Foldable (for_, toList)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Searchterm.Interface.Core (Index (..), TmName (..), TmVar (..), TmF (..), Tm (..), Forall (..), TyScheme (..), TyVar (..))
import Searchterm.Interface.Decl (DeclSet (..), mkLineDecls)
import Searchterm.Interface.Names (AlphaTm (..), closeAlphaTm, mapAlphaTm, namelessType, unsafeLookupSeq, closeAlphaTyScheme, AlphaTyScheme (..), toListWithIndex)
import Searchterm.Interface.Parser (parseLines, parseLinesIO, parseTerm, parseType)
import Searchterm.Interface.Printer (printTerm, printType)
import Searchterm.Synth.Search (SearchConfig (..), SearchSusp, Found (..), TmFound, nextSearchResult, runSearchSusp, TmUniq, UseSkolem (..), TyFoundScheme (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)
import Test.Tasty.Providers (TestName)
import Control.Monad.Reader (runReader, asks, MonadReader (..), Reader)
import Data.Sequence (Seq(..))
import Data.Maybe (fromMaybe)
import Data.Functor.Foldable (cata)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq

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

printAlphaTy :: AlphaTyScheme -> Text
printAlphaTy (AlphaTyScheme (Forall bs st)) =
  let bs' = Seq.fromList (fmap (TyVar . T.pack . ("?" ++) . show . unIndex . snd) (toListWithIndex bs))
      sc' = TyScheme (Forall bs' st)
  in printType sc'

reportMissing :: Map AlphaTm AlphaTyScheme -> IO ()
reportMissing tms =
  unless (null tms) $ do
    putStrLn "Did not find terms:"
    for_ (Map.keys tms) (TIO.putStrLn . printAlphaTm)
    fail ("Missing " ++ show (Map.size tms) ++ " terms")

reportIllegal :: AlphaTm -> IO ()
reportIllegal tm = fail ("Found illegal term: " ++ T.unpack (printAlphaTm tm))

reportMismatch :: AlphaTm -> AlphaTyScheme -> AlphaTyScheme -> IO a
reportMismatch tm tyExp tyAct = fail $
  "Found term with type mismatch: " ++ T.unpack (printAlphaTm tm) ++
  " | expected: " ++ T.unpack (printAlphaTy tyExp) ++
  " | actual: " ++ T.unpack (printAlphaTy tyAct)

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

findAll :: Int -> Map AlphaTm AlphaTyScheme -> Set AlphaTm -> SearchSusp Found -> IO ()
findAll !lim !yesTms !noTms !susp =
  if lim <= 0 || Map.null yesTms
    then reportMissing yesTms
    else do
      mx <- rethrow (nextSearchResult susp)
      case mx of
        Nothing -> reportMissing yesTms
        Just (Found tm ty, susp') -> do
          -- TIO.putStrLn (docToText (pretty tm))
          let tmNoLet = inlineLets tm
          let tm' = mapAlphaTm tmNoLet
          -- TIO.putStrLn (printAlphaTm tm')
          if Set.member tm' noTms
            then reportIllegal tm'
            else do
              -- TODO check type before removing
              tms' <- case Map.lookup tm' yesTms of
                Nothing -> pure yesTms
                Just tyExpected -> do
                  let tyActual = forgetTyScheme ty
                  -- TIO.putStrLn (printAlphaTy tyActual)
                  if tyActual == tyExpected
                    then pure (Map.delete tm' yesTms)
                    else reportMismatch tm' tyExpected tyActual
              findAll (lim - 1) tms' noTms susp'

forgetTyScheme :: TyFoundScheme -> AlphaTyScheme
forgetTyScheme (TyFoundScheme (Forall bs st)) = AlphaTyScheme (Forall (void bs) st)

data Match = Match
  { matchTm :: !Text
  , matchTy :: !Text
  } deriving stock (Eq, Show)

testFindsRaw :: TestName -> UseSkolem -> DeclSrc -> Text -> [Match] -> [Text] -> TestTree
testFindsRaw n useSkolem src tyStr yesMatchStrs noTmStrs = testCase n $ do
  ds <- loadDecls src
  tsNamed <- rethrow (parseType tyStr)
  ts <- rethrow (namelessType tsNamed)
  yesTms <- traverse (rethrow . parseTerm . matchTm) yesMatchStrs
  yesTys <- traverse ((rethrow . closeAlphaTyScheme) <=< ((rethrow . parseType) . matchTy)) yesMatchStrs
  noTms <- traverse (rethrow . parseTerm) noTmStrs
  let isKnown (TmVar v) = let k = TmName v in if Map.member k (dsMap ds) then Just k else Nothing
  yesCtms <- traverse (rethrow . closeAlphaTm isKnown) yesTms
  noCtms <- traverse (rethrow . closeAlphaTm isKnown) noTms
  let yesTmMap = Map.fromList (zip yesCtms yesTys)
      noTmSet = Set.fromList noCtms
      conf = SearchConfig ds ts maxSearchDepth useSkolem
      susp = runSearchSusp conf
  findAll maxSearchResults yesTmMap noTmSet susp

testFinds :: TestName -> DeclSrc -> Text -> [Text] -> [Text] -> TestTree
testFinds n src tyStr yesTmStrs noTmStrs =
  let yesTmMatches = fmap (`Match` tyStr) yesTmStrs
  in testFindsRaw n UseSkolemYes src tyStr yesTmMatches noTmStrs

testFindsTy :: TestName -> DeclSrc -> Text -> [Match] -> [Text] -> TestTree
testFindsTy n = testFindsRaw n UseSkolemNo

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
  , testFindsTy "without skolem"
      (DeclSrcList
        [ "forget :: Pair a b -> Pair a a"
        , "thing1 :: Pair Int b"
        , "thing2 :: Pair b Int"
        ]
      )
      "Pair a a"
      [ Match "thing1" "Pair Int Int"
      , Match "thing2" "Pair Int Int"
      , Match "(forget thing1)" "Pair Int Int"
      , Match "(forget thing2)" "Pair a a"
      ]
      []
  , testFinds "GenString"
      (DeclSrcList
       [ "instance IsString String"
       , "fromString :: IsString c => String0 -> c"
       , "primString :: String0"
       ]
      ) "String" ["(fromString primString)"] []
  , testFindsTy "solve constraints"
      (DeclSrcList
        [ "class Foo a"
        , "instance Foo Int"
        , "tm :: Foo a => Pair a b"
        ]
      )
      "Pair a b"
      [ Match "tm" "Pair Int b"
      ]
      []
  -- TODO more tests!!! But the pattern is clear...
  ]
