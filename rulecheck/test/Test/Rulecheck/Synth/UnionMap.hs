module Test.Rulecheck.Synth.UnionMap
  ( testUnionMap
  ) where

import Data.Bifunctor (bimap, first)
import Data.Char (ord)
import Data.Semigroup (Max)
import GHC.Char (chr)
import IntLike.Map (IntLikeMap)
import qualified IntLike.Map as ILM
import IntLike.Set (IntLikeSet)
import qualified IntLike.Set as ILS
import Rulecheck.Synth.UnionFind (MergeRes (..))
import qualified Rulecheck.Synth.UnionFind as UF
import Rulecheck.Synth.UnionMap (MergeFun, UnionMap)
import qualified Rulecheck.Synth.UnionMap as UM
import Test.Rulecheck.State (applyS, applyTestS, applyTestST, runS, testS)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

newtype V = V { unV :: Int }
  deriving stock (Show)
  deriving newtype (Eq, Ord)

toV :: Char -> V
toV = V . ord

fromV :: V -> Char
fromV = chr . unV

setV :: String -> IntLikeSet V
setV = ILS.fromList . fmap toV

mapV :: [(Char, a)] -> IntLikeMap V a
mapV = ILM.fromList . fmap (first toV)

mapVV :: [(Char, Char)] -> IntLikeMap V V
mapVV = ILM.fromList . fmap (bimap toV toV)

multiMapVV :: [(Char, String)] -> IntLikeMap V (IntLikeSet V)
multiMapVV = ILM.fromList . fmap (bimap toV setV)

type UMV = UnionMap V (Max Int)

emptyUMV :: UMV
emptyUMV = UM.empty

mergeUMV :: Applicative m => MergeFun m (Max Int)
mergeUMV a b = pure (a <> b)

unionUMV :: UMV -> IntLikeMap V (IntLikeSet V)
unionUMV = fst . UF.members . UM.unionFind

testUmSimple :: TestTree
testUmSimple = testCase "UM simple" $ runS emptyUMV $ do
  -- start with empty map
  testS $ \um -> UM.size um @?= 0
  -- add 'a'
  applyTestS (UM.stateInsert (toV 'a') 1) $ \_ um -> do
    UM.size um @?= 1
    UM.valueMap um @?= mapV [('a', 1)]
  -- lookup 'a'
  applyTestS (UM.stateFind (toV 'a')) $ \res _ -> do
    res @?= Just (toV 'a', 1)
  -- try to add 'a' again
  applyTestS (UM.stateInsert (toV 'a') 1) $ \_ um -> do
    UM.size um @?= 1
  -- add 'b' and 'c' and check them
  _ <- applyS (UM.stateInsert (toV 'b') 2)
  _ <- applyS (UM.stateInsert (toV 'c') 3)
  testS $ \um -> do
    UM.size um @?= 3
    UM.valueMap um @?= mapV [('a', 1), ('b', 2), ('c', 3)]
    unionUMV um @?= multiMapVV [('a', "a"), ('b', "b"), ('c', "c")]
  -- merge 'a' and 'c'
  applyTestST (UM.stateMerge mergeUMV (toV 'a') (toV 'c')) $ \res um -> do
    res @?= MergeResChanged (toV 'a') (toV 'c')
    UM.size um @?= 2
    UM.valueMap um @?= mapV [('a', 3), ('b', 2)]
    unionUMV um @?= multiMapVV [('a', "ac"), ('b', "b")]
  -- try to merge again
  applyTestST (UM.stateMerge mergeUMV (toV 'a') (toV 'c')) $ \res _ ->
    res @?= MergeResUnchanged (toV 'a')
  -- and the other way around
  applyTestST (UM.stateMerge mergeUMV (toV 'c') (toV 'a')) $ \res _ ->
    res @?= MergeResUnchanged (toV 'a')
  -- and a non-existent merge
  applyTestST (UM.stateMerge mergeUMV (toV 'b') (toV 'z')) $ \res _ ->
    res @?= MergeResMissing (toV 'z')
  -- and the other way around
  applyTestS (UM.stateMerge mergeUMV (toV 'z') (toV 'b')) $ \res _ ->
    res @?= MergeResMissing (toV 'z')
  -- final view
  testS $ \um -> do
    UM.valueMap um @?= mapV [('a', 3), ('b', 2)]
    unionUMV um @?= multiMapVV [('a', "ac"), ('b', "b")]

testUmRec :: TestTree
testUmRec = testCase "UM rec" $ runS emptyUMV $ do
  _ <- applyS (UM.stateInsert (toV 'a') 1)
  _ <- applyS (UM.stateInsert (toV 'b') 2)
  _ <- applyS (UM.stateInsert (toV 'c') 3)
  applyTestS (UM.stateMerge mergeUMV (toV 'b') (toV 'c')) $ \res um -> do
    res @?= MergeResChanged (toV 'b') (toV 'c')
    UM.size um @?= 2
    UM.valueMap um @?= mapV [('a', 1), ('b', 3)]
    unionUMV um @?= multiMapVV [('a', "a"), ('b', "bc")]
  applyTestS (UM.stateMerge mergeUMV (toV 'a') (toV 'b')) $ \res um -> do
    res @?= MergeResChanged (toV 'a') (toV 'b')
    UM.size um @?= 1
    UM.valueMap um @?= mapV [('a', 3)]
    unionUMV um @?= multiMapVV [('a', "abc")]

-- may seem redundant but this should trigger different cases
-- in path compression
testUmTail :: TestTree
testUmTail = testCase "UM tail" $ runS emptyUMV $ do
  applyS $ do
    _ <- UM.stateInsert (toV 'a') 1
    _ <- UM.stateInsert (toV 'b') 2
    _ <- UM.stateInsert (toV 'c') 3
    _ <- UM.stateInsert (toV 'd') 4
    _ <- UM.stateMerge mergeUMV (toV 'c') (toV 'd')
    _ <- UM.stateMerge mergeUMV (toV 'b') (toV 'c')
    _ <- UM.stateMerge mergeUMV (toV 'a') (toV 'b')
    pure ()
  testS $ \um -> do
    UM.valueMap um @?= mapV [('a', 4)]
    unionUMV um @?= multiMapVV [('a', "abcd")]

testUnionMap :: TestTree
testUnionMap = testGroup "UM unit"
  [ testUmSimple
  , testUmRec
  , testUmTail
  ]
