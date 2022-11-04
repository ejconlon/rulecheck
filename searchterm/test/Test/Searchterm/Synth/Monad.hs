{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Monad law, right identity" #-}

module Test.Searchterm.Synth.Monad (testMonad) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Data.Void (Void, absurd)
import Searchterm.Synth.Monad (Track, TrackSt (..), runManyTrack)
import Control.Monad.Logic (MonadLogic (..), reflect)
import Control.Applicative (Alternative (..))
import Control.Monad.State.Strict (MonadState (..), modify')

testMonad :: TestTree
testMonad = testGroup "Monad" [testBwd]

testBwd :: TestTree
testBwd = testCase "bwd" $ do
  let res2 = [2, 1] :: [Int]
      res3 = [1, 2, 3] :: [Int]
      res3' = [1, 3, 2] :: [Int]
      st0 = TrackSt 0 0 :: TrackSt Int Int
  trackRun (pure 2 <|> pure 1) @?= (res2, st0)
  trackRun (pure 1 <|> (pure 2 <|> pure 3)) @?= (res3, st0)
  trackRun ((pure 1 <|> pure 2) <|> pure 3) @?= (res3, st0)
  trackRun (interleave (pure 2) (pure 1)) @?= (res2, st0)
  trackRun (interleave (pure 1) (interleave (pure 2) (pure 3))) @?= (res3, st0)
  trackRun (interleave (interleave (pure 1) (pure 2)) (pure 3)) @?= (res3', st0)
  trackRun trackAlt0 @?= (res2, st0)
  trackRun trackAlt1 @?= (res2, st0)
  trackRun trackAlt2 @?= (res2, st0)
  trackRun trackAlt3 @?= (res2, st0)
  trackRun trackSplit0 @?= (res2, st0)
  trackRun (splitRef trackAlt0) @?= (res2, st0)
  trackRun (splitRef trackAlt1) @?= (res2, st0)
  trackRun (splitRef trackAlt2) @?= (res2, st0)
  trackRun (splitRef trackAlt3) @?= (res2, st0)
  trackRun (trackAlt0 >>= pure) @?= (res2, st0)
  trackRun (trackAlt0 >>- pure) @?= (res2, st0)
  trackRun (trackAlt1 >>= pure) @?= (res2, st0)
  trackRun (trackAlt1 >>- pure) @?= (res2, st0)
  trackRun (trackAlt2 >>= pure) @?= (res2, st0)
  trackRun (trackAlt2 >>- pure) @?= (res2, st0)
  trackRun (trackAlt3 >>= pure) @?= (res2, st0)
  trackRun (trackAlt3 >>- pure) @?= (res2, st0)
  trackRun trackInt0 @?= (res2, st0)
  trackRun trackInt1 @?= (res2, st0)
  trackRun trackInt2 @?= (res2, st0)
  trackRun trackInt3 @?= (res2, st0)

type T = Track () Int Int Void

-- trackIncFwd :: T ()
-- trackIncFwd = modify' (\st -> st { tsFwd = tsFwd st + 1})

trackIncBwd :: T ()
trackIncBwd = modify' (\st -> st { tsBwd = tsBwd st + 1})

trackRun :: T a -> ([a], TrackSt Int Int)
trackRun m = either absurd id (runManyTrack 100 m () (TrackSt 0 0))

-- split-reflect - should be identical to m...
-- It's not for us! But that's ok? (As long as there's a backtrack on the outside)
splitRef :: T a -> T a
splitRef m = msplit m >>= reflect

-- This should behave like trackAlt0
trackSplit0 :: T Int
trackSplit0 = trackIncBwd >> splitRef ((trackIncBwd >> fmap tsBwd get) <|> fmap tsBwd get)

-- These are the alt versions of the interleaving versions below
trackAlt0, trackAlt1, trackAlt2, trackAlt3 :: T Int
trackAlt0 = trackIncBwd >> ((trackIncBwd >> fmap tsBwd get) <|> fmap tsBwd get)
trackAlt1 = (trackIncBwd >> trackIncBwd >> fmap tsBwd get) <|> (trackIncBwd >> fmap tsBwd get)
trackAlt2 = ((trackIncBwd >> trackIncBwd) <|> trackIncBwd) >> fmap tsBwd get
trackAlt3 = (trackIncBwd <|> pure ()) >> trackIncBwd >> fmap tsBwd get

-- These are the interleaving versions of the alt versions above
trackInt0, trackInt1, trackInt2, trackInt3 :: T Int
trackInt0 = trackIncBwd >> interleave (trackIncBwd >> fmap tsBwd get) (fmap tsBwd get)
trackInt1 = interleave (trackIncBwd >> trackIncBwd >> fmap tsBwd get) (trackIncBwd >> fmap tsBwd get)
trackInt2 = interleave (trackIncBwd >> trackIncBwd) trackIncBwd >> fmap tsBwd get
trackInt3 = interleave trackIncBwd (pure ()) >> trackIncBwd >> fmap tsBwd get
