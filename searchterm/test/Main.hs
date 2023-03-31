module Main (main) where

import Test.Searchterm.Interface (testInterface)
import Test.Searchterm.NewInterface (testNewInterface)
import Test.Searchterm.Synth.Monad (testMonad)
import Test.Searchterm.Synth.Search (testSearch)
import Test.Searchterm.Synth.UnionMap (testUnionMap)
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main = defaultMain $ testGroup "Searchterm"
  [ testUnionMap
  , testInterface
  , testNewInterface
  , testMonad
  , testSearch
  ]
