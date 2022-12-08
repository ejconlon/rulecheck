module Main (main) where

import Test.Searchterm.Core (testCore)
import Test.Searchterm.Interface (testInterface)
import Test.Searchterm.Synth.Monad (testMonad)
import Test.Searchterm.Synth.Search (testSearch)
import Test.Searchterm.Synth.UnionMap (testUnionMap)
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main = defaultMain $ testGroup "Searchterm"
  [ testUnionMap
  , testCore
  , testInterface
  , testMonad
  , testSearch
  ]
