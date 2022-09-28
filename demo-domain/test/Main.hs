module Main where

import DemoDomain
import Data.Map as Map

main :: IO ()
main = do
  print (Var "x" .* Const 1) -- Good: Should output `Var "x"`
  print (Var "x" ./ Var "x") -- Bad: Should output `Const 1`
  print (eval (Map.singleton "x" 0) (Var "x" ./ Var "x")) -- Bad: Should output `1`
