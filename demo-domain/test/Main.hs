module Main where

import Data.Map as Map
import DemoDomain

main :: IO ()
main = do
  print (Var "x" .* Const 1) -- Good: Should output `Var "x"`
  print (Var "x" ./ Var "x") -- Bad: Should output `Const 1`
  print (eval (Map.singleton "x" 0) (Var "x" ./ Var "x")) -- Bad: Should output `Right 1`
