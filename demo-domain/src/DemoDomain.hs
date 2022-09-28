module DemoDomain where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

data Expr =
    Var String
  | Const Int
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  deriving (Show)

{-# NOINLINE (.*) #-}
(.*) :: Expr -> Expr -> Expr
(.*) = Mul

{-# NOINLINE (./) #-}
(./) :: Expr -> Expr -> Expr
(./) = Div

-- Good rule
{-# RULES "mul1" forall x.  x .* Const 1 = x #-}

-- Bad rule
{-# RULES "div_id" forall x.  x ./ x = Const 1 #-}

eval :: Map String Int -> Expr -> Maybe Int
eval _ (Const n)  = Just n
eval s (Var v)    = Map.lookup v s
eval s (Add x y)  = do
  x' <- eval s x
  y' <- eval s y
  return $ x' + y'
eval s (Sub x y)  = do
  x' <- eval s x
  y' <- eval s y
  return $ x' - y'
eval s (Mul x y)  = do
  x' <- eval s x
  y' <- eval s y
  return $ x' * y'
eval s (Div x y)  = do
  x' <- eval s x
  y' <- eval s y
  if y' == 0
    then Nothing
    else Just (div x' y')
