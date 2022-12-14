module DemoDomain where

import Control.Exception (Exception)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import GHC.Generics (Generic)
import Test.QuickCheck (Arbitrary)
import Test.QuickCheck.Arbitrary.Generic (GenericArbitrary (..))

data Expr =
    Var String
  | Const Int
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericArbitrary Expr)

{-# NOINLINE (.*) #-}
(.*) :: Expr -> Expr -> Expr
(.*) = Mul

{-# NOINLINE (./) #-}
(./) :: Expr -> Expr -> Expr
(./) = Div

-- Bad rule 1
{-# RULES "mul1" forall x.  x .* Const 1 = x #-}

-- Bad rule 2
{-# RULES "div_id" forall x.  x ./ x = Const 1 #-}

data EvalErr =
    EvalErrDivZero
  | EvalErrMissing String
  deriving stock (Eq, Show)

instance Exception EvalErr

eval :: Map String Int -> Expr -> Either EvalErr Int
eval _ (Const n)  = Right n
eval s (Var v)    = maybe (Left (EvalErrMissing v)) Right (Map.lookup v s)
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
    then Left EvalErrDivZero
    else Right (div x' y')
