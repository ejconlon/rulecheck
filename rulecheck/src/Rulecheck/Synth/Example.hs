{-# LANGUAGE OverloadedStrings #-}

module Rulecheck.Synth.Example where

import Control.Exception (throwIO)
import Data.Map.Strict (Map)
import Data.Sequence (Seq (..))
import Rulecheck.Synth.Core (Scheme (..), Tm (..), TmName, Ty (..))
import Rulecheck.Synth.Decl (Decl, DeclErr, mkDecls)
import Rulecheck.Synth.Search (SearchConfig (..), TmFound, runSearchN)

exampleDecls :: Either (TmName, DeclErr) (Map TmName Decl)
exampleDecls = res where
  tyInt = TyCon "Int" Empty
  tyIntFun2 = TyFun tyInt (TyFun tyInt tyInt)
  tmA = TmFree "a"
  tmB = TmFree "b"
  res = mkDecls
    [ ("myZero", Scheme Empty tyInt, Just (TmKnown "0"))
    , ("myOne", Scheme Empty tyInt, Just (TmKnown "1"))
    , ("myPlus", Scheme Empty tyIntFun2, Just (TmLam "a" (TmLam "b" (TmApp (TmApp (TmKnown "+") tmA) tmB))))
    ]

exampleSearch :: Int -> IO [TmFound]
exampleSearch n = do
  let scheme = Scheme mempty (TyCon "Int" mempty)
  decls <- either (\p -> fail ("Decl err: " ++ show p)) pure exampleDecls
  either throwIO pure (runSearchN (SearchConfig decls scheme 5) n)
