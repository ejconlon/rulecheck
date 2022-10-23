{-# LANGUAGE OverloadedStrings #-}

module Rulecheck.Synth.Example where

import Control.Exception (throwIO)
import Data.Map.Strict (Map)
import Data.Sequence (Seq (..))
import Rulecheck.Synth.Core (Scheme (..), TmName, Ty (..))
import Rulecheck.Synth.Decl (Decl, DeclErr, mkDecls)
import Rulecheck.Synth.Search (SearchConfig (..), TmFound, runSearchN)

exampleDecls :: Either (TmName, DeclErr) (Map TmName Decl)
exampleDecls = res where
  tyInt = TyCon "Int" Empty
  tyIntFun2 = TyFun tyInt (TyFun tyInt tyInt)
  res = mkDecls
    [ ("myZero", Scheme Empty tyInt)
    , ("myOne", Scheme Empty tyInt)
    , ("myPlus", Scheme Empty tyIntFun2)
    ]

exampleSearch :: Int -> IO [TmFound]
exampleSearch n = do
  let scheme = Scheme mempty (TyCon "Int" mempty)
  decls <- either (\p -> fail ("Decl err: " ++ show p)) pure exampleDecls
  either throwIO pure (runSearchN (SearchConfig decls scheme 5) n)
