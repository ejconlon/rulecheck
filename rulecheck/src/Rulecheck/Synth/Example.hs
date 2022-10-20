{-# LANGUAGE OverloadedStrings #-}

module Rulecheck.Synth.Example where

import Data.Map.Strict (Map)
import Data.Sequence (Seq (..))
import IntLike.Map (IntLikeMap)
import IntLike.Set (IntLikeSet)
import Rulecheck.Synth.Align (TyUniq)
import Rulecheck.Synth.Core (Scheme (..), Tm (..), TmName, Ty (..))
import Rulecheck.Synth.Decl (Decl, DeclErr, mkDecls)
import Rulecheck.Synth.Search (TmUnify, TmUniq, outerSearch)

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

exampleSearch :: IO ([TmUniq], IntLikeMap TyUniq (IntLikeSet TmUniq), IntLikeMap TmUniq TmUnify)
exampleSearch = do
  let scheme = Scheme mempty (TyCon "Int" mempty)
  decls <- either (\p -> fail ("Decl err: " ++ show p)) pure exampleDecls
  outerSearch decls scheme 5
