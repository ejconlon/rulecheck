-- File generated by the BNF Converter (bnfc 2.9.4).

-- Templates for pattern matching on abstract syntax

{-# OPTIONS_GHC -fno-warn-unused-matches #-}

module Searchterm.Grammar.Gen.Skel where

import Prelude (($), Either(..), String, (++), Show, show)
import qualified Searchterm.Grammar.Gen.Abs

type Err = Either String
type Result = Err String

failure :: Show a => a -> Result
failure x = Left $ "Undefined case: " ++ show x

transSignedInt :: Searchterm.Grammar.Gen.Abs.SignedInt -> Result
transSignedInt x = case x of
  Searchterm.Grammar.Gen.Abs.SignedInt string -> failure x

transSignedFloat :: Searchterm.Grammar.Gen.Abs.SignedFloat -> Result
transSignedFloat x = case x of
  Searchterm.Grammar.Gen.Abs.SignedFloat string -> failure x

transTyName :: Searchterm.Grammar.Gen.Abs.TyName -> Result
transTyName x = case x of
  Searchterm.Grammar.Gen.Abs.TyName string -> failure x

transTyVar :: Searchterm.Grammar.Gen.Abs.TyVar -> Result
transTyVar x = case x of
  Searchterm.Grammar.Gen.Abs.TyVar string -> failure x

transConName :: Searchterm.Grammar.Gen.Abs.ConName -> Result
transConName x = case x of
  Searchterm.Grammar.Gen.Abs.ConName string -> failure x

transTmName :: Searchterm.Grammar.Gen.Abs.TmName -> Result
transTmName x = case x of
  Searchterm.Grammar.Gen.Abs.TmName string -> failure x

transModName :: Searchterm.Grammar.Gen.Abs.ModName -> Result
transModName x = case x of
  Searchterm.Grammar.Gen.Abs.ModName string -> failure x

transLines :: Searchterm.Grammar.Gen.Abs.Lines -> Result
transLines x = case x of
  Searchterm.Grammar.Gen.Abs.Lines lines -> failure x

transLine :: Searchterm.Grammar.Gen.Abs.Line -> Result
transLine x = case x of
  Searchterm.Grammar.Gen.Abs.LineType tyname tyvars -> failure x
  Searchterm.Grammar.Gen.Abs.LineCons tyname connames -> failure x
  Searchterm.Grammar.Gen.Abs.LineMod modname -> failure x
  Searchterm.Grammar.Gen.Abs.LineLit lits -> failure x

transLit :: Searchterm.Grammar.Gen.Abs.Lit -> Result
transLit x = case x of
  Searchterm.Grammar.Gen.Abs.LitFloat signedfloat -> failure x
  Searchterm.Grammar.Gen.Abs.LitInteger signedint -> failure x
  Searchterm.Grammar.Gen.Abs.LitString string -> failure x
  Searchterm.Grammar.Gen.Abs.LitChar char -> failure x
