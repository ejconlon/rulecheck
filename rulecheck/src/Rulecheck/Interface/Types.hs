module Rulecheck.Interface.Types where

import Data.Sequence (Seq)
import Rulecheck.Interface.Core (TyVar, TyName, TmName, Scheme, Inst, Cls, ModName)

data DataLine = DataLine
  { dlName :: !TyName
  , dlVars :: !(Seq TyVar)
  } deriving stock (Eq, Show)

data ConsLine = ConsLine
  { clName :: !TyName
  , clCons :: !(Seq TmName)
  } deriving stock (Eq, Show)

data InstLine = InstLine
  { ilInst :: !(Inst TyVar)
  , ilParents :: !(Seq (Inst TyVar))
  } deriving stock (Eq, Show)

data FuncLine = FuncLine
  { flName :: !TmName
  , flType :: !(Scheme TyVar)
  } deriving stock (Eq, Show)

data ClsLine = ClsLine
  { clSelf :: !Cls
  , clParents :: !(Seq (Inst TyVar))
  } deriving stock (Eq, Show)

newtype ModLine = ModLine
  { mlName :: ModName
  } deriving stock (Eq, Show)

data Line =
    LineData !DataLine
  | LineCons !ConsLine
  | LineInst !InstLine
  | LineFunc !FuncLine
  | LineCls !ClsLine
  | LineMod !ModLine
  deriving stock (Eq, Show)
