{-# LANGUAGE OverloadedStrings #-}

-- | An example of program synthesis through search.
-- Note that it may be useful to pop into ghci with `make ghci`
-- and run something like:
--
--     import Text.Pretty.Simple (pPrint)
--     x <- exampleSearch 3
--     pPrint x
--
module Searchterm.Synth.Example where

import Control.Exception (throwIO)
import Data.Sequence (Seq (..))
import Searchterm.Interface.Core (ConTy (..), Forall (..), Strained (..), Ty (..), TyScheme (..))
import Searchterm.Interface.Decl (DeclErr, DeclSet, mkDecls)
import Searchterm.Synth.Search (Found, SearchConfig (..), SearchErr, UseSkolem (..), runSearchN, runSearchSusp,
                                takeSearchResults)

-- | Some declarations - here just some "Int" constants and addition.
exampleDecls :: Either DeclErr DeclSet
exampleDecls = res where
  tyInt = TyCon (ConTyKnown "Int") Empty
  tyIntFun2 = TyFun tyInt (TyFun tyInt tyInt)
  res = mkDecls
    [ ("0", TyScheme (Forall Empty (Strained Empty tyInt)))
    , ("1", TyScheme (Forall Empty (Strained Empty tyInt)))
    , ("+", TyScheme (Forall Empty (Strained Empty tyIntFun2)))
    ]

-- | Search for N terms matching the "Int" type.
exampleSearch :: Int -> IO [Found]
exampleSearch n = do
  let scheme = TyScheme (Forall Empty (Strained Empty (TyCon (ConTyKnown "Int") mempty)))
  decls <- either (\p -> fail ("Decl err: " ++ show p)) pure exampleDecls
  either throwIO pure (runSearchN (SearchConfig decls scheme 5 UseSkolemYes) n)

-- | Search for N terms matching the "Int" type (with incremental searching)
exampleSearchSusp :: Int -> IO ([Found], Maybe SearchErr)
exampleSearchSusp n = do
  let scheme = TyScheme (Forall Empty (Strained Empty (TyCon (ConTyKnown "Int") mempty)))
  decls <- either (\p -> fail ("Decl err: " ++ show p)) pure exampleDecls
  let susp = runSearchSusp (SearchConfig decls scheme 5 UseSkolemYes)
      (tms, ea) = takeSearchResults susp n
  pure (tms, either Just (const Nothing) ea)
