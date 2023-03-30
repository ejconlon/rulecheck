-- File generated by the BNF Converter (bnfc 2.9.4).

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
#if __GLASGOW_HASKELL__ <= 708
{-# LANGUAGE OverlappingInstances #-}
#endif

-- | Pretty-printer for Searchterm.

module Searchterm.Grammar.Gen.Print where

import Prelude
  ( ($), (.)
  , Bool(..), (==), (<)
  , Int, Integer, Double, (+), (-), (*)
  , String, (++)
  , ShowS, showChar, showString
  , all, elem, foldr, id, map, null, replicate, shows, span
  )
import Data.Char ( Char, isSpace )
import qualified Searchterm.Grammar.Gen.Abs
import qualified Data.Text

-- | The top-level printing method.

printTree :: Print a => a -> String
printTree = render . prt 0

type Doc = [ShowS] -> [ShowS]

doc :: ShowS -> Doc
doc = (:)

render :: Doc -> String
render d = rend 0 False (map ($ "") $ d []) ""
  where
  rend
    :: Int        -- ^ Indentation level.
    -> Bool       -- ^ Pending indentation to be output before next character?
    -> [String]
    -> ShowS
  rend i p = \case
      "["      :ts -> char '[' . rend i False ts
      "("      :ts -> char '(' . rend i False ts
      "{"      :ts -> onNewLine i     p . showChar   '{'  . new (i+1) ts
      "}" : ";":ts -> onNewLine (i-1) p . showString "};" . new (i-1) ts
      "}"      :ts -> onNewLine (i-1) p . showChar   '}'  . new (i-1) ts
      [";"]        -> char ';'
      ";"      :ts -> char ';' . new i ts
      t  : ts@(s:_) | closingOrPunctuation s
                   -> pending . showString t . rend i False ts
      t        :ts -> pending . space t      . rend i False ts
      []           -> id
    where
    -- Output character after pending indentation.
    char :: Char -> ShowS
    char c = pending . showChar c

    -- Output pending indentation.
    pending :: ShowS
    pending = if p then indent i else id

  -- Indentation (spaces) for given indentation level.
  indent :: Int -> ShowS
  indent i = replicateS (2*i) (showChar ' ')

  -- Continue rendering in new line with new indentation.
  new :: Int -> [String] -> ShowS
  new j ts = showChar '\n' . rend j True ts

  -- Make sure we are on a fresh line.
  onNewLine :: Int -> Bool -> ShowS
  onNewLine i p = (if p then id else showChar '\n') . indent i

  -- Separate given string from following text by a space (if needed).
  space :: String -> ShowS
  space t s =
    case (all isSpace t', null spc, null rest) of
      (True , _   , True ) -> []              -- remove trailing space
      (False, _   , True ) -> t'              -- remove trailing space
      (False, True, False) -> t' ++ ' ' : s   -- add space if none
      _                    -> t' ++ s
    where
      t'          = showString t []
      (spc, rest) = span isSpace s

  closingOrPunctuation :: String -> Bool
  closingOrPunctuation [c] = c `elem` closerOrPunct
  closingOrPunctuation _   = False

  closerOrPunct :: String
  closerOrPunct = ")],;"

parenth :: Doc -> Doc
parenth ss = doc (showChar '(') . ss . doc (showChar ')')

concatS :: [ShowS] -> ShowS
concatS = foldr (.) id

concatD :: [Doc] -> Doc
concatD = foldr (.) id

replicateS :: Int -> ShowS -> ShowS
replicateS n f = concatS (replicate n f)

-- | The printer class does the job.

class Print a where
  prt :: Int -> a -> Doc

instance {-# OVERLAPPABLE #-} Print a => Print [a] where
  prt i = concatD . map (prt i)

instance Print Char where
  prt _ c = doc (showChar '\'' . mkEsc '\'' c . showChar '\'')

instance Print String where
  prt _ = printString

printString :: String -> Doc
printString s = doc (showChar '"' . concatS (map (mkEsc '"') s) . showChar '"')

mkEsc :: Char -> Char -> ShowS
mkEsc q = \case
  s | s == q -> showChar '\\' . showChar s
  '\\' -> showString "\\\\"
  '\n' -> showString "\\n"
  '\t' -> showString "\\t"
  s -> showChar s

prPrec :: Int -> Int -> Doc -> Doc
prPrec i j = if j < i then parenth else id

instance Print Integer where
  prt _ x = doc (shows x)

instance Print Double where
  prt _ x = doc (shows x)

instance Print Searchterm.Grammar.Gen.Abs.Ident where
  prt _ (Searchterm.Grammar.Gen.Abs.Ident i) = doc $ showString (Data.Text.unpack i)
instance Print (Searchterm.Grammar.Gen.Abs.Prog' a) where
  prt i = \case
    Searchterm.Grammar.Gen.Abs.Prog _ decls -> prPrec i 0 (concatD [prt 0 decls])

instance Print (Searchterm.Grammar.Gen.Abs.Decl' a) where
  prt i = \case
    Searchterm.Grammar.Gen.Abs.DeclClass _ classdecl -> prPrec i 0 (concatD [prt 0 classdecl])
    Searchterm.Grammar.Gen.Abs.DeclFunc _ funcdecl -> prPrec i 0 (concatD [prt 0 funcdecl])
    Searchterm.Grammar.Gen.Abs.DeclVar _ vardecl -> prPrec i 0 (concatD [prt 0 vardecl, doc (showString ";")])
    Searchterm.Grammar.Gen.Abs.DeclStmt _ stmt -> prPrec i 0 (concatD [prt 0 stmt])

instance Print (Searchterm.Grammar.Gen.Abs.ClassDecl' a) where
  prt i = \case
    Searchterm.Grammar.Gen.Abs.ClassDecl _ id_ mayextend funcdecls -> prPrec i 0 (concatD [doc (showString "class"), prt 0 id_, prt 0 mayextend, doc (showString "{"), prt 0 funcdecls, doc (showString "}")])

instance Print (Searchterm.Grammar.Gen.Abs.FuncDecl' a) where
  prt i = \case
    Searchterm.Grammar.Gen.Abs.FuncDecl _ id_ params block -> prPrec i 0 (concatD [doc (showString "fun"), prt 0 id_, doc (showString "("), prt 0 params, doc (showString ")"), prt 0 block])

instance Print (Searchterm.Grammar.Gen.Abs.VarDecl' a) where
  prt i = \case
    Searchterm.Grammar.Gen.Abs.VarDecl _ id_ maydef -> prPrec i 0 (concatD [doc (showString "var"), prt 0 id_, prt 0 maydef])

instance Print (Searchterm.Grammar.Gen.Abs.Extend' a) where
  prt i = \case
    Searchterm.Grammar.Gen.Abs.Extend _ id_ -> prPrec i 0 (concatD [prt 0 id_])

instance Print (Searchterm.Grammar.Gen.Abs.Def' a) where
  prt i = \case
    Searchterm.Grammar.Gen.Abs.Def _ exp -> prPrec i 0 (concatD [prt 0 exp])

instance Print (Searchterm.Grammar.Gen.Abs.Stmt' a) where
  prt i = \case
    Searchterm.Grammar.Gen.Abs.StmtFor _ forstmt -> prPrec i 0 (concatD [prt 0 forstmt])
    Searchterm.Grammar.Gen.Abs.StmtIf _ ifstmt -> prPrec i 0 (concatD [prt 0 ifstmt])
    Searchterm.Grammar.Gen.Abs.StmtPrint _ exp -> prPrec i 0 (concatD [doc (showString "print"), prt 0 exp, doc (showString ";")])
    Searchterm.Grammar.Gen.Abs.StmtReturn _ exp -> prPrec i 0 (concatD [doc (showString "return"), prt 0 exp, doc (showString ";")])
    Searchterm.Grammar.Gen.Abs.StmtWhile _ whilestmt -> prPrec i 0 (concatD [prt 0 whilestmt])
    Searchterm.Grammar.Gen.Abs.StmtBlock _ block -> prPrec i 0 (concatD [prt 0 block])
    Searchterm.Grammar.Gen.Abs.StmtAssign _ assign -> prPrec i 0 (concatD [prt 0 assign, doc (showString ";")])
    Searchterm.Grammar.Gen.Abs.StmtCall _ call -> prPrec i 0 (concatD [prt 0 call, doc (showString ";")])

instance Print (Searchterm.Grammar.Gen.Abs.ForStmt' a) where
  prt i = \case
    Searchterm.Grammar.Gen.Abs.ForStmt _ mayforinit mayexp maystmt stmt -> prPrec i 0 (concatD [doc (showString "for"), doc (showString "("), prt 0 mayforinit, doc (showString ";"), prt 0 mayexp, doc (showString ";"), prt 0 maystmt, doc (showString ")"), prt 0 stmt])

instance Print (Searchterm.Grammar.Gen.Abs.ForInit' a) where
  prt i = \case
    Searchterm.Grammar.Gen.Abs.ForInitDecl _ vardecl -> prPrec i 0 (concatD [prt 0 vardecl])
    Searchterm.Grammar.Gen.Abs.ForInitAssign _ assign -> prPrec i 0 (concatD [prt 0 assign])

instance Print (Searchterm.Grammar.Gen.Abs.Assign' a) where
  prt i = \case
    Searchterm.Grammar.Gen.Abs.Assign _ target exp -> prPrec i 0 (concatD [prt 0 target, doc (showString "="), prt 0 exp])

instance Print (Searchterm.Grammar.Gen.Abs.IfStmt' a) where
  prt i = \case
    Searchterm.Grammar.Gen.Abs.IfStmt _ exp stmt mayelse -> prPrec i 0 (concatD [doc (showString "if"), doc (showString "("), prt 0 exp, doc (showString ")"), prt 0 stmt, prt 0 mayelse])

instance Print (Searchterm.Grammar.Gen.Abs.Else' a) where
  prt i = \case
    Searchterm.Grammar.Gen.Abs.Else _ stmt -> prPrec i 0 (concatD [doc (showString "else"), prt 0 stmt])

instance Print (Searchterm.Grammar.Gen.Abs.WhileStmt' a) where
  prt i = \case
    Searchterm.Grammar.Gen.Abs.WhileStmt _ exp stmt -> prPrec i 0 (concatD [doc (showString "while"), doc (showString "("), prt 0 exp, doc (showString ")"), prt 0 stmt])

instance Print (Searchterm.Grammar.Gen.Abs.Block' a) where
  prt i = \case
    Searchterm.Grammar.Gen.Abs.Block _ decls -> prPrec i 0 (concatD [doc (showString "{"), prt 0 decls, doc (showString "}")])

instance Print (Searchterm.Grammar.Gen.Abs.KnownTarget' a) where
  prt i = \case
    Searchterm.Grammar.Gen.Abs.KnownTarget _ knowntargethead mayknowntargettail -> prPrec i 0 (concatD [prt 0 knowntargethead, prt 0 mayknowntargettail])

instance Print (Searchterm.Grammar.Gen.Abs.KnownTargetHead' a) where
  prt i = \case
    Searchterm.Grammar.Gen.Abs.KnownTargetHeadThis _ -> prPrec i 0 (concatD [doc (showString "this")])
    Searchterm.Grammar.Gen.Abs.KnownTargetHeadSuper _ -> prPrec i 0 (concatD [doc (showString "super")])
    Searchterm.Grammar.Gen.Abs.KnownTargetHeadIdent _ id_ -> prPrec i 0 (concatD [prt 0 id_])

instance Print (Searchterm.Grammar.Gen.Abs.KnownTargetTail' a) where
  prt i = \case
    Searchterm.Grammar.Gen.Abs.KnownTargetTail _ id_ mayknowntargettail -> prPrec i 0 (concatD [doc (showString "."), prt 0 id_, prt 0 mayknowntargettail])

instance Print (Searchterm.Grammar.Gen.Abs.Exp' a) where
  prt i = \case
    Searchterm.Grammar.Gen.Abs.ExpNil _ -> prPrec i 0 (concatD [doc (showString "nil")])
    Searchterm.Grammar.Gen.Abs.ExpTarget _ target -> prPrec i 0 (concatD [prt 0 target])
    Searchterm.Grammar.Gen.Abs.ExpVar _ id_ -> prPrec i 0 (concatD [prt 0 id_])
    Searchterm.Grammar.Gen.Abs.ExpLit _ lit -> prPrec i 0 (concatD [prt 0 lit])
    Searchterm.Grammar.Gen.Abs.ExpOp _ op -> prPrec i 0 (concatD [prt 0 op])
    Searchterm.Grammar.Gen.Abs.ExpCall _ call -> prPrec i 0 (concatD [prt 0 call])

instance Print (Searchterm.Grammar.Gen.Abs.Call' a) where
  prt i = \case
    Searchterm.Grammar.Gen.Abs.Call _ target args -> prPrec i 0 (concatD [prt 0 target, doc (showString "("), prt 0 args, doc (showString ")")])

instance Print (Searchterm.Grammar.Gen.Abs.Target' a) where
  prt i = \case
    Searchterm.Grammar.Gen.Abs.TargetKnown _ knowntarget -> prPrec i 0 (concatD [prt 0 knowntarget])
    Searchterm.Grammar.Gen.Abs.TargetExp _ exp -> prPrec i 0 (concatD [doc (showString "("), prt 0 exp, doc (showString ")")])

instance Print (Searchterm.Grammar.Gen.Abs.Lit' a) where
  prt i = \case
    Searchterm.Grammar.Gen.Abs.LitBool _ boollit -> prPrec i 0 (concatD [prt 0 boollit])
    Searchterm.Grammar.Gen.Abs.LitInt _ n -> prPrec i 0 (concatD [prt 0 n])
    Searchterm.Grammar.Gen.Abs.LitString _ str -> prPrec i 0 (concatD [printString str])

instance Print (Searchterm.Grammar.Gen.Abs.BoolLit' a) where
  prt i = \case
    Searchterm.Grammar.Gen.Abs.BoolLitTrue _ -> prPrec i 0 (concatD [doc (showString "true")])
    Searchterm.Grammar.Gen.Abs.BoolLitFalse _ -> prPrec i 0 (concatD [doc (showString "false")])

instance Print (Searchterm.Grammar.Gen.Abs.Op' a) where
  prt i = \case
    Searchterm.Grammar.Gen.Abs.OpBool _ boolop -> prPrec i 0 (concatD [prt 0 boolop])
    Searchterm.Grammar.Gen.Abs.OpNum _ numop -> prPrec i 0 (concatD [prt 0 numop])
    Searchterm.Grammar.Gen.Abs.OpCmp _ cmpop -> prPrec i 0 (concatD [prt 0 cmpop])
    Searchterm.Grammar.Gen.Abs.OpStr _ strop -> prPrec i 0 (concatD [prt 0 strop])

instance Print (Searchterm.Grammar.Gen.Abs.BoolOp' a) where
  prt i = \case
    Searchterm.Grammar.Gen.Abs.BoolOpBin _ exp1 boolbinop exp2 -> prPrec i 0 (concatD [prt 0 exp1, prt 0 boolbinop, prt 0 exp2])
    Searchterm.Grammar.Gen.Abs.BoolOpUn _ boolunop exp -> prPrec i 0 (concatD [prt 0 boolunop, prt 0 exp])

instance Print (Searchterm.Grammar.Gen.Abs.BoolBinOp' a) where
  prt i = \case
    Searchterm.Grammar.Gen.Abs.BoolBinOpOr _ -> prPrec i 0 (concatD [doc (showString "or")])
    Searchterm.Grammar.Gen.Abs.BoolBinOpAnd _ -> prPrec i 0 (concatD [doc (showString "and")])

instance Print (Searchterm.Grammar.Gen.Abs.BoolUnOp' a) where
  prt i = \case
    Searchterm.Grammar.Gen.Abs.BoolUnOpNot _ -> prPrec i 0 (concatD [doc (showString "!")])

instance Print (Searchterm.Grammar.Gen.Abs.CmpOp' a) where
  prt i = \case
    Searchterm.Grammar.Gen.Abs.CmpOpBin _ exp1 cmpbinop exp2 -> prPrec i 0 (concatD [prt 0 exp1, prt 0 cmpbinop, prt 0 exp2])

instance Print (Searchterm.Grammar.Gen.Abs.CmpBinOp' a) where
  prt i = \case
    Searchterm.Grammar.Gen.Abs.CmpBinOpEq _ -> prPrec i 0 (concatD [doc (showString "==")])
    Searchterm.Grammar.Gen.Abs.CmpBinOpNe _ -> prPrec i 0 (concatD [doc (showString "!=")])
    Searchterm.Grammar.Gen.Abs.CmpBinOpGt _ -> prPrec i 0 (concatD [doc (showString ">")])
    Searchterm.Grammar.Gen.Abs.CmpBinOpGe _ -> prPrec i 0 (concatD [doc (showString ">=")])
    Searchterm.Grammar.Gen.Abs.CmpBinOpLt _ -> prPrec i 0 (concatD [doc (showString "<")])
    Searchterm.Grammar.Gen.Abs.CmpBinOpLe _ -> prPrec i 0 (concatD [doc (showString "<=")])

instance Print (Searchterm.Grammar.Gen.Abs.NumOp' a) where
  prt i = \case
    Searchterm.Grammar.Gen.Abs.NumOpBin _ exp1 numbinop exp2 -> prPrec i 0 (concatD [prt 0 exp1, prt 0 numbinop, prt 0 exp2])
    Searchterm.Grammar.Gen.Abs.NumOpUn _ numunop exp -> prPrec i 0 (concatD [prt 0 numunop, prt 0 exp])

instance Print (Searchterm.Grammar.Gen.Abs.NumBinOp' a) where
  prt i = \case
    Searchterm.Grammar.Gen.Abs.NumBinOpAdd _ -> prPrec i 0 (concatD [doc (showString "+")])
    Searchterm.Grammar.Gen.Abs.NumBinOpSub _ -> prPrec i 0 (concatD [doc (showString "-")])
    Searchterm.Grammar.Gen.Abs.NumBinOpMul _ -> prPrec i 0 (concatD [doc (showString "*")])
    Searchterm.Grammar.Gen.Abs.NumBinOpDiv _ -> prPrec i 0 (concatD [doc (showString "/")])

instance Print (Searchterm.Grammar.Gen.Abs.NumUnOp' a) where
  prt i = \case
    Searchterm.Grammar.Gen.Abs.NumUnOpNeg _ -> prPrec i 0 (concatD [doc (showString "-")])

instance Print (Searchterm.Grammar.Gen.Abs.StrOp' a) where
  prt i = \case
    Searchterm.Grammar.Gen.Abs.StrOpBin _ exp1 strbinop exp2 -> prPrec i 0 (concatD [prt 0 exp1, prt 0 strbinop, prt 0 exp2])

instance Print (Searchterm.Grammar.Gen.Abs.StrBinOp' a) where
  prt i = \case
    Searchterm.Grammar.Gen.Abs.StrBinOpAppend _ -> prPrec i 0 (concatD [doc (showString "++")])

instance Print (Searchterm.Grammar.Gen.Abs.Param' a) where
  prt i = \case
    Searchterm.Grammar.Gen.Abs.Param _ id_ -> prPrec i 0 (concatD [prt 0 id_])

instance Print (Searchterm.Grammar.Gen.Abs.Arg' a) where
  prt i = \case
    Searchterm.Grammar.Gen.Abs.Arg _ exp -> prPrec i 0 (concatD [prt 0 exp])

instance Print [Searchterm.Grammar.Gen.Abs.Decl' a] where
  prt _ [] = concatD []
  prt _ (x:xs) = concatD [prt 0 x, prt 0 xs]

instance Print [Searchterm.Grammar.Gen.Abs.FuncDecl' a] where
  prt _ [] = concatD []
  prt _ (x:xs) = concatD [prt 0 x, prt 0 xs]

instance Print [Searchterm.Grammar.Gen.Abs.Param' a] where
  prt _ [] = concatD []
  prt _ (x:xs) = concatD [prt 0 x, doc (showString ","), prt 0 xs]

instance Print [Searchterm.Grammar.Gen.Abs.Arg' a] where
  prt _ [] = concatD []
  prt _ (x:xs) = concatD [prt 0 x, doc (showString ","), prt 0 xs]

instance Print (Searchterm.Grammar.Gen.Abs.MayExtend' a) where
  prt i = \case
    Searchterm.Grammar.Gen.Abs.MayExtendNone _ -> prPrec i 0 (concatD [])
    Searchterm.Grammar.Gen.Abs.MayExtendSome _ extend -> prPrec i 0 (concatD [doc (showString "<"), prt 0 extend])

instance Print (Searchterm.Grammar.Gen.Abs.MayDef' a) where
  prt i = \case
    Searchterm.Grammar.Gen.Abs.MayDefNone _ -> prPrec i 0 (concatD [])
    Searchterm.Grammar.Gen.Abs.MayDefSome _ def -> prPrec i 0 (concatD [doc (showString "="), prt 0 def])

instance Print (Searchterm.Grammar.Gen.Abs.MayExp' a) where
  prt i = \case
    Searchterm.Grammar.Gen.Abs.MayExpNone _ -> prPrec i 0 (concatD [])
    Searchterm.Grammar.Gen.Abs.MayExpSome _ exp -> prPrec i 0 (concatD [prt 0 exp])

instance Print (Searchterm.Grammar.Gen.Abs.MayVarDecl' a) where
  prt i = \case
    Searchterm.Grammar.Gen.Abs.MayVarDeclNone _ -> prPrec i 0 (concatD [])
    Searchterm.Grammar.Gen.Abs.MayVarDeclSome _ vardecl -> prPrec i 0 (concatD [prt 0 vardecl])

instance Print (Searchterm.Grammar.Gen.Abs.MayElse' a) where
  prt i = \case
    Searchterm.Grammar.Gen.Abs.MayElseNone _ -> prPrec i 0 (concatD [])
    Searchterm.Grammar.Gen.Abs.MayElseSome _ else_ -> prPrec i 0 (concatD [prt 0 else_])

instance Print (Searchterm.Grammar.Gen.Abs.MayStmt' a) where
  prt i = \case
    Searchterm.Grammar.Gen.Abs.MayStmtNone _ -> prPrec i 0 (concatD [])
    Searchterm.Grammar.Gen.Abs.MayStmtSome _ stmt -> prPrec i 0 (concatD [prt 0 stmt])

instance Print (Searchterm.Grammar.Gen.Abs.MayForInit' a) where
  prt i = \case
    Searchterm.Grammar.Gen.Abs.MayForInitNone _ -> prPrec i 0 (concatD [])
    Searchterm.Grammar.Gen.Abs.MayForInitSome _ forinit -> prPrec i 0 (concatD [prt 0 forinit])

instance Print (Searchterm.Grammar.Gen.Abs.MayKnownTargetTail' a) where
  prt i = \case
    Searchterm.Grammar.Gen.Abs.MayKnownTargetTailNone _ -> prPrec i 0 (concatD [])
    Searchterm.Grammar.Gen.Abs.MayKnownTargetTailSome _ knowntargettail -> prPrec i 0 (concatD [prt 0 knowntargettail])
