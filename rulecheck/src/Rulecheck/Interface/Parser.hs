{-# LANGUAGE OverloadedStrings #-}

module Rulecheck.Interface.Parser where
  -- ( parseLines
  -- , parseLinesIO
  -- ) where

import Control.Applicative (Alternative (..))
import Control.Exception (throwIO)
import Control.Monad (void)
import Data.Char (isAlphaNum, isSpace)
import Data.Foldable (toList)
import Data.List (nub)
import Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Void (Void)
import Rulecheck.Interface.Core (Cls (..), ClsName (..), Forall (..), Inst (..), ModName (..), Rule (..), Scheme (..),
                                 StraintTy (..), Tm, TmName (..), TmVar (..), Ty (..), TyName (..), TyVar (..))
import Rulecheck.Interface.Types (ClsLine (..), ConsLine (..), DataLine (..), FuncLine (..), InstLine (..), Line (..),
                                  ModLine (..), RuleLine (..))
import Text.Megaparsec (ParseErrorBundle, Parsec)
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as MPC
import qualified Text.Megaparsec.Char.Lexer as MPCL

newtype P a = P { unP :: Parsec Void Text a }
  deriving newtype (Functor, Applicative, Monad, MonadFail)

instance Alternative P where
  empty = P empty
  P a <|> P b = P (MP.try a <|> MP.try b)

spaceP :: P ()
spaceP = P (MPCL.space MPC.hspace1 (MPCL.skipLineComment "--") empty)

moreSpaceP :: P ()
moreSpaceP = P (MPCL.space MPC.space1 (MPCL.skipLineComment "--") empty)

lexP :: P a -> P a
lexP = P . MPCL.lexeme (unP spaceP) . unP

moreLexP :: P a -> P a
moreLexP = P . MPCL.lexeme (unP moreSpaceP) . unP

commaP :: P ()
commaP = lexP (P (void (MPC.char ',')))

periodP :: P ()
periodP = lexP (P (void (MPC.char '.')))

openParenP :: P ()
openParenP = lexP (P (void (MPC.char '(')))

closeParenP :: P ()
closeParenP = lexP (P (void (MPC.char ')')))

keywordP :: Text -> P ()
keywordP = lexP . P . void . MPC.string

inParensP :: P a -> P a
inParensP p = do
  openParenP
  a <- p
  closeParenP
  pure a

optParensP :: P a -> P a
optParensP p = inParensP p <|> p

satisfy :: (Char -> Bool) -> P Char
satisfy f = P (MP.satisfy f)

sepBy :: P a -> P () -> P [a]
sepBy p x = P (MP.sepBy (unP p) (unP x))

isIdentChar :: Char -> Bool
isIdentChar c = isAlphaNum c || c == '_'

isSymChar :: Char -> Bool
isSymChar c = not (isSpace c || c == ')')

upperP :: P Text
upperP = lexP $ do
  x <- P MPC.upperChar
  xs <- many (satisfy isIdentChar)
  pure (T.pack (x:xs))

lowerP :: P Text
lowerP = lexP $ do
  x <- P MPC.lowerChar
  xs <- many (satisfy isIdentChar)
  pure (T.pack (x:xs))

identP :: P Text
identP = lexP $ do
  x <- P MPC.letterChar
  xs <- many (satisfy isIdentChar)
  pure (T.pack (x:xs))

tyNameP :: P TyName
tyNameP = fmap TyName upperP

tmNameP :: P TmName
tmNameP = lexP $ lower <|> sym where
  lower = fmap TmName identP
  sym = do
    -- Don't lex parens here because no space allowed
    _ <- P (MPC.char '(')
    xs <- many (satisfy isSymChar)
    _ <- P (MPC.char ')')
    pure (TmName (T.pack ("(" ++ xs ++ ")")))

tyVarP :: P TyVar
tyVarP = fmap TyVar lowerP

tmVarP :: P TmVar
tmVarP = fmap TmVar lowerP

tyConP :: P (Ty TyVar)
tyConP = do
  cn <- tyNameP
  as <- some (tyP True True)
  pure (TyCon cn (Seq.fromList as))

tyArrP :: P (Ty TyVar)
tyArrP = do
  parts <- sepBy (tyP False True) (keywordP "->")
  case parts of
    [] -> empty
    [_] -> empty
    ty:tys -> pure (assocFn ty tys)

innerTyP :: P (Ty TyVar)
innerTyP = fmap TyFree tyVarP <|> tyConP

singleTyP :: P (Ty TyVar)
singleTyP = fmap TyFree tyVarP <|> fmap (`TyCon` Empty) tyNameP

tyP :: Bool -> Bool -> P (Ty TyVar)
tyP conNeedParen arrNeedParen =
  (if arrNeedParen then inParensP tyArrP else tyArrP) <|>
  (if conNeedParen then inParensP tyConP else tyConP) <|>
  singleTyP

assocFn :: Ty TyVar -> [Ty TyVar] -> Ty TyVar
assocFn ty tys =
  case tys of
    [] -> ty
    ty':tys' -> TyFun ty (assocFn ty' tys')

straintTyP :: P (StraintTy TyVar)
straintTyP = do
  cons <- optP Empty (constraintsP instP)
  ty <- tyP False False
  pure (StraintTy cons ty)

forallP :: (Foldable f, Eq b) => P b -> P (f b) -> P (Forall b (f b))
forallP binderP bodyP = withForall <|> withoutForall where
  withForall = do
    keywordP "forall"
    bs <- some binderP
    periodP
    Forall (Seq.fromList bs) <$> bodyP
  withoutForall = do
    body <- bodyP
    let bs = Seq.fromList (nub (toList body))
    pure (Forall bs body)

schemeP :: P (Scheme TyVar)
schemeP = fmap Scheme (forallP tyVarP straintTyP)

clsNameP :: P ClsName
clsNameP = fmap ClsName upperP

modNameP :: P ModName
modNameP = fmap ModName upperP

optP :: a -> P a -> P a
optP a p = p <|> pure a

constraintsP :: P a -> P (Seq a)
constraintsP p = (single <|> multiple) <* keywordP "=>" where
  single = Seq.singleton <$> p
  multiple = do
    _ <- openParenP
    as <- sepBy p commaP
    _ <- closeParenP
    pure (Seq.fromList as)

clsP :: P (Cls TyVar)
clsP = do
  cn <- clsNameP
  as <- many tyVarP
  pure (Cls cn (Seq.fromList as))

instP :: P (Inst TyVar)
instP = do
  cn <- clsNameP
  as <- many (tyP True True)
  pure (Inst cn (Seq.fromList as))

lineP :: P Line
lineP = moreLexP $ foldr1 (<|>)
  [ LineMod <$> modLineP
  , LineData <$> dataLineP
  , LineCons <$> consLineP
  , LineFunc <$> funcLineP
  , LineCls <$> clsLineP
  , LineInst <$> instLineP
  , LineRule <$> ruleLineP
  ]

linesP :: P (Seq Line)
linesP = fmap Seq.fromList (many lineP)

modLineP :: P ModLine
modLineP = do
  keywordP "module"
  ModLine <$> modNameP

dataLineP :: P DataLine
dataLineP = do
  keywordP "data"
  tn <- tyNameP
  vs <- many tyVarP
  pure (DataLine tn (Seq.fromList vs))

consLineP :: P ConsLine
consLineP = do
  keywordP "constructors"
  tn <- tyNameP
  cns <- many tmNameP
  pure (ConsLine tn (Seq.fromList cns))

instLineP :: P InstLine
instLineP = do
  keywordP "instance"
  parents <- optP Empty (constraintsP instP)
  self <- instP
  pure (InstLine self parents)

funcLineP :: P FuncLine
funcLineP = do
  tn <- tmNameP
  keywordP "::"
  FuncLine tn <$> schemeP

clsLineP :: P ClsLine
clsLineP = do
  keywordP "class"
  parents <- optP Empty (constraintsP instP)
  self <- clsP
  pure (ClsLine self parents)

-- TODO parse terms so we can parse rules
tmP :: P (Tm TmVar TmVar)
tmP = error "TODO"

ruleLineP :: P RuleLine
ruleLineP = do
  keywordP "rule"
  _ <- P (void (MPC.char '"'))
  n <- some (satisfy (/= '"'))
  _ <- P (void (MPC.char '"'))
  keywordP "forall"
  vs <- some tmVarP
  periodP
  lhs <- tmP
  keywordP "="
  rhs <- tmP
  keywordP "::"
  RuleLine . Rule (T.pack n) (Seq.fromList vs) lhs rhs <$> schemeP

consumeP :: P a -> P a
consumeP p = do
  moreSpaceP
  a <- p
  moreSpaceP
  end <- P MP.atEnd
  if end then pure a else empty

type ParseErr = ParseErrorBundle Text Void

runP :: P a -> FilePath -> Text -> Either ParseErr a
runP p = MP.runParser (unP (consumeP p))

parseLines :: FilePath -> Text -> Either ParseErr (Seq Line)
parseLines = runP linesP

parseLinesIO :: FilePath -> IO (Seq Line)
parseLinesIO fp = do
  t <- TIO.readFile fp
  either throwIO pure (parseLines fp t)

parseScheme :: Text -> Either ParseErr (Scheme TyVar)
parseScheme = runP schemeP "<interactive>"

parseTerm :: Text -> Either ParseErr (Tm TmVar TmVar)
parseTerm = runP tmP "<interactive>"

-- | Can't figure out why the parser's not working? Use this to debug
parseDebug :: P a -> String -> Either (ParseErrorBundle Text Void) a
parseDebug p = MP.runParser (unP (consumeP p)) "<interactive>" . T.pack
