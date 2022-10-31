{-# LANGUAGE OverloadedStrings #-}

module Rulecheck.Interface.Parser where

import Text.Megaparsec (Parsec, ParseErrorBundle)
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as MPC
import qualified Text.Megaparsec.Char.Lexer as MPCL
import Data.Text (Text)
import Data.Void (Void)
import Control.Applicative (Alternative (..))
import Rulecheck.Interface.Types (Line (..), ModLine (..), DataLine (..), ConsLine (..), InstLine (..), FuncLine (..), ClsLine (..))
import Rulecheck.Interface.Core (TyName (..), TmName (..), TyVar (..), Ty (..), ClsName (..), ModName (..), Scheme (..), Cls (..), Inst (..))
import qualified Data.Sequence as Seq
import Data.Sequence (Seq (..))
import qualified Data.Text.IO as TIO
import Control.Exception (throwIO)
import qualified Data.Text as T
import Data.Char (isSpace)
import Control.Monad (void)

newtype P a = P { unP :: Parsec Void Text a }
  deriving newtype (Functor, Applicative, Monad, MonadFail)

instance Alternative P where
  empty = P empty
  P a <|> P b = P (MP.try a <|> MP.try b)

spaceP :: P ()
spaceP = P (MPCL.space MPC.hspace1 (MPCL.skipLineComment "--") empty)

lexP :: P a -> P a
lexP = P . MPCL.lexeme (unP spaceP) . unP

commaP :: P ()
commaP = lexP (P (void (MPC.char ',')))

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

upperP :: P Text
upperP = lexP $ do
  x <- P MPC.upperChar
  xs <- many (satisfy (not . isSpace))
  pure (T.pack (x:xs))

lowerP :: P Text
lowerP = lexP $ do
  x <- P MPC.lowerChar
  xs <- many (satisfy (not . isSpace))
  pure (T.pack (x:xs))

tyNameP :: P TyName
tyNameP = fmap TyName upperP

tmNameP :: P TmName
tmNameP = lexP $ lower <|> sym where
  lower = fmap TmName lowerP
  sym = do
    _ <- P (MPC.char '(')
    xs <- many (satisfy (\c -> not (isSpace c || c == ')')))
    _ <- P (MPC.char ')')
    pure (TmName (T.pack ("(" ++ xs ++ ")")))

tyVarP :: P TyVar
tyVarP = fmap TyVar lowerP

tyConP :: P (Ty TyVar)
tyConP = do
  cn <- tyNameP
  as <- many tyP
  pure (TyCon cn (Seq.fromList as))

innerTyP :: P (Ty TyVar)
innerTyP = optParensP $ fmap TyFree tyVarP <|> tyConP

tyP :: P (Ty TyVar)
tyP = optParensP $ do
  parts <- sepBy innerTyP (keywordP "->")
  case parts of
    [] -> empty
    ty:tys -> pure (assocFn ty tys)

assocFn :: Ty TyVar -> [Ty TyVar] -> Ty TyVar
assocFn ty tys =
  case tys of
    [] -> ty
    ty':tys' -> TyFun ty (assocFn ty' tys')

schemeP :: P (Scheme TyVar)
schemeP = do
  -- TODO use constraints
  _ <- constraintsP instP
  ty <- tyP
  -- TODO calculate type vars
  let tvs = Seq.empty
  pure (Scheme tvs ty)

clsNameP :: P ClsName
clsNameP = fmap ClsName upperP

modNameP :: P ModName
modNameP = fmap ModName upperP

constraintsP :: P a -> P (Seq a)
constraintsP p = lexP $ (single <|> multiple) <* keywordP "=>" where
  single = Seq.singleton <$> p
  multiple = do
    _ <- openParenP
    as <- sepBy p commaP
    _ <- closeParenP
    pure (Seq.fromList as)

clsP :: P Cls
clsP = do
  cn <- clsNameP
  as <- some tyVarP
  pure (Cls cn (Seq.fromList as))

instP :: P (Inst TyVar)
instP = do
  cn <- clsNameP
  as <- some tyP
  pure (Inst cn (Seq.fromList as))

lineP :: P Line
lineP = lexP $ foldr1 (<|>)
  [ LineMod <$> modLineP
  , LineData <$> dataLineP
  , LineCons <$> consLineP
  , LineFunc <$> funcLineP
  , LineCls <$> clsLineP
  , LineInst <$> instLineP
  ]

linesP :: P (Seq Line)
linesP = P (fmap Seq.fromList (MP.sepBy1 (unP lineP) (some MPC.newline)))

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
  parents <- constraintsP instP
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
  parents <- constraintsP instP
  self <- clsP
  pure (ClsLine self parents)

runP :: P a -> FilePath -> Text -> Either (ParseErrorBundle Text Void) a
runP p = MP.runParser (unP (spaceP *> p))

parseLines :: FilePath -> Text -> Either (ParseErrorBundle Text Void) (Seq Line)
parseLines = runP linesP

parseLinesIO :: FilePath -> IO (Seq Line)
parseLinesIO fp = do
  t <- TIO.readFile fp
  either throwIO pure (parseLines fp t)
