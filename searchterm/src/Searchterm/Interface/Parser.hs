{-# LANGUAGE OverloadedStrings #-}

module Searchterm.Interface.Parser where
  -- ( parseTerm
  -- , parseType
  -- , parseLine
  -- , parseLines
  -- , parseLinesIO
  -- , ParseErr
  -- ) where

import Control.Applicative (Alternative (..))
import Control.Exception (throwIO)
import Control.Monad (join, void)
import Data.Char (isAlphaNum, isDigit, isSpace)
import Data.Foldable (toList)
import Data.List (nub)
import Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Void (Void)
import Searchterm.Interface.Core (Cls (..), ClsName (..), ClsScheme (..), ConPat (..), Forall (..),
                                  Inst (..), InstScheme (..), Lit (..), ModName (..), Pat (..), PatPair (..), Rule (..),
                                  Rw (..), RwScheme (..), Strained (..), Tm (..), TmName (..), TmVar (..), Ty (..),
                                  TyName (..), TyScheme (..), TyVar (..), strainedVars, KindAnno (..), Kind (..))
import Searchterm.Interface.Types (ClsLine (..), ConsLine (..), FuncLine (..), InstLine (..), Line (..), LitLine (..),
                                   ModLine (..), RuleLine (..), TypeLine (..))
import Text.Megaparsec (ParseErrorBundle, Parsec)
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as MPC
import qualified Text.Megaparsec.Char.Lexer as MPCL
import Data.Scientific (Scientific)
import Data.String (fromString)
import Text.Read (readMaybe)

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

keywordP :: Text -> P ()
keywordP = lexP . P . void . MPC.string

commaP :: P ()
commaP = keywordP ","

periodP :: P ()
periodP = keywordP "."

openParenP :: P ()
openParenP = keywordP "("

closeParenP :: P ()
closeParenP = keywordP ")"

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

-- Only satisfy the longest match of 1 or more (facepalm)
satisfyGreedy :: (Char -> Bool) -> P String
satisfyGreedy f = go [] where
  go !acc = do
    mx <- P (MP.optional (MP.satisfy f))
    case mx of
      Nothing -> if null acc then empty else pure (reverse acc)
      Just x -> go (x:acc)

sepBy :: P a -> P () -> P [a]
sepBy p x = P (MP.sepBy (unP p) (unP x))

isIdentChar :: Char -> Bool
isIdentChar c = isAlphaNum c || c == '_' || c == '\''

isSymChar :: Char -> Bool
isSymChar c = not (isSpace c || c == '(' || c == ')')

-- Raw `UpperIdent_string` (no post-lex)
rawUpperP :: P Text
rawUpperP = do
  x <- P MPC.upperChar
  xs <- many (satisfy isIdentChar)
  pure (T.pack (x:xs))

-- Raw `lowerIdent_string` (no post-lex)
rawLowerP :: P Text
rawLowerP = do
  x <- P MPC.lowerChar
  xs <- many (satisfy isIdentChar)
  pure (T.pack (x:xs))

-- Raw module prefix `UpperIdent.UpperIdent.` (no post-lex)
rawModPrefixP :: P Text
rawModPrefixP = do
  ms <- many (rawUpperP <* periodP)
  pure $ case ms of
    [] -> ""
    _ -> T.concat (join [[m, "."] | m <- ms])

-- Raw identifier (`Up...` or `low...`)
rawIdentP :: P Text
rawIdentP = do
  x <- P MPC.letterChar
  xs <- many (satisfy isIdentChar)
  pure (T.pack (x:xs))

withModPrefixP :: P Text -> P Text
withModPrefixP p = (<>) <$> rawModPrefixP <*> p

upperP :: P Text
upperP = lexP (withModPrefixP rawUpperP)

lowerP :: P Text
lowerP = lexP (withModPrefixP rawLowerP)

identP :: P Text
identP = lexP (withModPrefixP rawIdentP)

-- Symbol with module prefix - inside parens like "(Mod.++)"
symP :: P Text
symP = lexP $ do
  -- Don't lex parens here because no space allowed
  _ <- P (MPC.char '(')
  ms <- rawModPrefixP
  xs <- many (satisfy isSymChar)
  _ <- P (MPC.char ')')
  pure (T.pack ("(" ++ T.unpack ms ++ xs ++ ")"))

tyNameP :: P TyName
tyNameP = fmap TyName (upperP <|> symP)

tmUnitP :: P TmName
tmUnitP = "()" <$ keywordP "()"

-- TODO Make these 3 infix and uncomment in builtinTmNameP

-- tmListNilP :: P TmName
-- tmListNilP = error "TODO"

-- tmListConsP :: P TmName
-- tmListConsP = error "TODO"

-- tmTupP :: P TmName
-- tmTupP = error "TODO"

builtinTmNameP :: P TmName
builtinTmNameP = tmUnitP -- <|> tmListNilP <|> tmListConsP <|> tmTupP

plainTmNameP :: P TmName
plainTmNameP = fmap TmName (identP <|> symP)

tmNameP :: P TmName
tmNameP = builtinTmNameP <|> plainTmNameP

tyVarP :: P TyVar
tyVarP = fmap TyVar lowerP

tmVarP :: P TmVar
tmVarP = fmap TmVar lowerP

-- Parse a "plain" type constructor - no special logic for lists
plainTyAppP :: P (Ty TyVar)
plainTyAppP = do
  hd <- tyP True True  -- TODO are these paren rules right?
  as <- some (tyP True True)
  pure (TyApp hd (Seq.fromList as))

-- Parse an infix list constructor
listTyAppP :: P (Ty TyVar)
listTyAppP = do
  _ <- keywordP "["
  ty <- tyP False False
  _ <- keywordP "]"
  pure (TyApp (TyKnown "([])") (Seq.singleton ty))

-- Parse an infix tuple constructor (of arbitrary arity)
tupTyAppP :: P (Ty TyVar)
tupTyAppP = do
  _ <- openParenP
  ty1 <- tyP False False
  tys <- some $ do
    _ <- commaP
    tyP False False
  _ <- closeParenP
  let tn = fromString ("(" ++ replicate (length tys) ',' ++ ")")
  pure (TyApp (TyKnown tn) (Seq.fromList (ty1:tys)))

-- Parse the unit type
unitTyAppP :: P (Ty TyVar)
unitTyAppP = TyKnown "()" <$ keywordP "()"

builtinTyAppP :: P (Ty TyVar)
builtinTyAppP = unitTyAppP <|> listTyAppP <|> tupTyAppP

tyAppP :: P (Ty TyVar)
tyAppP = builtinTyAppP <|> plainTyAppP

tyArrP :: P (Ty TyVar)
tyArrP = do
  parts <- sepBy (tyP False True) (keywordP "->")
  case parts of
    [] -> empty
    [_] -> empty
    ty:tys -> pure (assocFn ty tys)

tyFreeP :: P (Ty TyVar)
tyFreeP = fmap TyFree tyVarP

tyKnownP :: P (Ty TyVar)
tyKnownP = fmap TyKnown tyNameP

singleTyP :: P (Ty TyVar)
singleTyP = tyFreeP <|> tyKnownP

tyP :: Bool -> Bool -> P (Ty TyVar)
tyP conNeedParen arrNeedParen =
  (if arrNeedParen then inParensP tyArrP else tyArrP) <|>
  (if conNeedParen then listTyAppP <|> tupTyAppP <|> inParensP tyAppP else tyAppP) <|>
  singleTyP

assocFn :: Ty TyVar -> [Ty TyVar] -> Ty TyVar
assocFn ty tys =
  case tys of
    [] -> ty
    ty':tys' -> TyFun ty (assocFn ty' tys')

strainedP :: P a -> P (Strained TyVar a)
strainedP bodyP = do
  cons <- optP Empty (constraintsP instP)
  Strained cons <$> bodyP

forallP :: (a -> [b]) -> P b -> P a -> P (Forall b a)
forallP xtract binderP bodyP = withForall <|> withoutForall where
  withForall = do
    keywordP "forall"
    bs <- some binderP
    periodP
    Forall (Seq.fromList bs) <$> bodyP
  withoutForall = do
    body <- bodyP
    let bs = Seq.fromList (xtract body)
    pure (Forall bs body)

kindP :: Bool -> P Kind
kindP arrNeedParen = (if arrNeedParen then inParensP con else con) <|> one where
  one = KindTy <$ keywordP "Type"
  con = fmap (KindTyCon . Seq.fromList) con'
  con' = do
    hd <- kindP True
    keywordP "->"
    tl <- con' <|> ([] <$ one)
    pure (hd : tl)

kindAnnoP :: P a -> P (KindAnno a)
kindAnnoP p = yesAnno <|> noAnno where
  yesAnno = inParensP $ do
    a <- p
    keywordP "::"
    KindAnno a . Just <$> kindP False
  noAnno = fmap (`KindAnno` Nothing) p

forallStrainedP :: Foldable f => P (f TyVar) -> P (Forall (KindAnno TyVar) (Strained TyVar (f TyVar)))
forallStrainedP = forallP (fmap (`KindAnno` Nothing) . nub . strainedVars) (kindAnnoP tyVarP) . strainedP

tySchemeP :: P (TyScheme TyVar)
tySchemeP = fmap TyScheme (forallStrainedP (tyP False False))

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

clsSchemeP :: P (ClsScheme TyVar)
clsSchemeP = do
  cons <- optP Empty (constraintsP instP)
  cn <- clsNameP
  as <- many (kindAnnoP tyVarP)
  let vs = fmap kaName as
  pure (ClsScheme (Forall (Seq.fromList as) (Strained cons (Cls cn (Seq.fromList vs)))))

instP :: P (Inst TyVar)
instP = do
  cn <- clsNameP
  as <- many (tyP True True)
  pure (Inst cn (Seq.fromList as))

instSchemeP :: P (InstScheme TyVar)
instSchemeP = fmap InstScheme (forallStrainedP instP)

lineP :: P Line
lineP = moreLexP $ foldr1 (<|>)
  [ LineMod <$> modLineP
  , LineType <$> typeLineP
  , LineCons <$> consLineP
  , LineFunc <$> funcLineP
  , LineCls <$> clsLineP
  , LineInst <$> instLineP
  , LineRule <$> ruleLineP
  , LineLit <$> litLineP
  ]

linesP :: P (Seq Line)
linesP = fmap Seq.fromList (many lineP)

modLineP :: P ModLine
modLineP = do
  keywordP "module"
  ModLine <$> modNameP

typeLineP :: P TypeLine
typeLineP = do
  keywordP "type"
  tn <- tyNameP
  vs <- many tyVarP
  pure (TypeLine tn (Seq.fromList vs))

consLineP :: P ConsLine
consLineP = do
  keywordP "constructors"
  tn <- tyNameP
  cns <- many tmNameP
  pure (ConsLine tn (Seq.fromList cns))

instLineP :: P InstLine
instLineP = do
  keywordP "instance"
  InstLine <$> instSchemeP

funcLineP :: P FuncLine
funcLineP = do
  tn <- tmNameP
  keywordP "::"
  FuncLine tn <$> tySchemeP

clsLineP :: P ClsLine
clsLineP = do
  keywordP "class"
  ClsLine <$> clsSchemeP

tmP, tmLamP, tmAppP, tmFreeP, tmLetP, tmCaseP, tmLitP :: P (Tm TmVar TmVar)
tmP = foldr1 (<|>)
  [ tmLamP
  , tmAppP
  , tmFreeP
  , tmLetP
  , tmCaseP
  , tmLitP
  ]
tmLamP = optParensP $ do
  _ <- keywordP "\\"
  x <- tmVarP
  keywordP "->"
  TmLam x <$> tmP
-- Need parens here because of recursive descent parsing!
tmAppP = inParensP $ do
  one <- tmP
  two <- tmP
  rest <- many tmP
  pure (mkApp one two rest)
-- We can only parse vars/funs as free until we resolve them later
tmFreeP = fmap (TmFree . TmVar . unTmName) tmNameP
tmLetP = optParensP $ do
  _ <- keywordP "let"
  b <- tmVarP
  _ <- keywordP "="
  arg <- tmP
  _ <- keywordP "in"
  TmLet b arg <$> tmP
tmCaseP = optParensP $ do
  _ <- keywordP "case"
  scrut <- tmP
  _ <- keywordP "of"
  _ <- keywordP "{"
  pairs <- sepBy patPairP (keywordP ";")
  _ <- keywordP "}"
  pure (TmCase scrut (Seq.fromList pairs))
tmLitP = TmLit <$> litP

patPairP :: P (PatPair TmVar (Tm TmVar TmVar))
patPairP = do
  cn <- tmNameP
  bs <- many tmVarP
  _ <- keywordP "->"
  PatPair (Pat (ConPat cn (Seq.fromList bs))) <$> tmP

mkApp :: Tm TmVar TmVar -> Tm TmVar TmVar -> [Tm TmVar TmVar]-> Tm TmVar TmVar
mkApp one two rest =
  let app = TmApp one two
  in case rest of
    [] -> app
    three:more -> mkApp app three more

rwP :: P (Rw TmVar)
rwP = do
  lhs <- tmP
  keywordP "="
  Rw lhs <$> tmP

rwSchemeP :: P (RwScheme TmVar)
rwSchemeP = fmap RwScheme (forallP (nub . toList) tmVarP rwP)

ruleLineP :: P RuleLine
ruleLineP = do
  keywordP "rule"
  _ <- P (void (MPC.char '"'))
  n <- some (satisfy (/= '"'))
  _ <- P (void (MPC.char '"'))
  rw <- rwSchemeP
  keywordP "::"
  RuleLine . Rule (T.pack n) rw <$> tySchemeP

isIntChar :: Char -> Bool
isIntChar c = isDigit c || c == '-'

intP :: P Integer
intP = lexP $ do
  n <- some (satisfy isIntChar) <* P (MP.notFollowedBy (MP.satisfy isFloatChar))
  maybe empty pure (readMaybe n)

charP :: P Char
charP = lexP $ do
  _ <- P (void (MPC.char '\''))
  c <- satisfy (/= '\'')
  _ <- P (void (MPC.char '\''))
  pure c

strP :: P Text
strP = lexP $ do
  _ <- P (void (MPC.char '"'))
  n <- many (satisfy (/= '"'))
  _ <- P (void (MPC.char '"'))
  pure (T.pack n)

isFloatChar :: Char -> Bool
isFloatChar c = isDigit c || c == '.' || c == '-'

sciP :: P Scientific
sciP = lexP $ do
  n <- some (satisfy isFloatChar) <* P (MP.notFollowedBy (MP.satisfy isFloatChar))
  maybe empty pure (readMaybe n)

litP :: P Lit
litP = foldr1 (<|>)
  [ LitChar <$> charP
  , LitString <$> strP
  , LitInteger <$> intP
  , LitScientific <$> sciP
  ]

litLineP :: P LitLine
litLineP = do
  keywordP "literals"
  tyn <- tyNameP
  vals <- many litP
  pure (LitLine tyn (Seq.fromList vals))

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

parseLine :: FilePath -> Text -> Either ParseErr Line
parseLine = runP lineP

parseLines :: FilePath -> Text -> Either ParseErr (Seq Line)
parseLines = runP linesP

parseLinesIO :: FilePath -> IO (Seq Line)
parseLinesIO fp = do
  t <- TIO.readFile fp
  either throwIO pure (parseLines fp t)

parseType :: Text -> Either ParseErr (TyScheme TyVar)
parseType = runP tySchemeP "<interactive>"

parseTerm :: Text -> Either ParseErr (Tm TmVar TmVar)
parseTerm = runP tmP "<interactive>"

-- | Can't figure out why the parser's not working? Use this to debug
parseDebug :: P a -> String -> Either (ParseErrorBundle Text Void) a
parseDebug p = MP.runParser (unP (consumeP p)) "<interactive>" . T.pack
