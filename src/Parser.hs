{-# LANGUAGE OverloadedStrings #-}

module Parser 
  ( parseExpr
  , parseProgram
  , Parser
  ) where

import SurfaceAST
import Control.Monad (void)
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Functor (($>))

type Parser = Parsec Void String

-- | Espacios en blanco y comentarios
sc :: Parser ()
sc = L.space
  space1
  (L.skipLineComment ";")
  empty

-- | Lexema: parsea y consume espacios
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

-- | Símbolo: parsea string y consume espacios
symbol :: String -> Parser String
symbol = L.symbol sc

-- | Entre paréntesis
parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

-- | Entre corchetes
brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")

-- | Palabras reservadas
reserved :: [String]
reserved = 
  [ "let", "let*", "letrec", "if", "if0", "lambda"
  , "cond", "else", "not", "fst", "snd", "head", "tail"
  , "add1", "sub1", "sqrt", "expt"
  ]

-- | Identificador
identifier :: Parser String
identifier = lexeme $ try $ do
  first <- letterChar <|> char '_'
  rest <- many (alphaNumChar <|> char '_' <|> char '-' <|> char '?' <|> char '!')
  return (first : rest)

-- | Número entero
integer :: Parser Integer
integer = lexeme $ L.signed sc L.decimal

-- | Booleano
boolean :: Parser Bool
boolean = lexeme $
  (string "#t" $> True) <|>
  (string "#f" $> False)

-- | Expresión
expr :: Parser SExpr
expr = choice
  [ listExpr
  , atomExpr
  , parens (try pairExpr' <|> compoundExpr)
  ]

-- | Par sin paréntesis externos (ya están consumidos)
pairExpr' :: Parser SExpr
pairExpr' = do
  e1 <- atomExpr <|> parens expr
  symbol ","
  e2 <- atomExpr <|> parens expr
  return $ SPair e1 e2

-- | Átomo (variable, número, booleano)
atomExpr :: Parser SExpr
atomExpr = choice
  [ SInt <$> integer
  , SBool <$> boolean
  , SVar <$> identifier
  ]

-- | Par ordenado: (e1, e2)
pairExpr :: Parser SExpr
pairExpr = parens $ do
  e1 <- expr
  symbol ","
  e2 <- expr
  return $ SPair e1 e2

-- | Lista: [e1, e2, ..., en] o []
listExpr :: Parser SExpr
listExpr = brackets $ do
  exprs <- expr `sepBy` symbol ","
  return $ SList exprs

-- | Expresiones compuestas
compoundExpr :: Parser SExpr
compoundExpr = choice
  [ try if0Expr
  , try ifExpr
  , try condExpr
  , try letExpr
  , try letStarExpr
  , try letRecExpr
  , try lambdaExpr
  , try notExpr
  , try add1Expr
  , try sub1Expr
  , try sqrtExpr
  , try exptExpr
  , try fstExpr
  , try sndExpr
  , try headExpr
  , try tailExpr
  , try opExpr
  , appExpr
  ]

-- | If: (if cond then else)
ifExpr :: Parser SExpr
ifExpr = do
  symbol "if"
  c <- expr
  t <- expr
  e <- expr
  return $ SIf c t e

-- | If0: (if0 e then else)
if0Expr :: Parser SExpr
if0Expr = do
  symbol "if0"
  e <- expr
  t <- expr
  f <- expr
  return $ SIf0 e t f

-- | Cond: (cond [g1 e1] ... [else ee])
condExpr :: Parser SExpr
condExpr = do
  symbol "cond"
  clauses <- many (try clause)
  elseClause <- elseClause
  return $ SCond clauses elseClause
  where
    clause = brackets $ try $ do
      guard <- expr
      -- Verificar que guard no sea la variable "else"
      case guard of
        SVar "else" -> fail "else clause"
        _ -> do
          body <- expr
          return (guard, body)
    elseClause = brackets $ do
      symbol "else"
      expr

-- | Let: (let ((x1 e1) ...) body)
letExpr :: Parser SExpr
letExpr = do
  symbol "let"
  bindings <- parens $ many (parens binding)
  body <- expr
  return $ SLet bindings body

-- | Let*: (let* ((x1 e1) ...) body)
letStarExpr :: Parser SExpr
letStarExpr = do
  symbol "let*"
  bindings <- parens $ many (parens binding)
  body <- expr
  return $ SLetStar bindings body

-- | LetRec: (letrec ((f1 e1) ...) body)
letRecExpr :: Parser SExpr
letRecExpr = do
  symbol "letrec"
  bindings <- parens $ many (parens binding)
  body <- expr
  return $ SLetRec bindings body

-- | Binding: (x e)
binding :: Parser Binding
binding = do
  x <- identifier
  e <- expr
  return (x, e)

-- | Lambda: (lambda (x1 ... xn) body)
lambdaExpr :: Parser SExpr
lambdaExpr = do
  symbol "lambda"
  params <- parens $ many identifier
  body <- expr
  return $ SLambda params body

-- | Not: (not e)
notExpr :: Parser SExpr
notExpr = do
  symbol "not"
  e <- expr
  return $ SNot e

-- | Add1: (add1 e)
add1Expr :: Parser SExpr
add1Expr = do
  symbol "add1"
  e <- expr
  return $ SAdd1 e

-- | Sub1: (sub1 e)
sub1Expr :: Parser SExpr
sub1Expr = do
  symbol "sub1"
  e <- expr
  return $ SSub1 e

-- | Sqrt: (sqrt e)
sqrtExpr :: Parser SExpr
sqrtExpr = do
  symbol "sqrt"
  e <- expr
  return $ SSqrt e

-- | Expt: (expt base exp)
exptExpr :: Parser SExpr
exptExpr = do
  symbol "expt"
  base <- expr
  exp <- expr
  return $ SExpt base exp

-- | Fst: (fst e)
fstExpr :: Parser SExpr
fstExpr = do
  symbol "fst"
  e <- expr
  return $ SFst e

-- | Snd: (snd e)
sndExpr :: Parser SExpr
sndExpr = do
  symbol "snd"
  e <- expr
  return $ SSnd e

-- | Head: (head e)
headExpr :: Parser SExpr
headExpr = do
  symbol "head"
  e <- expr
  return $ SHead e

-- | Tail: (tail e)
tailExpr :: Parser SExpr
tailExpr = do
  symbol "tail"
  e <- expr
  return $ STail e

-- | Operadores: (+/-/*/= e1 e2 ...)
opExpr :: Parser SExpr
opExpr = do
  op <- choice 
    [ symbol "+" $> SAdd
    , symbol "-" $> SSub
    , try (symbol "*") $> SMul
    , symbol "/" $> SDiv
    , try (symbol "=") $> SEq
    , try (symbol "<=") $> SLe
    , try (symbol ">=") $> SGe
    , try (symbol "!=") $> SNe
    , symbol "<" $> SLt
    , symbol ">" $> SGt
    ]
  exprs <- some expr
  return $ op exprs

-- | Aplicación: (f e1 ... en)
appExpr :: Parser SExpr
appExpr = do
  f <- expr
  args <- many expr
  return $ SApp f args

-- | Parsea expresión completa
parseExpr :: String -> Either String SExpr
parseExpr input = 
  case runParser (sc *> expr <* eof) "" input of
    Left err -> Left $ errorBundlePretty err
    Right result -> Right result

-- | Parsea programa (múltiples expresiones)
parseProgram :: String -> Either String [SExpr]
parseProgram input =
  case runParser (sc *> many expr <* eof) "" input of
    Left err -> Left $ errorBundlePretty err
    Right result -> Right result