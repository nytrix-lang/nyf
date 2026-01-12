{-# LANGUAGE OverloadedStrings #-}

module Parser where

import Control.Monad (void)
import Data.Void (Void)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Char (isAlpha, isDigit)
import Data.Functor (($>))
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import AST

type Parser = Parsec Void Text

--- Whitespace and comments

sc :: Parser ()
sc = L.space space1 lineComment blockComment
  where
    lineComment  = L.skipLineComment ";"
    blockComment = L.skipBlockComment ";*" "*;"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser ()
symbol = void . L.symbol sc

parens, braces :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")
braces = between (symbol "{") (symbol "}")


chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl1 p op = p >>= rest
  where
    rest x = do
      f <- optional op
      case f of
        Nothing -> return x
        Just f' -> do
          y <- p
          rest (f' x y)

--- Keywords

keywords :: [Text]
keywords =
  [ "fn", "define", "def", "use", "if", "else", "while", "return"
  , "for", "in", "try", "catch", "defer", "goto", "lambda"
  , "break", "continue", "match", "otherwise", "const"
  ]

isKeyword :: Text -> Bool
isKeyword = (`elem` keywords)

identifier :: Parser Text
identifier = do
  x <- lexeme $ takeWhile1P Nothing (\c -> isAlpha c || isDigit c || c == '_')
  if isKeyword x
    then fail $ "keyword cannot be identifier: " ++ T.unpack x
    else return x

--- Literals

integer :: Parser Integer
integer = lexeme L.decimal

float :: Parser Double
float = lexeme L.float

stringLiteral :: Parser Text
stringLiteral = lexeme $ T.pack <$> (char '"' *> manyTill L.charLiteral (char '"'))

--- Top-level

file :: Parser [Decl]
file = sc *> many decl <* eof

decl :: Parser Decl
decl =
      try useDecl
  <|> try defineDecl
  <|> fnDecl

useDecl :: Parser Decl
useDecl = do
  symbol "use"
  parts <- some (identifier <* optional (symbol "."))
  let path = T.intercalate "." parts
  forNames <- optional $ do
    symbol ";"
    symbol "for"
    commaSep identifier
  case forNames of
    Nothing -> pure $ UseDecl path
    Just names -> pure $ UseAsDecl path names

defineDecl :: Parser Decl
defineDecl = do
  symbol "define"
  name <- identifier
  symbol "="
  val <- expr
  pure $ DefineDecl name val

fnDecl :: Parser Decl
fnDecl = do
  symbol "fn"
  name <- identifier
  params <- parens (commaSep identifier)
  bodyStmts <- braces (many stmt)
  let (doc, rest) = extractDocString bodyStmts
  pure $ FnDecl name params doc rest

extractDocString :: [Stmt] -> (Maybe DocString, [Stmt])
extractDocString (ExprStmt (StrLit s) : rest) = (Just s, rest)
extractDocString xs = (Nothing, xs)

stmt :: Parser Stmt
stmt =
      try assignStmt
  <|> try ifStmt
  <|> try whileStmt
  <|> try returnStmt
  <|> exprStmt

assignStmt :: Parser Stmt
assignStmt = do
  var <- identifier
  symbol "="
  e <- expr
  pure $ Assign var e

ifStmt :: Parser Stmt
ifStmt = do
  symbol "if"
  cond <- parens expr
  tBranch <- braces (many stmt)
  fBranch <- optional $ do
    symbol "else"
    braces (many stmt)
  pure $ IfStmt cond tBranch fBranch

whileStmt :: Parser Stmt
whileStmt = do
  symbol "while"
  cond <- parens expr
  body <- braces (many stmt)
  pure $ WhileStmt cond body

returnStmt :: Parser Stmt
returnStmt = do
  symbol "return"
  e <- optional expr
  pure $ ReturnStmt e

exprStmt :: Parser Stmt
exprStmt = ExprStmt <$> expr

expr :: Parser Expr
expr = opOr

opOr :: Parser Expr
opOr = chainl1 opAnd (symbol "||" $> BinOp Or)

opAnd :: Parser Expr
opAnd = chainl1 opEq (symbol "&&" $> BinOp And)

opEq :: Parser Expr
opEq = chainl1 opRel equalityOp
  where
    equalityOp = (symbol "==" $> BinOp Eq) <|> (symbol "!=" $> BinOp Neq)

opRel :: Parser Expr
opRel = chainl1 opAdd relOp
  where
    relOp =  (symbol "<=" $> BinOp Le)
         <|> (symbol ">=" $> BinOp Ge)
         <|> (symbol "<"  $> BinOp Lt)
         <|> (symbol ">"  $> BinOp Gt)

opAdd :: Parser Expr
opAdd = chainl1 opMul addOp
  where
    addOp = (symbol "+" $> BinOp Add) <|> (symbol "-" $> BinOp Sub)

opMul :: Parser Expr
opMul = chainl1 unary mulOp
  where
    mulOp = (symbol "*" $> BinOp Mul)
        <|> (symbol "/" $> BinOp Div)
        <|> (symbol "%" $> BinOp Mod)

unary :: Parser Expr
unary =
      (symbol "-" >> (UnOp Neg <$> unary))
  <|> (symbol "!" >> (UnOp Not <$> unary))
  <|> term

term :: Parser Expr
term =
      try (StrLit <$> stringLiteral)
  <|> try (FloatLit <$> float)
  <|> try (IntLit <$> integer)
  <|> parens expr
  <|> callOrVar

callOrVar :: Parser Expr
callOrVar = do
  name <- identifier
  args <- optional (parens (commaSep expr))
  case args of
    Nothing -> pure $ Var name
    Just as -> pure $ Call name as

commaSep :: Parser a -> Parser [a]
commaSep p = sepBy p (symbol ",")
