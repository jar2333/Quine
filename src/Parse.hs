{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- | Parser for the untyped lambda calculus, whose AST is defined in "AST".
module Parse (parse, tryParse) where

import qualified AST as A

import Text.Megaparsec (
  MonadParsec (..),
  Parsec,
  errorBundlePretty,
  noneOf,
  runParser,
  some,
  (<?>),
  (<|>),
  between
 )
import Text.Megaparsec.Char( space1 )
import qualified Text.Megaparsec.Char.Lexer as L

import Control.Monad (void)
import Data.Bifunctor (Bifunctor (..))
import Data.Void (Void)

-- * Exposed functions

-- | Parse a Lambda expression; throw an exception over an error
parse :: String -> A.Term
parse = either error id . tryParse

-- | Parse some code 'String' into an 'L.Expr' or an error message.
tryParse :: String -> Either String A.Term
tryParse = first errorBundlePretty .
           runParser (pSpace >> pExpr <* eof) "<input>"

{- * Expression parser

Note that Megaparsec allows us to label tokens with 'label' or '(<?>)', which
helps it produce human-readable error messages.
-}

-- | Entry point for parser.
pExpr :: Parser A.Term
pExpr = pBody <?> "expression"

-- | Parse expressions at the lowest level of precedence, i.e., lambdas.
pBody :: Parser A.Term
pBody = pAbs 
    <|> pApp 
    <|> pLet

-- | Parse lambda abstractions.
pAbs :: Parser A.Term
pAbs = do
  pToken "\\"
  bs <- some pIdent <?> "lambda binders"
  pToken "."
  body <- pBody <?> "lambda body"
  return $ A.Abs bs body

-- | Parse juxtaposition as application.
pApp :: Parser A.Term
pApp = foldl1 A.App <$> some pAtom <?> "term application"

pLet :: Parser A.Term
pLet =  do
  pToken "let"
  vs <- some pIdent <?> "let binders"
  pToken "="
  b <- pBody <?> "let binded term"
  pToken "in"
  body <- pBody <?> "let body"
  return $ A.Let (head vs) b body

-- | Parse expressions at the highest precedence, including parenthesized terms
pAtom :: Parser A.Term
pAtom = A.Var <$> pVar 
    <|> A.UVar <$> pVar 
    <|> pParens pExpr
 where
  pVar = pIdent <?> "variable"

-- * Megaparsec boilerplate and helpers

-- | Parsing monad.
type Parser = Parsec Void String

-- | Parse an identifier, possible surrounded by spaces
pIdent :: Parser String
pIdent = L.lexeme pSpace (some $ noneOf ['\\','.','(',')',' ','\n','\r','\t','-'])

-- | Consume a token defined by a string, possibly surrounded by spaces
pToken :: String -> Parser ()
pToken = void . L.symbol pSpace

-- | Parse some element surrounded by parentheses.
pParens :: Parser a -> Parser a
pParens = between (pToken "(") (pToken ")")

-- | Consumes whitespace and comments.
pSpace :: Parser ()
pSpace = label "whitespace" $ L.space
    space1
    (L.skipLineComment "--")
    (L.skipBlockCommentNested "{-" "-}")
