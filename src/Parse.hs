{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

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


---
-- Exposed functions
---

-- | Parse a Lambda expression; throw an exception over an error
parse :: String -> A.Statement
parse = either error id . tryParse

-- | Parse some code 'String' into an 'L.Expr' or an error message.
tryParse :: String -> Either String A.Statement
tryParse = first errorBundlePretty .
           runParser (pSpace >> pStatement <* eof) "<input>"


---
-- Entry point for parser.
---

-- | Parse statements
pStatement :: Parser A.Statement
pStatement  = pStatementBody <?> "program"

pStatementBody :: Parser A.Statement
pStatementBody = pRule
             <|> pQuery
             <|> pParens pStatement

pRule :: Parser A.Statement
pRule = do
  ident <- pIdent <?> "rule id"
  uvars <- pBrackets $ some pIdent
  pToken "="
  goal  <- pGoal <?> "goal"
  return $ A.Rule ident uvars goal

pQuery :: Parser A.Statement
pQuery = do
  nums  <- some pNum
  uvars <- pBrackets $ some pIdent
  goal  <- pGoal <?> "goal"
  let num = case nums of n:_ -> Just n
                         _   -> Nothing
  return $ A.Query num uvars goal


-- | Parse goals
pGoal :: Parser A.Goal
pGoal = pParens pGoalBody

pGoalBody :: Parser A.Goal
pGoalBody = pDisj
        <|> pConj
        <|> pFresh
        <|> pEqual
        <|> pRelation
        <|> pParens pGoalBody

pDisj :: Parser A.Goal
pDisj = do
  g1 <- pGoal <?> "goal"
  pToken "||"
  g2 <- pGoal <?> "goal"
  return $ A.Disj g1 g2

pConj :: Parser A.Goal
pConj = do
  g1 <- pGoal <?> "goal"
  pToken "&&"
  g2 <- pGoal <?> "goal"
  return $ A.Conj g1 g2

pFresh :: Parser A.Goal
pFresh = do
  uvars <- pBrackets $ some pIdent
  goal  <- pGoal <?> "goal"
  return $ A.Fresh uvars goal

pEqual :: Parser A.Goal
pEqual = do
  t1 <- pAtom <?> "term"
  pToken "=="
  t2 <- pAtom <?> "term"
  return $ A.Equal t1 t2

pRelation :: Parser A.Goal
pRelation = do
  ident <- pIdent
  args <- pBrackets $ some pArg
  return $ A.Relation ident args


-- | Parse args
pArg :: Parser A.Arg
pArg = pArgTerm
   <|> pParam

pArgTerm :: Parser A.Arg
pArgTerm = do
  A.Term <$> pTerm

pParam :: Parser A.Arg
pParam = do
  A.Param <$> pIdent


-- | Parse lambda terms
pTerm :: Parser A.Term
pTerm = pBody <?> "expression"

-- | Parse expressions at the lowest level of precedence, i.e., lambdas.
pBody :: Parser A.Term
pBody = pAbs
    <|> pApp
    <|> pLet
    <|> pAtom

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
  vs <- pIdent <?> "let binders"
  pToken "="
  b <- pBody <?> "let binded term"
  pToken "in"
  body <- pBody <?> "let body"
  return $ A.Let vs b body

-- | Parse expressions at the highest precedence, including parenthesized terms
pAtom :: Parser A.Term
pAtom = A.Var <$> pVar
    <|> A.UVar <$> pVar
    <|> pParens pTerm
 where
  pVar = pIdent <?> "variable"


---
-- Megaparsec boilerplate and helpers
---

-- | Parsing monad.
type Parser = Parsec Void String

-- | Parse an identifier, possibly surrounded by spaces
pIdent :: Parser String
pIdent = L.lexeme pSpace (some $ noneOf ['\\','.','(',')',' ','\n','\r','\t','-','[',']'])

-- | Parse an integer, possibly surrounded by spaces
pNum :: Parser Int
pNum = L.lexeme pSpace L.decimal

-- | Consume a token defined by a string, possibly surrounded by spaces
pToken :: String -> Parser ()
pToken = void . L.symbol pSpace

-- | Parse some element surrounded by parentheses.
pParens :: Parser a -> Parser a
pParens = between (pToken "(") (pToken ")")

-- | Parse some element surrounded by brackets.
pBrackets :: Parser a -> Parser a
pBrackets = between (pToken "[") (pToken "]")

-- | Consumes whitespace and comments.
pSpace :: Parser ()
pSpace = label "whitespace" $ L.space
    space1
    (L.skipLineComment "--")
    (L.skipBlockCommentNested "{-" "-}")
