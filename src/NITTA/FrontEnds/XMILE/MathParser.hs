module MathParser (exprparser) where

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import Text.Parsec.Token
import Text.Parsec.Language

data Expr = Var String | Val Double | Duo Duop Expr Expr
  deriving Show

data Duop = Mul | Div | Add | Sub deriving Show


def = emptyDef{ identStart = char '\"'
              , identLetter = letter <|> space <|> char '\"'
              , opStart = oneOf "+-/*"
              , opLetter = oneOf "+-/*"
              , reservedOpNames = ["+", "-", "/", "*"]
              , reservedNames = ["true", "false"]
              }

TokenParser{ parens = m_parens
           , identifier = m_identifier
           , reservedOp = m_reservedOp
            } = makeTokenParser def

exprparser :: Parser Expr
exprparser = buildExpressionParser table term <?> "expression"
table = [ [Infix (m_reservedOp "*" >> return (Duo Mul)) AssocLeft]
        , [Infix (m_reservedOp "/" >> return (Duo Div)) AssocLeft]
        , [Infix (m_reservedOp "+" >> return (Duo Add)) AssocLeft]
        , [Infix (m_reservedOp "-" >> return (Duo Sub)) AssocLeft]
        ]
term = m_parens exprparser
       <|> fmap Var m_identifier