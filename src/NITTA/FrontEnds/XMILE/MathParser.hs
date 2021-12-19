module NITTA.FrontEnds.XMILE.MathParser (
    parseXmileEquation,
    XMExpr(..),
    XMDuop(..)
) where

import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Language
import Text.Parsec.String
import Text.Parsec.Token

data XMExpr = Var String | Val Double | Duo XMDuop XMExpr XMExpr
    deriving (Show)

data XMDuop = Mul | Div | Add | Sub deriving (Show)

def =
    emptyDef
        { identStart = letter <|> space
        , identLetter = letter <|> space
        , opStart = oneOf "+-/*"
        , opLetter = oneOf "+-/*"
        , reservedOpNames = ["+", "-", "/", "*"]
        , reservedNames = ["true", "false"]
        }

TokenParser
    { parens = m_parens
    , identifier = m_identifier
    , reservedOp = m_reservedOp
    } = makeTokenParser def


parseXmileEquation eqn = parse exprparser "" eqn 

exprparser :: Parser XMExpr
exprparser = buildExpressionParser table term <?> "expression"
table =
    [ [Infix (m_reservedOp "*" >> return (Duo Mul)) AssocLeft]
    , [Infix (m_reservedOp "/" >> return (Duo Div)) AssocLeft]
    , [Infix (m_reservedOp "+" >> return (Duo Add)) AssocLeft]
    , [Infix (m_reservedOp "-" >> return (Duo Sub)) AssocLeft]
    ]
term =
    m_parens exprparser <|> fmap Var (between (char '\"') (char '\"') m_identifier)
