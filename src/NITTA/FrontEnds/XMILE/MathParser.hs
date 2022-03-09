{- |
Module      : NITTA.FrontEnds.XMILE.MathParser
Description : Parses XMILE math equations
Copyright   : (c) Aleksandr Penskoi, 2021
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.FrontEnds.XMILE.MathParser (
    parseXmileEquation,
    calculateDefaultValue,
    XMExpr (..),
    XMDuop (..),
) where

import qualified Data.HashMap.Strict as HM
import Data.Maybe (fromMaybe)
import Data.Text as T
import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Language
import Text.Parsec.String
import Text.Parsec.Token
import Text.Read hiding (parens)

data XMExpr = Var String | Val Double | Duo XMDuop XMExpr XMExpr
    deriving (Show, Eq)

data XMDuop = Mul | Div | Add | Sub deriving (Show, Eq)

def =
    emptyDef
        { identStart = letter <|> space <|> digit <|> char '\"'
        , identLetter = letter <|> space <|> digit <|> char '.'
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

parseXmileEquation eqn = case parse exprparser "" eqn of
    Right e -> prepareTree e
    Left _ -> error $ "error while parsing XMILE equation : " <> eqn

exprparser :: Parser XMExpr
exprparser = buildExpressionParser table term <?> "expression"
table =
    [ [Infix (m_reservedOp "*" >> return (Duo Mul)) AssocLeft]
    , [Infix (m_reservedOp "/" >> return (Duo Div)) AssocLeft]
    , [Infix (m_reservedOp "+" >> return (Duo Add)) AssocLeft]
    , [Infix (m_reservedOp "-" >> return (Duo Sub)) AssocLeft]
    ]
term =
    m_parens exprparser <|> fmap Var (between (skipMany (char '\"')) (skipMany (char '\"' <|> space)) m_identifier)

trimString str = T.unpack $ T.map repl $ T.strip $ T.pack str
    where
        repl ' ' = '_'
        repl c = c

prepareTree (Var str) = case (readMaybe str :: Maybe Double) of
    (Just val) -> Val val
    _ -> Var (trimString str)
prepareTree v@(Val _) = v
prepareTree (Duo op a b) = Duo op (prepareTree a) (prepareTree b)

calculateDefaultValue _ (Val value) = value
calculateDefaultValue defaultValues (Var name) = fromMaybe 0 $ HM.lookup (T.pack name) defaultValues
calculateDefaultValue defaultValues (Duo op expl expr) =
    let leftValue = calculateDefaultValue defaultValues expl
        rightValue = calculateDefaultValue defaultValues expr
     in case op of
            Mul -> leftValue * rightValue
            Div -> leftValue / rightValue
            Add -> leftValue + rightValue
            Sub -> leftValue - rightValue
