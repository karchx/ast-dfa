{-# LANGUAGE OverloadedStrings #-}

module Frontend.Parser 
    ( rowP
    , matrixP
    , parserDSL
    ) where

import Data.Text (Text)
import Data.Void
import Text.Megaparsec
import Frontend.Lexer
import Frontend.AST

exprP :: Parser Expr
exprP = letExprP <|> atomP

letExprP :: Parser Expr
letExprP = Let
    <$> (reserved "let" *> identifier)
    <*> (symbol "="     *> exprP)
    <*> (reserved "in"  *> exprP)

-- binExprP :: Parser Expr
-- binExprP = makeExpr

rowP :: Parser [Int]
rowP = between (symbol "[") (symbol "]") (int `sepBy` symbol ",")

matrixP :: Parser [[Int]]
matrixP = between (symbol "[") (symbol "]") (rowP `sepBy` symbol ",")

atomP :: Parser Expr
atomP = MatLiteral
    <$> matrixP

parserDSL :: Text -> Either (ParseErrorBundle Text Void) Expr
parserDSL = parse (sc *> exprP <* eof) ""

