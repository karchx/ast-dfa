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

rowP :: Parser [Int]
rowP = between (symbol "[") (symbol "]") (int `sepBy` symbol ",")

matrixP :: Parser Expr
matrixP = Matrix <$> between (symbol "[") (symbol "]") (rowP `sepBy` symbol ",")

parserDSL :: Text -> Either (ParseErrorBundle Text Void) Expr
parserDSL = parse (sc *> matrixP <* eof) ""
