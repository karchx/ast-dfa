{-# LANGUAGE OverloadedStrings #-}

module Parser.Parser 
    ( parseExpr
    , parseFile
    ) where

import AST.Syntax
import Parser.Lexer
import Text.Megaparsec
import Data.Text (Text)
import Data.Void (Void)
import qualified Data.Text.IO as TIO
import Control.Monad.Combinators.Expr

pTerm :: Parser Expr
pTerm = choice
    [ parens pExpr
    , ELit <$> number
    , EVar <$> identifier
    ]

pApp :: Parser Expr
pApp = do
    ts <- some pTerm
    return $ foldl1 EApp ts

operatorTable :: [[Operator Parser Expr]]
operatorTable =
    [ [ InfixR (EBinOp Pow <$ symbol "^") ]
    , [ InfixL (EBinOp Mul <$ symbol "*") ]
    , [ InfixL (EBinOp Div <$ symbol "/") ]
    , [ InfixL (EBinOp Add <$ symbol "+") ]
    , [ InfixL (EBinOp Sub <$ symbol "-") ]
    ]

pLam :: Parser Expr
pLam = do
    _ <- symbol "\\"
    param <- identifier
    _ <- symbol "->"
    body <- pExpr
    return $ ELam param body

pExpr :: Parser Expr
pExpr = pLam <|> makeExprParser pApp operatorTable

parseExpr :: Text -> Either (ParseErrorBundle Text Void) Expr
parseExpr = runParser (sc *> pExpr <* eof) ""

parseFile :: FilePath -> IO (Either (ParseErrorBundle Text Void) Expr)
parseFile path = parseExpr <$> TIO.readFile path
