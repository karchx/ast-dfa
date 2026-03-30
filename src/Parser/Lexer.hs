{-# LANGUAGE OverloadedStrings #-}

module Parser.Lexer 
    ( Parser
    , sc
    , lexeme
    , symbol
    , parens
    , number
    , identifier
    ) where

import Data.Void (Void)
import Data.Text (Text)
import Data.Char (isAlphaNum)
import qualified Data.Text as T
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

sc :: Parser ()
sc = L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

number :: Parser Double
number = lexeme (try L.float <|> (fromIntegral <$> L.decimal))

identifier :: Parser Text
identifier = lexeme $ do
    c <- letterChar
    cs <- takeWhileP Nothing isAlphaNum
    return $ T.cons c cs

