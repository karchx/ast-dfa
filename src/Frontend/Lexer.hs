{-# LANGUAGE OverloadedStrings #-}
module Frontend.Lexer 
    ( Parser
    , sc
    , symbol
    , int
    , reservedWords
    , reserved
    , identifier) where

import Data.Void
import Data.Text (Text)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Data.Text as T
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

sc :: Parser ()
sc = L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

int :: Parser Int
int = lexeme L.decimal

reservedWords :: [Text]
reservedWords = [ "let", "in", "assert", "print" ]

identifier :: Parser Text
identifier = lexeme . try $ do
    first <- letterChar <|> char '_'
    rest <- many (alphaNumChar <|> char '_')
    let w = T.pack (first : rest)
    if w `elem` reservedWords
        then fail $ "reserved word: " <> T.unpack w
        else pure w

reserved :: Text -> Parser ()
reserved w = lexeme . try $ string w *> notFollowedBy (alphaNumChar <|> char '_')

