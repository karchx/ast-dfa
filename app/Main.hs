module Main (main) where

import Parser.Parser (parseFile)
import System.Environment (getArgs)
import Text.Megaparsec.Error (errorBundlePretty)

main :: IO ()
main = do
    args <- getArgs
    case args of
        (param:_) -> do
            result <- parseFile param
            case result of
                Left err -> putStrLn (errorBundlePretty err)
                Right ast -> putStrLn $ "" ++ show ast
        _ -> putStrLn "Use: stack run -- <file>"
