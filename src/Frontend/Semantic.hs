{-# LANGUAGE OverloadedStrings #-}
module Frontend.Semantic ( checkNilpotency ) where

import Data.Text (Text)
import Frontend.AST
import Data.List (transpose)

mult :: [[Int]] -> [[Int]] -> [[Int]]
mult a b = [[ sum $ zipWith (*) ar bc | bc <- transpose b ] | ar <- a ]

isZeroMatrix :: [[Int]] -> Bool
isZeroMatrix = all (all (== 0))

isSquare :: [[Int]] -> Bool
isSquare m = all (\r -> length r == length m) m

checkNilpotency :: Expr -> Either Text Expr
checkNilpotency expr@(MatLiteral m)
    | null m || not (isSquare m) = Left "Semantic Error: The matrix is not square or is empty"
    | any isZeroMatrix powers = Right expr
    | otherwise = Left "Semantic Error: The matrix is not nilpotent."
    where
        n = length m
        powers = take n $ iterate (`mult` m) m

