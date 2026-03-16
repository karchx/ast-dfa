{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Test.QuickCheck
import Syntax
import CEK

genArithExpr :: Int -> Gen Expr
genArithExpr 0 = ELit <$> arbitrary
genArithExpr n = frequency
    [ (3, ELit <$> arbitrary)
    , (1, EAdd <$> genArithExpr (n `div` 2) <*> genArithExpr (n `div` 2))
    ]

newtype WellFormedArith = WellFormedArith Expr deriving Show

instance Arbitrary WellFormedArith where
    arbitrary = sized (\n -> WellFormedArith <$> genArithExpr (min n 10))

prop_literal_eval :: Int -> Property
prop_literal_eval n = runCEK (ELit n) === Right (VInt n)

main :: IO ()
main = do
    putStrLn "Verfied prop_literal_eval..."
    quickCheck prop_literal_eval
