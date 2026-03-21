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

-- Test that addition is commutative.
prop_add_comm :: WellFormedArith -> WellFormedArith -> Property
prop_add_comm (WellFormedArith e1) (WellFormedArith e2) =
    let res1 = runCEK (EAdd e1 e2)
        res2 = runCEK (EAdd e2 e1)
    in res1 === res2

prop_identity_app :: Int -> Property
prop_identity_app n =
    let expr = EApp (ELam "x" (EVar "x")) (ELit n)
    in runCEK expr === Right (VInt n)

prop_closure_capture :: Int -> Int -> Property
prop_closure_capture a b =
    let innerLam = ELam "y" (EAdd (EVar "x") (EVar "y"))
        outerLam = ELam "x" innerLam
        expr = EApp (EApp outerLam (ELit a)) (ELit b)
    in runCEK expr === Right (VInt (a + b))

main :: IO ()
main = do
    putStrLn "Verfied prop_literal_eval..."
    quickCheck prop_literal_eval
    putStrLn "Verfied prop_add_comm..."
    quickCheck prop_add_comm
    putStrLn "Verfied prop_identity_app..."
    quickCheck (prop_identity_app 3)
    putStrLn "Verfied prop_closure_capture..."
    quickCheck (prop_closure_capture 5 7)

