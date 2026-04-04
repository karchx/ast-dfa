module Core.Expr 
    ( Expr(..)
    ) where

import AST.Syntax (BinOp)

data Expr
    = Lit Double
    | Var Int -- index 0-based
    | Lam Expr
    | App Expr Expr
    | BinOp BinOp Expr Expr
    deriving (Show, Eq)
