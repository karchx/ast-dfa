{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}

module AST.Syntax
    ( Expr(..)
    , Val(..)
    , BinOp(..)
    , Env
    ) where

import Data.Text (Text)
import Data.Map.Strict (Map)

data BinOp = Add | Sub | Mul | Div | Pow
    deriving (Show, Eq)

data Expr
    = ELit Double
    | EVar Text
    | ELam Text Expr -- lambda function
    | EApp Expr Expr
    | EBinOp BinOp Expr Expr
    deriving (Show, Eq)

data Val
    = VDouble Double
    | VClosure Text Expr Env -- close lexical environment for lambda
    deriving (Show, Eq)

type Env = Map Text Val

