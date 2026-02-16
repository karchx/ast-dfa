{-# LANGUAGE OverloadedStrings #-}

module Syntax
    ( Name
    , Expr(..)
    , BinOp(..)
    , Stmt(..)
    , Program
    ) where

import Data.Text (Text)
import Data.Int (Int64)

type Name = Text

data Expr
    = ELit Int64
    | EVar Name
    | EBinOp BinOp Expr Expr
    deriving (Show, Eq)

data BinOp = Add | Sub | Mul | Div | Eq | Lt deriving (Show, Eq)

data Stmt
    = SAssign Name Expr
    | SBlock [Stmt]
    | SIf Expr Stmt Stmt
    | SWhile Expr Stmt
    | SPrint Expr
    deriving (Show, Eq)

type Program = [Stmt]

