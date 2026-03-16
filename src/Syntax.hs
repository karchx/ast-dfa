{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}

module Syntax
    ( Name
    , Expr(..)
    , Val(..)
    , Env
    ) where

import Data.Text (Text)
import Data.Map.Strict (Map)

type Name = Text

data Expr
    = ELit Int
    | EVar Name
    | ELam Name Expr -- lambda function
    | EApp Expr Expr
    | EAdd Expr Expr
    deriving (Show, Eq)

data Val
    = VInt Int
    | VClosure Name Expr Env -- close lexical environment for lambda
    deriving (Show, Eq)

type Env = Map Name Val

