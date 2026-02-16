module Lib
    (Expr(..)
    , Stmt(..)
    , BinOp(..)
    , Program
    , runProgram
    ) where

import Syntax (Expr(..), BinOp(..), Stmt(..), Program)
import Eval (runProgram)
