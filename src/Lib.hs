module Lib
    (Expr(..)
    , Stmt(..)
    , BinOp(..)
    , Program
    , runProgram
    , compileExpr
    , compileStmt
    , Compiler
    , Bytecode
    ) where

import Syntax (Expr(..), BinOp(..), Stmt(..), Program)
import Eval (runProgram)
import Bytecode (compileExpr, compileStmt, Compiler, Bytecode)
