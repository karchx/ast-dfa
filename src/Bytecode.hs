module Bytecode where

import Syntax (Name, BinOp(..), Stmt(..), Expr(..), Program)
import Data.Int (Int64)
import Control.Monad.State
import qualified Data.Text as T

type Label = String

data Inst
    = IPush Int64
    | IOp BinOp
    | ILoad Name
    | IStore Name
    | ILabel Label
    | IJmp Label
    | IJmpZero Label
    | IPrint
    deriving (Show, Eq)

type Bytecode = [Inst]

type Compiler a = State Int a

freshLabel :: Compiler Label
freshLabel = do
    n <- get
    put (n + 1)
    return $ "L" ++ show n

compileExpr :: Expr -> Compiler Bytecode
compileExpr (ELit n) =    return [IPush n]
compileExpr (EVar name) = return [ILoad name]
compileExpr (EBinOp op l r) = do
    cl <- compileExpr l
    cr <- compileExpr r
    return $ cl ++ cr ++ [IOp op] -- Order argument: Arg1, Arg2, Op

compileStmt :: Stmt -> Compiler Bytecode
compileStmt (SPrint expr) = do
    ce <- compileExpr expr
    return $ ce ++ [IPrint]

compileStmt (SAssign name expr) = do
    ce <- compileExpr expr
    return $ ce ++ [IStore name]

compileStmt (SBlock stmts) = do
    code <- mapM compileStmt stmts
    return $ concat code

compileStmt (SIf cond t f) = do
    -- +--------------+
    -- | cond         |
    -- +--------------+
    -- | jmpz L_FALSE |
    -- +--------------+
    -- | t            |
    -- +--------------+
    -- | jmp L_END    |
    -- +--------------+
    -- | L_FALSE:     |
    -- +--------------+
    -- | f            |
    -- +--------------+
    -- | L_END:       |
    -- +--------------+
    lFalse <- freshLabel
    lEnd <- freshLabel
    cCond <- compileExpr cond
    cTrue <- compileStmt t
    cFalse <- compileStmt f
    return $ cCond
            ++ [IJmpZero lFalse]
            ++ cTrue
            ++ [IJmp lEnd, ILabel lFalse]
            ++ cFalse
            ++ [ILabel lEnd]


compile :: Program -> Bytecode
compile prog = evalState (compileStmt (SBlock prog)) 0

