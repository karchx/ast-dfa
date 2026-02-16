module Eval (runProgram) where

import Syntax (Name, Expr(..), BinOp(..), Stmt(..), Program)
import qualified Data.Map as M
import Control.Monad.State
import Control.Monad.IO.Class ()
import Control.Monad (void)
import Data.Int (Int64)

type Env = M.Map Name Int64

type Runtime a = StateT Env IO a

evalExpr :: Expr -> Runtime Int64
evalExpr (ELit n) = return n
evalExpr (EVar name) = do
    env <- get
    case M.lookup name env of
        Just val -> return val
        Nothing -> error $ "Undefined variable: " ++ show name
evalExpr (EBinOp op lhs rhs) = do
    l <- evalExpr lhs
    r <- evalExpr rhs
    return $ applyOp op l r

applyOp :: BinOp -> Int64 -> Int64 -> Int64
applyOp Add x y = x + y
applyOp Sub x y = x - y
applyOp Mul x y = x * y
applyOp Div x y = x `div` y
applyOp Eq x y
    | x == y = 1
    | otherwise = 0
applyOp Lt x y
    | x < y = 1
    | otherwise = 0

execStmt :: Stmt -> Runtime ()

execStmt (SAssign name expr) = do
    val <- evalExpr expr
    modify $ M.insert name val

execStmt (SPrint expr) = do
    val <- evalExpr expr
    liftIO $ print val

execStmt (SBlock stmts) = mapM_ execStmt stmts

execStmt (SIf cond t f) = do
    val <- evalExpr cond
    if val /= 0 then execStmt t else execStmt f

runProgram :: Program -> IO ()
runProgram prog = void $ runStateT (mapM_ execStmt prog) M.empty

