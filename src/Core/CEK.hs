{-# LANGUAGE OverloadedStrings #-}

module Core.CEK 
    ( runCEK
    , step
    ) where

import AST.Syntax
import qualified Data.Map.Strict as Map

data Frame
    = KArg Expr Env
    | KApp Val
    | KBinOpL BinOp Expr Env
    | KBinOpR BinOp Val
    deriving (Show, Eq)

type Kont = [Frame]

data State
    = Eval Expr Env Kont
    | Apply Kont Val
    | Done Val
    | Error String
    deriving (Show, Eq)

-- Transition CEK machine by one step
step :: State -> State

-- Lits and vars
step (Eval (ELit n) _ k) = Apply k (VDouble n)
step (Eval (EVar x) env k) =
    case Map.lookup x env of
        Just v  -> Apply k v
        Nothing -> Error $ "Variable not defined: " ++ show x

-- lambda evaluation
step (Eval (ELam x body) env k) = Apply k (VClosure x body env)
step (Eval (EApp e1 e2) env k) = Eval e1 env (KArg e2 env : k)

step (Eval (EBinOp op e1 e2) env k) = Eval e1 env (KBinOpL op e2 env : k)

step (Apply [] v) = Done v

step (Apply (KArg e2 env : k) v1) = Eval e2 env (KApp v1 : k)
step (Apply (KApp (VClosure x body cEnv) : k) v2) =
    Eval body (Map.insert x v2 cEnv) k
step (Apply (KApp _ : _) _) = Error "Type error: attempt to apply non-function value"

step (Apply (KBinOpR op (VDouble n1) : k) (VDouble n2)) =
    case evalOp op n1 n2 of
        Right res -> Apply k (VDouble res)
        Left err -> Error err

step (Apply (KBinOpR _ _ : _) _) = Error "Type error: attempt to performance arithmetic on non-numeric"
step (Apply (KBinOpL op e2 env : k) v1) = Eval e2 env (KBinOpR op v1 : k)

step (Done v) = Done v
step (Error msg) = Error msg

evalOp :: BinOp -> Double -> Double -> Either String Double
evalOp Add x y = Right (x + y)
evalOp Sub x y = Right (x - y)
evalOp Mul x y = Right (x * y)
evalOp Div _ 0 = error "Division by zero"
evalOp Div x y = Right (x / y)
evalOp Pow x y = Right (x ** y)

runCEK :: Expr -> Either String Val
runCEK expr = loop (Eval expr Map.empty [])
    where
        loop state = case state of
            Done v -> Right v
            Error msg -> Left msg
            _ -> loop (step state)

