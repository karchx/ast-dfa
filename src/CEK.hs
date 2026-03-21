module CEK (
    runCEK
    , step
) where

import Syntax
import qualified Data.Map.Strict as Map

data Frame
    = KArg Expr Env
    | KApp Val
    | KAddL Expr Env
    | KAddR Val
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
step (Eval (ELit n) _ k) = Apply k (VInt n)
step (Eval (EVar x) env k) =
    case Map.lookup x env of
        Just v  -> Apply k v
        Nothing -> Error $ "Variable not defined: " ++ show x

-- lambda evaluation
step (Eval (ELam x body) env k) = Apply k (VClosure x body env)

step (Eval (EApp e1 e2) env k) = Eval e1 env (KArg e2 env : k)

step (Eval (EAdd e1 e2) env k) = Eval e1 env (KAddL e2 env : k)

step (Apply [] v) = Done v

step (Apply (KArg e2 env : k) v1) = Eval e2 env (KApp v1 : k)
step (Apply (KApp (VClosure x body cEnv) : k) v2) =
    Eval body (Map.insert x v2 cEnv) k
step (Apply (KApp _ : _) _) = Error "Type error: attempt to apply non-function value"

step (Apply (KAddL e2 env : k) v1) = Eval e2 env (KAddR v1 : k)

step (Apply (KAddR (VInt n1) : k) (VInt n2)) = Apply k (VInt (n1 + n2))
step (Apply (KAddR _ : _) _) = Error "Type error: attempt to add non-integer values"

step (Done v) = Done v
step (Error msg) = Error msg

runCEK :: Expr -> Either String Val
runCEK expr = loop (Eval expr Map.empty [])
    where
        loop state = case state of
            Done v -> Right v
            Error msg -> Left msg
            _ -> loop (step state)

