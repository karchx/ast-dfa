module Frontend.AST 
    ( Program
    , Expr(..) 
    ) where

import Data.Text (Text)

type Name = Text

data MatBinOp
    = MatMul -- A * B
    | MatAdd -- A + B
    | MatSub -- A - B
    deriving (Show, Eq)

data UOp
    = Transpose -- Aᵀ
    | NegMat    -- -A
    deriving (Show, Eq)

data Expr
    = MatLiteral    [[Int]]
    | Var           Name
    | MatBinOp      MatBinOp Expr Expr
    | UOp           UOp Expr
    | Power         Expr Int            -- A^k (key for nilpotency)
    | Let           Name Expr Expr
    | NilIdx        Expr                -- nilpotency index: k tq Aᵏ=0
    | Commutador    Expr Expr           -- [A, B] = AB - BA
    | Jordan        Expr                -- Jordan canonical
    deriving (Show, Eq)

data Stmt
    = Assign Name Expr
    | Assert Expr
    | Print Expr
    deriving (Show, Eq)

type Program = [Stmt]
