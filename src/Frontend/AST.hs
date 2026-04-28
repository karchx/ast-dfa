module Frontend.AST ( Expr(..) ) where

newtype Expr = Matrix [[Int]]
    deriving (Show, Eq)
