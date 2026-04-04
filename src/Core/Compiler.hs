module Core.Compiler
    ( compile
    ) where

import qualified AST.Syntax as S
import Core.Expr (Expr(..))
import Data.List (elemIndex)
import Data.Text (Text)

type CompileEnv = [Text]

compile :: CompileEnv -> S.Expr -> Either String Expr
compile _ (S.ELit n) = Right (Lit n)
compile env (S.EVar x) = case elemIndex x env of
    Just idx -> Right (Var idx)
    Nothing -> Left $ "Unbound variable: " ++ show x
compile env (S.ELam x body) = Lam <$> compile (x : env) body
compile env (S.EApp f arg) = App <$> compile env f <*> compile env arg
compile env (S.EBinOp op e1 e2) = BinOp op <$> compile env e1 <*> compile env e2
