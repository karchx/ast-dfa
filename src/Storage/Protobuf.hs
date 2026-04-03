{-# LANGUAGE OverloadedStrings #-}

module Storage.Protobuf 
    ( toProtoExpr
    , fromProtoExpr
    , serializeExpr
    , deserializeExpr
    ) where

import AST.Syntax
import qualified Proto.Ast as P
import qualified Proto3.Suite as PB
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (toStrict)
import qualified Data.Text.Lazy as TL
import qualified Proto3.Suite as P

toProtoExpr :: Expr -> P.Expr
toProtoExpr (ELit d) = P.Expr $ Just $ P.ExprExprKindLit d
toProtoExpr (EVar v) = P.Expr $ Just $ P.ExprExprKindVar (TL.fromStrict v)
toProtoExpr (ELam p b) = P.Expr $ Just $ P.ExprExprKindLam $
    P.Lambda (TL.fromStrict p) (Just $ toProtoExpr b)
toProtoExpr (EApp f a) = P.Expr $ Just $ P.ExprExprKindApp $
    P.App (Just $ toProtoExpr f) (Just $ toProtoExpr a)
toProtoExpr (EBinOp op l r) = P.Expr $ Just $ P.ExprExprKindBinop $
    P.BinOpExpr (toProtoOp op) (Just $ toProtoExpr l) (Just $ toProtoExpr r)

toProtoOp :: BinOp -> PB.Enumerated P.BinOp
toProtoOp Add = PB.Enumerated (Right P.BinOpBIN_OP_ADD)
toProtoOp Sub = PB.Enumerated (Right P.BinOpBIN_OP_SUB)
toProtoOp Mul = PB.Enumerated (Right P.BinOpBIN_OP_MUL)
toProtoOp Div = PB.Enumerated (Right P.BinOpBIN_OP_DIV)
toProtoOp Pow = PB.Enumerated (Right P.BinOpBIN_OP_POW)

fromProtoExpr :: P.Expr -> Either String Expr
fromProtoExpr (P.Expr Nothing) = Left "Missing expression kind in protobuf"
fromProtoExpr (P.Expr (Just kind)) = case kind of
    P.ExprExprKindLit d -> Right $ ELit d
    P.ExprExprKindVar v -> Right $ EVar (TL.toStrict v)
    P.ExprExprKindLam (P.Lambda p (Just b)) -> 
        ELam (TL.toStrict p) <$> fromProtoExpr b
    P.ExprExprKindLam _ -> Left "Malformed Lambda in protobuf"
    P.ExprExprKindApp (P.App (Just f) (Just a)) -> 
        EApp <$> fromProtoExpr f <*> fromProtoExpr a
    P.ExprExprKindApp _ -> Left "Malformed App in protobuf"
    P.ExprExprKindBinop (P.BinOpExpr op (Just lhs) (Just rhs)) -> 
        EBinOp <$> fromProtoOp op <*> fromProtoExpr lhs <*> fromProtoExpr rhs
    P.ExprExprKindBinop _ -> Left "Malformed BinOp in protobuf"

fromProtoOp :: P.Enumerated P.BinOp -> Either String BinOp
fromProtoOp (PB.Enumerated (Right P.BinOpBIN_OP_ADD)) = Right Add
fromProtoOp (PB.Enumerated (Right P.BinOpBIN_OP_SUB)) = Right Sub
fromProtoOp (PB.Enumerated (Right P.BinOpBIN_OP_MUL)) = Right Mul
fromProtoOp (PB.Enumerated (Right P.BinOpBIN_OP_DIV)) = Right Div
fromProtoOp (PB.Enumerated (Right P.BinOpBIN_OP_POW)) = Right Pow
fromProtoOp (PB.Enumerated (Right P.BinOpBIN_OP_UNSPECIFIED)) = Left "Unspecified BinOp in protobuf"
fromProtoOp (PB.Enumerated (Left err)) = Left $ "Error parsing BinOp from protobuf: " ++ show err

serializeExpr  :: Expr -> ByteString
serializeExpr = toStrict . PB.toLazyByteString . toProtoExpr

deserializeExpr :: ByteString -> Either String Expr
deserializeExpr bs = case PB.fromByteString bs of
    Left err -> Left $ "Error deserializing protobuf: " ++ show err
    Right protoExpr -> fromProtoExpr protoExpr

