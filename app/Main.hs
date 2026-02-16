{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Lib

program =
    let cond = EBinOp Lt (EVar "x") (ELit 20)
        branchTrue = SPrint (EVar "x")
        branchFalse = SAssign "x" (ELit 0)
    in [SAssign "x" (ELit 10), SIf cond branchTrue branchFalse]


main :: IO ()
main = putStrLn "Hello, World!"
