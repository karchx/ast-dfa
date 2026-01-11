module Main (main) where

import qualified Data.Map as M
import VM

prog =
    [ LOAD "x"
    , LOAD "y"
    , ADD
    , PUSH 2
    , MUL
    , HALT
    ]

env0 = M.fromList [("x", 3), ("y", 4)]

main :: IO ()
main = do
    let vm0 = initalVM prog env0
    print (run vm0)


