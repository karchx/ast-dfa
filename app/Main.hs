module Main (main) where

import VM

main :: IO ()
main = do
    let vm = initialState
    let newState = step vm (PUSH 42)
    let newStateNext = step newState (0)
    print vm
    print newState
    print newStateNext


