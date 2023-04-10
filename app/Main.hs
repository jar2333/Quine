module Main (main) where

import Lib
import Kanren

example :: Goal
example = disj 
            (callFresh 
                (\x -> 
                    Symbol "z" === Var x
                )
            ) 
            (callFresh 
                (\x -> 
                    Pair (Symbol "s") (Symbol "z") === Var x
                )
            ) 

main :: IO ()
main = do
    let results = runAll example initialState
    print results

