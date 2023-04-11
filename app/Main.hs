module Main (main) where

import Kanren

example :: Goal
example = callFresh 
            (\q ->
                (callFresh 
                    (\x -> 
                        (conj
                            (Pair (Symbol "s") (Var q) === Var x)
                            (Symbol "z" === Var q)
                        )
                    )
                ) 
            )

main :: IO ()
main = do
    let results = runAll example initialState
    print results

