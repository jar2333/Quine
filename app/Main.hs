module Main (main) where

import Kanren

example :: Goal
example = callFresh 
            (\q ->
                (disj
                    (callFresh 
                        (\x -> 
                            (conj
                                (Pair (Symbol "s") (Var q) === Var x)
                                (Symbol "z" === Var q)
                            )
                        )
                    ) 
                    (Var q === Symbol "r")
                )
            )

main :: IO ()
main = do
    let results = runAll example initialState
    print results
    let reified = map (\s -> reify [0] (substitution s)) results
    print reified

