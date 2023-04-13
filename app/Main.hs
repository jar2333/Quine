module Main (main) where

import Kanren

example :: Goal
example = callFresh "q"
                (disj
                    (callFresh "x" 
                            (conj
                                (Pair (Symbol "s") (ID "q") === ID "x")
                                (Symbol "z" === ID "q")
                            )
                    ) 
                    (ID "q" === Symbol "r")
                )
            

main :: IO ()
main = do
    let results = runAll example initialState
    let reified = map (reifyPrint ["q", "x"]) results
    print reified

