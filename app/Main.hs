module Main (main) where

import Kanren
import Control.Monad.State

example :: Goal
example = callFresh "Q"
                (disj
                    (callFresh "x" 
                            (conj
                                (Pair (Symbol "s") (ID "x") === ID "Q")
                                (Symbol "z" === ID "x")
                            )
                    ) 
                    (ID "Q" === Symbol "r")
                )

shadowingExample = callFresh "Q" (callFresh "a"
        (conj 
            (
                (ID "Q" === ID "a")
            )
            (callFresh "a"
                (ID "a" === Symbol "4")
            )
        )
    )  


append :: State Environment ()
append = defineRelation "append" ["L", "S", "O"] (
                (disj
                    (conj
                        (Nil === ID "L")
                        (ID "S" === ID "O") 
                    )
                    (callFresh "a" 
                        (callFresh "d"
                            (conj
                                (Pair (ID "a") (ID "d") === ID "L")
                                (callFresh "r"
                                    (conj
                                        (Pair (ID "a") (ID "r") === ID "O")
                                        (callFresh "_S"
                                            (conj
                                                (ID "S" === ID "_S")
                                                (callRelation "append" [ID "d", ID "_S", ID "r"])
                                            )
                                        )
                                    )
                                )
                            )
                        )
                    )
                )
            ) 

callExample :: Goal
callExample = callFresh "T" $ 
              callFresh "Q" $ 
              callRelation "append" [
                                    --  Pair (Symbol "t") (Pair (Symbol "u") (Pair (Symbol "v") Nil)), 
                                     ID "T",
                                     ID "Q",
                                     Pair (Symbol "t") (Pair (Symbol "u") (Pair (Symbol "v") (Pair (Symbol "w") (Pair (Symbol "x") Nil))))
                                        ]

runner :: State Environment [KanrenState]
runner = do
    append
    run 6 callExample

main :: IO ()
main = do
    let results = evalState runner initialEnv
    let reified = [printSubst $ reify ["T", "Q"] s | s <- results]
    print reified



