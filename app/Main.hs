module Main (main) where

import Kanren
import Control.Monad.State

-- shadowingExample = callFresh "Q" (callFresh "a"
--         (conj 
--             (
--                 (ID "Q" === ID "a")
--             )
--             (callFresh "a"
--                 (ID "a" === Symbol "4")
--             )
--         )
--     )  


append :: State Environment ()
append = defineRelation "append" ["L", "S", "O"] (
                (disj
                    (conj
                        (Nil === ID "L")
                        (ID "S" === ID "O") 
                    )
                    (fresh ["a", "d"] 
                        (conj
                            (Pair (ID "a") (ID "d") === ID "L")
                            (fresh ["r"]
                                (conj
                                    (Pair (ID "a") (ID "r") === ID "O")
                                    (callRelation "append" [ID "d", ID "S", ID "r"])
                                )
                            )
                        )
                    )
                )
            ) 

callExample :: Goal
callExample = fresh ["T", "Q"] $ callRelation "append" [ ID "T", ID "Q",
                                     Pair (Symbol "t") (Pair (Symbol "u") (Pair (Symbol "v") (Pair (Symbol "w") (Pair (Symbol "x") Nil))))]

runner :: State Environment [KanrenState]
runner = do
    append
    run 6 callExample

main :: IO ()
main = do
    let results = evalState runner initialEnv
    let reified = printStream $ reifyAll ["T", "Q"] results
    print reified



