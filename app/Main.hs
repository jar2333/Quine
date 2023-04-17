module Main (main) where

import Kanren
import KanrenTerm
import Print ( printStream )
import Control.Monad.State

append :: State (Environment KanrenTerm) ()
append = defineRelation "append" ["L", "S", "O"]
                (disj
                    (conj
                        (Nil === ID "L")
                        (ID "S" === ID "O") 
                    )
                    (fresh ["a", "d", "r"] 
                        (conjPlus [
                            (Pair (ID "a") (ID "d") === ID "L"),
                            (Pair (ID "a") (ID "r") === ID "O"),
                            (callRelation "append" [ID "d", ID "S", ID "r"])
                            ]
                        )
                    )
                )
            

callExample :: Goal KanrenTerm
callExample = fresh ["T", "Q"] $ 
    callRelation "append" [ 
            ID "T", 
            ID "Q", 
            Pair (Symbol "t") (Pair (Symbol "u") (Pair (Symbol "v") (Pair (Symbol "w") (Pair (Symbol "x") Nil))))
        ]

runner :: State (Environment KanrenTerm) (Stream KanrenTerm)
runner = do
    append
    run 6 ["T", "Q"] callExample

main :: IO ()
main = do
    let results = evalState runner initialEnv
    print $ printStream results
