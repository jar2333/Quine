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
-- addRelations :: KanrenState -> KanrenState
-- addRelations (State s b r c) = State s b append c
--     where append = defineRelation "append" ["L", "S", "O"] (
--                 (disj
--                     (conj
--                         (Nil === ID "L")
--                         (ID "S" === ID "O") 
--                     )
--                     (callFresh "a" 
--                         (callFresh "d"
--                             (conj
--                                 (Pair (ID "a") (ID "d") === ID "L")
--                                 (callFresh "r"
--                                     (conj
--                                         (Pair (ID "a") (ID "r") === ID "O")
--                                         (callRelation "append" [ID "d", ID "S", ID "r"])
--                                     )
--                                 )
--                             )
--                         )
--                     )
--                 )
--             ) r 

-- callExample :: Goal
-- callExample = callRelation "append" [
--                                      Pair (Symbol "t") (Pair (Symbol "u") (Pair (Symbol "v") Nil)), 
--                                      ID "Q",
--                                      Pair (Symbol "t") (Pair (Symbol "u") (Pair (Symbol "v") (Pair (Symbol "w") (Pair (Symbol "x") Nil))))
--                                         ]

main :: IO ()
main = do
    let results = runAll example (initialState)
    let reified = map (reifyPrint ["q"]) results
    print reified

