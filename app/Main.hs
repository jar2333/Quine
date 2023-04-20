module Main (main) where

-- import Kanren
-- import KanrenTerm
-- import Print ( printStream )
-- import Control.Monad.State

-- append :: KanrenT KanrenTerm IO ()
-- append = defineRelation "append" ["L", "S", "O"]
--                 (disj
--                     (conj
--                         (Nil === ID "L")
--                         (ID "S" === ID "O")
--                     )
--                     (fresh ["a", "d", "r"]
--                         (conjPlus [
--                             Pair (ID "a") (ID "d") === ID "L",
--                             Pair (ID "a") (ID "r") === ID "O",
--                             callRelation "append" [ID "d", ID "S", ID "r"]
--                             ]
--                         )
--                     )
--                 )


-- callExample :: Goal KanrenTerm
-- callExample = fresh ["T", "Q"] $
--     callRelation "append" [
--             ID "T",
--             ID "Q",
--             Pair (Symbol "t") (Pair (Symbol "u") (Pair (Symbol "v") (Pair (Symbol "w") (Pair (Symbol "x") Nil))))
--         ]


-- callExample2 :: Goal KanrenTerm
-- callExample2 = fresh ["Q"] $
--     callRelation "append" [
--             Pair (Symbol "t") (Pair (Symbol "u") (Pair (Symbol "v") Nil)),
--             ID "Q",
--             Pair (Symbol "t") (Pair (Symbol "u") (Pair (Symbol "v") (Pair (Symbol "w") (Pair (Symbol "x") Nil))))
--         ]


-- runner :: KanrenT KanrenTerm IO ()
-- runner = do
--     append

--     r <- runMany 6 ["T", "Q"] callExample
--     liftIO $ printStream r

--     r1 <- run ["Q"] callExample2
--     liftIO $ printStream r1


import Interpreter ( execute )

main :: IO ()
main = execute