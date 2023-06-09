import Test.HUnit

import UTerm

import Kanren
import KanrenTerm
import KanrenPrint

testAppend :: IO Test
testAppend = do
    result <- evalKanrenT kanrenProgram initialEnv
    mapM_ print $ printStreamList result
    return $ TestCase $ assertEqual
            "Should be equal"
            (printStream result)
            "[{T: (), Q: (t, (u, (v, (w, (x, ())))))}, {T: (t, ()), Q: (u, (v, (w, (x, ()))))}, {T: (t, (u, ())), Q: (v, (w, (x, ())))}, {T: (t, (u, (v, ()))), Q: (w, (x, ()))}, {T: (t, (u, (v, (w, ())))), Q: (x, ())}, {T: (t, (u, (v, (w, (x, ()))))), Q: ()}]"

    where kanrenProgram = do
            defineRelation "append" ["L", "S", "O"]
                (disj
                    (conj
                        (Nil === uvar "L")
                        (uvar "S" === uvar "O")
                    )
                    (fresh ["a", "d", "r"]
                        (conjPlus [
                            Pair (uvar "a") (uvar "d") === uvar "L",
                            Pair (uvar "a") (uvar "r") === uvar "O",
                            callRelation "append" [uvar "d", uvar "S", uvar "r"]
                            ]
                        )
                    )
                )

            runMany 6 ["T", "Q"]
                (fresh ["T", "Q"] $
                    callRelation "append" [
                        uvar "T",
                        uvar "Q",
                        Pair (Symbol "t") (Pair (Symbol "u") (Pair (Symbol "v") (Pair (Symbol "w") (Pair (Symbol "x") Nil))))
                    ]
                )

main :: IO ()
main = do
    putStrLn "\nRUNNING TESTS:"

    runTestTTAndExit =<< testAppend
