module Print
    ( printStream
    ) where

import Data.List ( intercalate )

import Kanren ( Stream )
import UTerm ( UTerm(pretty) )

---
-- Print
---

printStream ::(UTerm t) =>  Stream t -> IO ()
printStream stream = putStrLn $ "[" ++ intercalate ", " (map printSubst stream) ++ "]"

printSubst :: (UTerm t) => [(String, Maybe t)] -> String
printSubst results = subst
    where subst = "{" ++ intercalate ", " pairs ++ "}"
          pairs = [printTerm i t | (i, t) <- results]
          printTerm i Nothing  = i ++ ": _"
          printTerm i (Just t) = i ++ ": " ++ pretty t
