module Print
    ( printStream
    ) where

import Data.List ( intercalate )

import Kanren
import Term

---
-- Print
---

printStream :: Stream -> String
printStream stream = "[" ++ intercalate ", " (map printSubst stream) ++ "]"

printSubst :: [(String, Maybe Term)] -> String
printSubst results = subst
    where subst = "{" ++ intercalate ", " pairs ++ "}"
          pairs = [printTerm i t | (i, t) <- results]
          printTerm i Nothing  = i ++ ": _"
          printTerm i (Just t) = i ++ ": " ++ pretty t
