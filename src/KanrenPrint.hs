module KanrenPrint (
    printStream,
    printStreamList,
    printRelation,
    putStrLn,
    putStr,
    MonadIO(..)
) where

import Data.List (intercalate)
import Control.Monad.IO.Class ( MonadIO(..) )

import Kanren (Stream)
import UTerm (UTerm (pretty))

---
-- Print
---

printRelation :: String -> [String] -> String
printRelation name idents = "Relation: " ++ name ++ unwords idents

printStream :: (UTerm t) => Stream t -> String
printStream stream = "[" ++ intercalate ", " (printStreamList stream) ++ "]"

printStreamList :: (UTerm t) => Stream t -> [String]
printStreamList = map printSubst

printSubst :: (UTerm t) => [(String, Maybe t)] -> String
printSubst results = subst
  where
    subst = "{" ++ intercalate ", " pairs ++ "}"
    pairs = [printTerm i t | (i, t) <- results]
    printTerm i Nothing = i ++ ": _"
    printTerm i (Just t) = i ++ ": " ++ pretty t
