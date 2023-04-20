module Translator 
    ( translateProgram
    , translateStatement
    ) where

import AST
import UTerm
import Kanren
import Print

import Control.Monad.State

type Statement t = KanrenT t IO ()

---
-- Final Executable statements, built using Kanren primitives
---

rule :: (UTerm t) => String -> [String] -> Kanren.Goal t -> Translator.Statement t
rule name idents g = do defineRelation name idents g ; liftIO $ printRelation name idents

query :: (UTerm t) => Maybe Int -> [String] -> Kanren.Goal t -> Translator.Statement t
query Nothing  idents g = do stream <- runAll idents g    ; liftIO $ printStream stream -- Replace with run?
query (Just i) idents g = do stream <- runMany i idents g ; liftIO $ printStream stream


---
-- Translate AST to executable statements.
---

translateProgram :: (UTerm t) => [AST.Statement] -> [Translator.Statement t]
translateProgram = map translateStatement

translateStatement :: (UTerm t) => AST.Statement -> Translator.Statement t
translateStatement (Rule name idents goal) = rule name idents (translateGoal goal)
translateStatement (Query i idents goal)   = query i idents (translateGoal goal) 

translateGoal :: (UTerm t) => AST.Goal -> Kanren.Goal t
translateGoal (Disj g1 g2)         = disj (translateGoal g1) (translateGoal g2)
translateGoal (Conj g1 g2)         = conj (translateGoal g1) (translateGoal g2)
translateGoal (Fresh uvars g)      = fresh uvars (translateGoal g)
translateGoal (Equal t1 t2)        = translateTerm t1 === translateTerm t2
translateGoal (Relation name args) = callRelation name (map getArg args)
    where getArg (Param u) = uvar u
          getArg (Term t)  = translateTerm t

translateTerm :: (UTerm t) => AST.Term -> t
translateTerm (UVar u) = uvar u
translateTerm _ = error "Not implemented." 