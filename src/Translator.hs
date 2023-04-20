module Translator 
    ( translateProgram
    , translateStatement
    ) where


import Control.Monad.IO.Class

import AST
import UTerm
import Kanren
import Print
import LambdaTerm

type Statement t = KanrenT t IO ()

---
-- Final Executable statements, built using Kanren primitives
---

rule :: String -> [String] -> Kanren.Goal LambdaTerm -> Translator.Statement LambdaTerm
rule name idents g = do defineRelation name idents g ; liftIO $ printRelation name idents

query :: Maybe Int -> [String] -> Kanren.Goal LambdaTerm -> Translator.Statement LambdaTerm
query Nothing  idents g = do stream <- runAll idents g    ; liftIO $ printStream stream -- Replace with run?
query (Just i) idents g = do stream <- runMany i idents g ; liftIO $ printStream stream


---
-- Translate AST to executable statements.
---

translateProgram :: [AST.Statement] -> [Translator.Statement LambdaTerm]
translateProgram = map translateStatement

translateStatement :: AST.Statement -> Translator.Statement LambdaTerm
translateStatement (Rule name idents goal) = rule name idents (translateGoal goal)
translateStatement (Query i idents goal)   = query i idents (translateGoal goal) 

translateGoal :: AST.Goal -> Kanren.Goal LambdaTerm
translateGoal (Disj g1 g2)         = disj (translateGoal g1) (translateGoal g2)
translateGoal (Conj g1 g2)         = conj (translateGoal g1) (translateGoal g2)
translateGoal (Fresh uvars g)      = fresh uvars (translateGoal g)
translateGoal (Equal t1 t2)        = translateTerm t1 === translateTerm t2
translateGoal (Relation name args) = callRelation name (map getArg args)
    where getArg (Param u) = uvar u
          getArg (Term t)  = translateTerm t

translateTerm :: AST.Term -> LambdaTerm
translateTerm (UVar u) = uvar u
translateTerm _ = error "Not implemented." 