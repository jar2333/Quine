module Translator (
    translateProgram,
    translateStatement,
) where

import AST as A
import Data.Maybe (fromJust)
import Kanren
import LambdaTerm as L
import KanrenPrint
import Semant

type Statement = KanrenT LambdaTerm IO ()

---
-- Final Executable statements, built using Kanren primitives
---

rule :: String -> [String] -> Kanren.Goal LambdaTerm -> Translator.Statement
rule name idents g = do defineRelation name idents g; liftIO $ putStrLn $ printRelation name idents

query :: Maybe Int -> [String] -> Kanren.Goal LambdaTerm -> Translator.Statement
query Nothing idents g = do stream <- runAll idents g; liftIO $ putStrLn $ printStream stream -- Replace with run?
query (Just i) idents g = do stream <- runMany i idents g; liftIO $ putStrLn $ printStream stream

---
-- Translate AST to executable statements.
---

translateProgram :: [A.Statement] -> [Translator.Statement]
translateProgram = map translateStatement

translateStatement :: A.Statement -> Translator.Statement
translateStatement (Rule name idents goal) = rule name idents (translateGoal goal)
translateStatement (Query i idents goal) = query i idents (translateGoal goal)

translateGoal :: A.Goal -> Kanren.Goal LambdaTerm
translateGoal (Disj g1 g2) = disj (translateGoal g1) (translateGoal g2)
translateGoal (Conj g1 g2) = conj (translateGoal g1) (translateGoal g2)
translateGoal (Fresh uvars g) = fresh uvars (translateGoal g)
translateGoal (Equal t1 t2) = translateTerm t1 === translateTerm t2
translateGoal (Relation name args) = callRelation name (map getArg args)
  where
    getArg (Param u) = uvar u
    getArg (Term t) = translateTerm t

translateTerm :: A.Term -> L.LambdaTerm
translateTerm term =
    case term of
        A.UVar u -> uvar u
        A.Var v -> L.Var v ty
        A.Abs b e -> L.Abs b (translateTerm e) ty
        A.App f x -> L.App (translateTerm f) (translateTerm x) ty
        A.Let v e1 e2 -> L.Let v (translateTerm e1) (translateTerm e2) ty
        A.Pair l r -> L.Pair (translateTerm l) (translateTerm r) ty
        A.Fst e -> L.Fst (translateTerm e) ty
        A.Snd e -> L.Snd (translateTerm e) ty
  where
    ty = fromJust $ typeof [] term
