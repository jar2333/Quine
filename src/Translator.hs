module Translator 
    ( translateStatement
    ) where

import AST
import Kanren
import UTerm

type Statement t = Either (KanrenT t IO ()) (KanrenT t IO (Stream t))

translateStatement :: (UTerm t) => AST.Statement -> Translator.Statement t
translateStatement (Rule name idents goal)      = Left $ defineRelation name idents (translateGoal goal)
translateStatement (Query Nothing idents goal)  = Right $ runAll idents (translateGoal goal) -- Replace with run?
translateStatement (Query (Just i) idents goal) = Right $ runMany i idents (translateGoal goal)


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