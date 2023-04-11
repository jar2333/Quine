module Kanren
    ( (===)
    , callFresh
    , disj
    , conj
    , initialState
    , disjPlus
    , conjPlus
    , run
    , runAll
    , Term(..)
    , Goal
    , KanrenState
    ) where


import Data.Map as Map ( insert, lookup, Map, empty )
import Data.Maybe as Maybe ( fromMaybe )

import Control.Monad
import Control.Monad.Logic

type Var = Int

data Term = Var Var | Symbol String | Bool Bool | Nil | Pair Term Term deriving (Eq, Show)

type Subst = Map.Map Var Term

data KanrenState = State {substitution :: Subst, count :: Int} deriving (Show)

type Stream = Logic KanrenState

type Goal = KanrenState -> Stream

-- TO DO: SEPARATE THE COUNTER STATE FROM THE SUBSTITUTION PASSING!

---
-- First-order unification implementation
---

find :: Term -> Subst -> Term
find (Var u) s = Maybe.fromMaybe (Var u) (Map.lookup u s)
find t _ = t

occurs :: Var -> Term -> Subst -> Bool
occurs x (Var u) _    = x == u
occurs x (Pair a b) s = occurs x (find a s) s || occurs x (find b s) s
occurs _ _ _          = False

extendSubst :: Var -> Term -> Subst -> Maybe Subst
extendSubst x v s | occurs x v s = Nothing
                  | otherwise    = Just (Map.insert x v s)

unify :: Term -> Term -> Subst -> Maybe Subst
unify u v s | u == v              = Just s
unify (Var u) v s                 = extendSubst u v s
unify u var@(Var _) s             = unify var u s
unify (Pair ua ub) (Pair va vb) s = do
    s' <- unify (find ua s) (find va s) s
    unify (find ub s') (find vb s') s'
unify _ _ _                       = Nothing

---
-- Goal constructors
---

(===) :: Term -> Term -> Goal
(===) u v (State s c) = 
    case unify (find u s) (find v s) s of
        Just s' -> return $ State s' c
        Nothing -> mzero

callFresh :: (Var -> Goal) -> Goal
callFresh f (State s c) = f c $ State s (c+1)

disj :: Goal -> Goal -> Goal
disj g1 g2 state = g1 state `mplus` g2 state

conj :: Goal -> Goal -> Goal
conj g1 g2 state = mconcatMap g2 $ g1 state
    where mconcatMap f = msum . mapM f

---
-- Initial state
--- 

initialState :: KanrenState
initialState = State Map.empty 0

---
-- Extensions
---

disjPlus :: [Goal] -> Goal
disjPlus [g]    = g
disjPlus (g:gs) = disj g (disjPlus gs)
disjPlus [] = error "Not possible."

conjPlus :: [Goal] -> Goal
conjPlus [g]    = g
conjPlus (g:gs) = conj g (conjPlus gs)
conjPlus [] = error "Not possible."

---
-- Runner
--- 

run :: Int -> Goal -> KanrenState -> [KanrenState]
run i g s = observeMany i (g s)

runAll :: Goal -> KanrenState -> [KanrenState]
runAll g s = observeAll (g s)