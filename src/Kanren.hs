module Kanren
    ( (===)
    , callFresh
    , disj
    , conj
    , callRelation
    , initialState
    , disjPlus
    , conjPlus
    , run
    , runAll
    , defineRelation
    , reify
    , reifyPrint
    , Term(..)
    , Goal
    , KanrenState(..)
    ) where


import Data.Map as Map ( insert, lookup, Map, empty )
import Data.Maybe as Maybe ( fromMaybe )

import Control.Monad
import Control.Monad.Logic

type Var = Int

data Term = ID String | Var Var | Symbol String | Bool Bool | Nil | Pair Term Term deriving (Eq, Show)

type Subst = Map.Map Var Term
type Bind  = Map.Map String Var
type Relations   = Map.Map String ([Term] -> Goal)

data KanrenState = State {
                            substitution :: Subst, 
                            bindings :: Bind,
                            relations :: Relations, 
                            count :: Int
                        } 

type Goal = KanrenState -> Logic KanrenState

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
-- consider case of ID?

---
-- Goal constructors
---

(===) :: Term -> Term -> Goal
(===) u v (State s b r c) =
    case unify (find u' s) (find v' s) s of
        Just s' -> return $ State s' b r c
        Nothing -> mzero
    where u' = substID u
          v' = substID v
          substID ident@(ID i) = maybe ident Var (Map.lookup i b)
          substID (Pair t1 t2) = Pair (substID t1) (substID t2)
          substID x = x

callFresh :: String -> Goal -> Goal
callFresh q g = \(State s b r c) -> g $ State s (Map.insert q c b) r (c+1)

disj :: Goal -> Goal -> Goal
disj g1 g2 = \state -> g1 state `interleave` g2 state

conj :: Goal -> Goal -> Goal
conj g1 g2 = \state -> sumMap g2 (g1 state)
    where sumMap f = fairsum . mapM f
          fairsum = foldr interleave mzero

callRelation :: String -> [Term] -> Goal
callRelation rel args = \s@(State _ _ r _) -> 
    case Map.lookup rel r of
        Just f  -> let g = f args in g s
        Nothing -> error "Relation not found!"

---
-- Initial state
--- 

initialState :: KanrenState
initialState = State Map.empty Map.empty Map.empty 0 

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
-- Statements: maintain a global state of defined relations
---

defineRelation :: String -> [String] -> Goal -> Relations -> Relations
defineRelation rel idents goal = Map.insert rel binded
    where binded args = foldr callFresh (padded args) idents
          padded args = foldr ($) goal (constraints args)
          constraints args = [conj (ID i === t) | (t, i) <- zip args idents]

run :: Int -> Goal -> KanrenState -> [KanrenState]
run i g s = observeMany i (g s)

runAll :: Goal -> KanrenState -> [KanrenState]
runAll g s = observeAll (g s)

---
-- Reifiers
---

reify :: [String] -> KanrenState -> [Maybe Term]
reify idents (State subst bind _ _) = map fromString idents
    where fromString :: String -> Maybe Term
          fromString i = get =<< Map.lookup i bind

          get :: Var -> Maybe Term
          get v = do t <- Map.lookup v subst
                     return $ replace t

          replace :: Term -> Term
          replace (Var v) = fromMaybe (Var (-1)) (get v)
          replace (Pair t1 t2) = Pair (replace t1) (replace t2)
          replace x = x

reifyPrint :: [String] -> KanrenState -> [String]
reifyPrint i s = map pretty $ reify i s
    where pretty Nothing = "_"
          pretty (Just t)  = show t