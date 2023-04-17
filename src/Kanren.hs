module Kanren
    ( (===)
    , callFresh
    , disj
    , conj
    , callRelation
    , disjPlus
    , conjPlus
    , fresh
    , initialEnv
    , defineRelation
    , run
    , runAll
    , Term(..)
    , Goal
    , KanrenState
    , Environment(..)
    , Stream
    ) where

import Data.Map as Map ( insert, lookup, empty, Map )

import Control.Monad ( MonadPlus(mzero) )
import Control.Monad.State ( MonadState(get), State, modify )
import Control.Monad.Logic ( observeAll, observeMany, MonadLogic(interleave), Logic )

import UTerm
import Term

---
-- State and Goal types
---

data KanrenState = State {
                            substitution :: Subst,
                            bindings :: Bind,
                            count :: Int
                        } deriving (Show)

type Goal = KanrenState -> State Environment (Logic KanrenState)

---
-- Environment: define and call relations
---

type Relation = [Term] -> Goal

newtype Environment = Env (Map.Map String Relation)

---
-- Goal constructors
---

-- Unification constraints
(===) :: Term -> Term -> Goal
(===) u v (State s b c) = return $ do
    s' <- unify (find u' s) (find v' s) s 
    return (State s' b c)
    where u' = substID u b
          v' = substID v b

-- For the subtree, make it so every instance of q is replaced with cnt
-- Then for each state in the result stream, restore the original binding.
-- This makes it so that the states in the resulting stream only have the 
-- topmost binding for any shadowed variables.
callFresh :: String -> Goal -> Goal
callFresh q g (State subt bind cnt) = do
    let updated = Map.insert q cnt bind
    stream <- g $ State subt updated (cnt+1)

    let restored = stream >>= \(State s b c) -> return $ State s (restore b) c
    return restored
    
    where restore b = case Map.lookup q bind of
            Just t  -> Map.insert q t b
            Nothing -> b

-- Concatenate together two streams of states
disj :: Goal -> Goal -> Goal
disj g1 g2 state = do
    s1 <- g1 state
    s2 <- g2 state
    return $ s1 `interleave` s2

-- Apply the second goal to every state in the stream evaluated from first goal, concatenate all results
conj :: Goal -> Goal -> Goal
conj g1 g2 state = do
    s1 <- g1 state
    s' <- mapM g2 s1
    return $ fairsum s'

    where fairsum :: Logic (Logic KanrenState) -> Logic KanrenState
          fairsum = foldr interleave mzero

-- Call a previously defined relation
-- KNOWN BUG: 
-- If you call a relation with a variable introduced inside a define relation parameter list (recursive relation),
-- then the algorithm doesnt work, due to naming conflicts. At the moment, defining a new variable and using a 
-- conjunction to add an equality constraint to the intended one seems to work! 
-- FIX:
-- Can add an extra preprocessing step to callRelation which adds fresh bindings which serve as renamings of all args,
-- Since variable name does not matter, can use indexing with some prefix that is disallowed in identifiers.
-- DRAWBACK: 
-- Is redundant in most cases.
callRelation :: String -> [Term] -> Goal
callRelation name args = binded $ \state -> do
    Env relations <- get
    case Map.lookup name relations of
        Just r  -> let args' = map ID idents
                       g = r args'
                   in g state
        Nothing -> error "Relation not found!"

    where binded goal = fresh idents $ padded goal
          padded goal = conj goal $ conjPlus constraints

          constraints = [t === ID i| (t, i) <- zip args idents]

          idents = ["_" ++ show n | n <- [0..length args]]

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

fresh :: [String] -> Goal -> Goal
fresh idents g = foldr callFresh g idents

---
-- Initial state
--- 

initialState :: KanrenState
initialState = State Map.empty Map.empty 0

initialEnv :: Environment
initialEnv = Env Map.empty

---
-- Statements: maintain a global state of defined relations
---

defineRelation :: String -> [String] -> Goal -> State Environment ()
defineRelation name idents goal = modify addRelation
    where addRelation (Env e) = Env $ Map.insert name binded e

          binded args = fresh idents $ padded args
          padded args = conj goal $ conjPlus $ constraints args

          constraints args = [t === ID i| (t, i) <- zip args idents]

type Stream = [[(String, Maybe Term)]]

run :: Int -> [String] -> Goal -> State Environment Stream
run i idents g = do
    stream <- g initialState
    let results = reifyAll idents $ observeMany i stream
    return results


runAll :: [String] -> Goal -> State Environment Stream
runAll idents g = do
    stream <- g initialState
    let results = reifyAll idents $ observeAll stream
    return results

---
-- Reifiers
---

reifyAll :: [String] -> [KanrenState] -> Stream
reifyAll idents = map (reify idents)

reify :: [String] -> KanrenState -> [(String, Maybe Term)]
reify idents (State subst bind _) = zip idents terms
    where terms = map fromString idents

          fromString :: String -> Maybe Term
          fromString i = Map.lookup i bind >>= getTerm subst

