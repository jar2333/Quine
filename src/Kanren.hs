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

---
-- State and Goal types
---

data KanrenState t = State {
                            substitution :: USubst t,
                            bindings :: Bind,
                            count :: Int
                        } deriving (Show)

type Goal t = KanrenState t -> State (Environment t) (Logic (KanrenState t))

---
-- Environment: define and call relations
---

type Relation t = [t] -> Goal t

newtype Environment t = Env (Map.Map String (Relation t))

---
-- Goal constructors
---

-- Unification constraints
(===) :: (UTerm t) => t -> t -> Goal t
(===) u v (State s b c) = return $ do
    s' <- unify (find u' s) (find v' s) s 
    return (State s' b c)
    where u' = substID u b
          v' = substID v b

-- For the subtree, make it so every instance of q is replaced with cnt
-- Then for each state in the result stream, restore the original binding.
-- This makes it so that the states in the resulting stream only have the 
-- topmost binding for any shadowed variables.
callFresh :: (UTerm t) => String -> Goal t -> Goal t
callFresh q g (State subt bind cnt) = do
    let updated = Map.insert q cnt bind
    stream <- g $ State subt updated (cnt+1)

    let restored = stream >>= \(State s b c) -> return $ State s (restore b) c
    return restored
    
    where restore b = case Map.lookup q bind of
            Just t  -> Map.insert q t b
            Nothing -> b

-- Concatenate together two streams of states
disj :: (UTerm t) => Goal t -> Goal t -> Goal t
disj g1 g2 state = do
    s1 <- g1 state
    s2 <- g2 state
    return $ s1 `interleave` s2

-- Apply the second goal to every state in the stream evaluated from first goal, concatenate all results
conj :: (UTerm t) => Goal t -> Goal t -> Goal t
conj g1 g2 state = do
    s1 <- g1 state
    s' <- mapM g2 s1
    return $ fairsum s'

    where fairsum :: Logic (Logic (KanrenState t)) -> Logic (KanrenState t)
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
callRelation :: (UTerm t) => String -> [t] -> Goal t
callRelation name args = binded $ \state -> do
    Env relations <- get
    case Map.lookup name relations of
        Just r  -> let args' = map uvar idents
                       g = r args'
                   in g state
        Nothing -> error "Relation not found!"

    where binded goal = fresh idents $ padded goal
          padded goal = conj goal $ conjPlus constraints

          constraints = [t === uvar i| (t, i) <- zip args idents]

          idents = ["_" ++ show n | n <- [0..length args]]

---
-- Extensions
---

disjPlus :: (UTerm t) => [Goal t] -> Goal t
disjPlus [g]    = g
disjPlus (g:gs) = disj g (disjPlus gs)
disjPlus [] = error "Not possible."

conjPlus :: (UTerm t) => [Goal t] -> Goal t
conjPlus [g]    = g
conjPlus (g:gs) = conj g (conjPlus gs)
conjPlus [] = error "Not possible."

fresh :: (UTerm t) => [String] -> Goal t -> Goal t
fresh idents g = foldr callFresh g idents

---
-- Initial state
--- 

initialState :: KanrenState t
initialState = State Map.empty Map.empty 0

initialEnv :: Environment t
initialEnv = Env Map.empty

---
-- Statements: maintain a global state of defined relations
---

defineRelation :: (UTerm t) => String -> [String] -> Goal t -> State (Environment t) ()
defineRelation name idents goal = modify addRelation
    where addRelation (Env e) = Env $ Map.insert name binded e

          binded args = fresh idents $ padded args
          padded args = conj goal $ conjPlus $ constraints args

          constraints args = [t === uvar i| (t, i) <- zip args idents]

type Stream t = [[(String, Maybe t)]]

run :: (UTerm t) => Int -> [String] -> Goal t -> State (Environment t) (Stream t)
run i idents g = do
    stream <- g initialState
    let results = reifyAll idents $ observeMany i stream
    return results


runAll :: (UTerm t) => [String] -> Goal t -> State (Environment t) (Stream t)
runAll idents g = do
    stream <- g initialState
    let results = reifyAll idents $ observeAll stream
    return results

---
-- Reifiers
---

reifyAll :: (UTerm t) => [String] -> [KanrenState t] -> (Stream t)
reifyAll idents = map (reify idents)

reify :: (UTerm t) => [String] -> KanrenState t -> [(String, Maybe t)]
reify idents (State subst bind _) = zip idents terms
    where terms = map fromString idents
          fromString i = Map.lookup i bind >>= getTerm subst

