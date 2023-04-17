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
    , reifyAll
    , printStream
    , Term(..)
    , Goal
    , KanrenState
    , Environment(..)
    ) where


import Data.Map as Map ( insert, lookup, empty, Map )
import Data.Maybe as Maybe ( fromMaybe )

import Control.Monad ( MonadPlus(mzero) )
import Control.Monad.Logic
import Control.Monad.State ( MonadState(get), State, modify )

import Term (Term(..), Var, pretty)

import Data.List ( intercalate )

type Subst = Map.Map Var Term
type Bind  = Map.Map String Var

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
-- First-order unification implementation
---

find :: Term -> Subst -> Term
find (Var u) s = Maybe.fromMaybe (Var u) (Map.lookup u s)
find t _ = t

occurs :: Var -> Term -> Subst -> Bool
occurs x (Var u) _    = x == u
occurs x (Pair a b) s = occurs x (find a s) s || occurs x (find b s) s
occurs _ _ _          = False

extendSubst :: Var -> Term -> Subst -> Logic Subst
extendSubst x v s | occurs x v s = mzero
                  | otherwise    = return (Map.insert x v s)

unify :: Term -> Term -> Subst -> Logic Subst
unify u v s | u == v              = return s
unify (Var u) v s                 = extendSubst u v s
unify u var@(Var _) s             = unify var u s
unify (Pair ua ub) (Pair va vb) s = unify (find ua s) (find va s) s >>= \s' -> unify (find ub s') (find vb s') s'
unify _ _ _                       = mzero

---
-- Goal constructors
---

-- Unification constraints
(===) :: Term -> Term -> Goal
(===) u v (State s b c) = return $ do
    s' <- unify (find u' s) (find v' s) s 
    return (State s' b c)
    where u' = substID u
          v' = substID v
          substID (ID i) = maybe (error $ i ++ " NOT FOUND") Var (Map.lookup i b)
          substID (Pair t1 t2) = Pair (substID t1) (substID t2)
          substID x = x

-- For the subtree, make it so every instance of q is replaced with cnt
-- Then for each state in the result stream, restore the original binding
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
    where
         fairsum :: Logic (Logic KanrenState) -> Logic KanrenState
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


run :: Int -> Goal -> State Environment [KanrenState]
run i g = do
    stream <- g initialState
    return $ observeMany i stream

runAll :: Goal -> State Environment [KanrenState]
runAll g = do
    stream <- g initialState
    return $ observeAll stream

---
-- Reifiers
---

reifyAll :: [String] -> [KanrenState] -> [[(String, Maybe Term)]]
reifyAll idents = map (reify idents)

printStream :: [[(String, Maybe Term)]] -> String
printStream stream = "[" ++ intercalate ", " (map printSubst stream) ++ "]"

reify :: [String] -> KanrenState -> [(String, Maybe Term)]
reify idents (State subst bind _) = zip idents terms
    where terms = map fromString idents

          fromString :: String -> Maybe Term
          fromString i = Map.lookup i bind >>= getTerm

          getTerm :: Var -> Maybe Term
          getTerm v = Map.lookup v subst >>= return . replace

          replace :: Term -> Term
          replace (Var v) = fromMaybe (ID "_") (getTerm v) -- If no substitution exists, any answer suffices!
          replace (Pair t1 t2) = Pair (replace t1) (replace t2)
          replace x = x

printSubst :: [(String, Maybe Term)] -> String
printSubst results = subst
    where subst = "{" ++ intercalate ", " pairs ++ "}"
          pairs = [printTerm i t | (i, t) <- results]
          printTerm i Nothing  = i ++ ": _"
          printTerm i (Just t) = i ++ ": " ++ pretty t
