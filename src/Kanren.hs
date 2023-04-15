module Kanren
    ( (===)
    , callFresh
    , disj
    , conj
    , callRelation
    , disjPlus
    , conjPlus
    , initialEnv
    , defineRelation
    , run
    , runAll
    , reify
    , printSubst
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

import Data.Char ( toUpper, toLower )
import Data.List ( intercalate )

type Var = Int

data Term = ID String | Var Var | Symbol String | Bool Bool | Nil | Pair Term Term deriving (Eq, Show)

pretty :: Term -> String
pretty (Pair t1 t2) = "(" ++ pretty t1 ++ ", " ++ pretty t2 ++ ")"
pretty (ID i) = map toUpper i
pretty (Var v) = show v
pretty (Symbol s) = s
pretty (Bool b) = map toLower $ show b
pretty Nil = "()"

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

newtype Environment = Env (Map.Map String ([Term] -> Goal))

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

-- Unification constraints
(===) :: Term -> Term -> Goal
(===) u v (State s b c) =
    case unify (find u' s) (find v' s) s of
        Just s' -> return $ return (State s' b c)
        Nothing -> return mzero
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

    return $ do
        (State s b c) <- stream
        return $ State s (restore b) c
    
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

    where binded goal = foldr callFresh (padded goal) idents
          padded goal = foldr ($) goal constraints

          constraints = [conj (t === ID i) | (t, i) <- zip args idents]

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

          binded args = foldr callFresh (padded args) idents
          padded args = foldr ($) goal (constraints args)

          constraints args = [conj (t === ID i) | (t, i) <- zip args idents]


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

reify :: [String] -> KanrenState -> [(String, Maybe Term)]
reify idents (State subst bind _) = zip idents terms
    where terms = map fromString idents

          fromString :: String -> Maybe Term
          fromString i = getTerm =<< Map.lookup i bind

          getTerm :: Var -> Maybe Term
          getTerm v = do
                t <- Map.lookup v subst
                return $ replace t

          replace :: Term -> Term
          replace (Var v) = fromMaybe (ID "_") (getTerm v) -- If no substitution exists, any answer suffices!
          replace (Pair t1 t2) = Pair (replace t1) (replace t2)
          replace x = x

printSubst :: [(String, Maybe Term)] -> String
printSubst results = subst
    where subst = "{" ++ intercalate ", " r ++ "}"
          r = [printTerm i t | (i, t) <- results]
          printTerm i Nothing  = i ++ ": _"
          printTerm i (Just t) = i ++ ": " ++ pretty t
