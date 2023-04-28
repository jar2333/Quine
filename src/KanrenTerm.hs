{-# LANGUAGE InstanceSigs #-}

module KanrenTerm (
    KanrenTerm (..),
    pretty,
    unify,
    getTerm,
    replace,
    find,
    uvar,
    ident,
    Subst,
) where

import Data.Char (toLower, toUpper)
import Data.Map as Map (insert, lookup)
import Data.Maybe as Maybe (fromMaybe)

import Control.Monad (MonadPlus (mzero))
import Control.Monad.Logic (Logic)

import UTerm

data KanrenTerm = UVar String | ID Int | Symbol String | Bool Bool | Nil | Pair KanrenTerm KanrenTerm deriving (Eq, Show)

---
-- Type of substitution + print entire Term corresponding to variable in substitution.
---

type Subst = USubst KanrenTerm

---
-- Term is a UTerm
---

instance UTerm KanrenTerm where
    -- Pretty-print the term.
    pretty :: KanrenTerm -> String
    pretty (Pair t1 t2) = "(" ++ pretty t1 ++ ", " ++ pretty t2 ++ ")"
    pretty (UVar i) = map toUpper i
    pretty (ID v) = show v
    pretty (Symbol s) = s
    pretty (Bool b) = map toLower $ show b
    pretty Nil = "()"

    -- subst N x M = M[x := N]
    -- Substitute all instances of a uvar x with N in M
    substitute :: KanrenTerm -> String -> KanrenTerm -> KanrenTerm
    substitute n x (Pair m1 m2) = Pair (substitute n x m1) (substitute n x m2)
    substitute n x (UVar i) | i == x = n
    substitute _ _ m = m

    -- Find a stream of substitutions that can unify the two terms given a base substitution.
    unify :: KanrenTerm -> KanrenTerm -> Subst -> Logic Subst
    unify u v s | u == v = return s
    unify (ID u) v s = extendSubst u v s
    unify u v@(ID _) s = unify v u s
    unify (Pair ua ub) (Pair va vb) s = unify (find ua s) (find va s) s >>= \s' -> unify (find ub s') (find vb s') s'
    unify _ _ _ = mzero

    -- Use a substitution to replace each uvar subterm with the corresponding term in the substitution
    replace :: KanrenTerm -> Subst -> KanrenTerm
    replace (ID v) subst = fromMaybe (UVar "_") (getTerm subst v) -- If no substitution exists, any answer suffices!
    replace (Pair t1 t2) subst = Pair (replace t1 subst) (replace t2 subst)
    replace x _ = x

    -- Find term corredponding to a uvar term, return itself on failure or if not a uvar term.
    find :: KanrenTerm -> Subst -> KanrenTerm
    find (ID u) s = Maybe.fromMaybe (ID u) (Map.lookup u s)
    find t _ = t

    -- Wrap the given string as a uvar term.
    uvar :: String -> KanrenTerm
    uvar = UVar

    -- Wrap the given Int as an Ident term.
    ident :: Int -> KanrenTerm
    ident = ID

---
-- First-order unification implementation
---

extendSubst :: Int -> KanrenTerm -> Subst -> Logic Subst
extendSubst x v s
    | occurs x v s = mzero
    | otherwise = return (Map.insert x v s)

occurs :: Int -> KanrenTerm -> Subst -> Bool
occurs x (ID u) _ = x == u
occurs x (Pair a b) s = occurs x (find a s) s || occurs x (find b s) s
occurs _ _ _ = False
