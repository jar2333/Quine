module Unification
    ( Subst
    , unify
    , find
    ) where

import Data.Map as Map ( insert, lookup, Map )
import Data.Maybe as Maybe ( fromMaybe )

import Control.Monad ( MonadPlus(mzero) )
import Control.Monad.Logic

import Term

---
-- Type of substitution
---

type Subst = Map.Map Var Term

---
-- First-order unification implementation
---

unify :: Term -> Term -> Subst -> Logic Subst
unify u v s | u == v              = return s
unify (Var u) v s                 = extendSubst u v s
unify u var@(Var _) s             = unify var u s
unify (Pair ua ub) (Pair va vb) s = unify (find ua s) (find va s) s >>= \s' -> unify (find ub s') (find vb s') s'
unify _ _ _                       = mzero

extendSubst :: Var -> Term -> Subst -> Logic Subst
extendSubst x v s | occurs x v s = mzero
                  | otherwise    = return (Map.insert x v s)

occurs :: Var -> Term -> Subst -> Bool
occurs x (Var u) _    = x == u
occurs x (Pair a b) s = occurs x (find a s) s || occurs x (find b s) s
occurs _ _ _          = False

find :: Term -> Subst -> Term
find (Var u) s = Maybe.fromMaybe (Var u) (Map.lookup u s)
find t _ = t





