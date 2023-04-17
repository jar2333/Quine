{-# LANGUAGE InstanceSigs #-}

module Term
    ( Term(..)
    , pretty
    , substID
    , unify
    , getTerm
    , replace
    , find
    , Subst
    ) where

import Data.Map as Map ( insert, lookup )
import Data.Char ( toUpper, toLower )
import Data.Maybe as Maybe ( fromMaybe )

import Control.Monad ( MonadPlus(mzero) )
import Control.Monad.Logic ( Logic )

import UTerm

data Term = ID String | Var Var | Symbol String | Bool Bool | Nil | Pair Term Term deriving (Eq, Show)

---
-- Type of substitution + print entire Term corresponding to variable in substitution.
---

type Subst = USubst Term

---
-- Term is a UTerm
---

instance UTerm Term where
    pretty :: Term -> String
    pretty (Pair t1 t2) = "(" ++ pretty t1 ++ ", " ++ pretty t2 ++ ")"
    pretty (ID i) = map toUpper i
    pretty (Var v) = show v
    pretty (Symbol s) = s
    pretty (Bool b) = map toLower $ show b
    pretty Nil = "()"

    substID :: Term -> Bind -> Term
    substID (ID i) b = maybe (error $ i ++ " NOT FOUND") Var (Map.lookup i b)
    substID (Pair t1 t2) b = Pair (substID t1 b) (substID t2 b)
    substID x _ = x

    unify :: Term -> Term -> Subst -> Logic Subst
    unify u v s | u == v              = return s
    unify (Var u) v s                 = extendSubst u v s
    unify u var@(Var _) s             = unify var u s
    unify (Pair ua ub) (Pair va vb) s = unify (find ua s) (find va s) s >>= \s' -> unify (find ub s') (find vb s') s'
    unify _ _ _                       = mzero

    replace :: Subst -> Term -> Term
    replace subst (Var v) = fromMaybe (ID "_") (getTerm subst v) -- If no substitution exists, any answer suffices!
    replace subst (Pair t1 t2) = Pair (replace subst t1) (replace subst t2)
    replace _ x = x

---
-- First-order unification implementation
---

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