{-# LANGUAGE InstanceSigs #-}

module UTerm
    ( UTerm(..)
    , Var
    , Bind
    , USubst
    ) where

import Control.Monad.Logic ( Logic )

import Data.Map as Map ( Map, lookup )

type Var = Int
type Bind = Map.Map String Var
type USubst t = Map.Map Var t 

class (Show t, Eq t) => UTerm t where
    ---
    -- Pretty-print term
    ---

    -- Pretty-print the term. By default, is show.
    pretty :: t -> String 
    pretty = show

    ---
    -- Wrapper functions
    ---

    -- Wrap the given String as a uvar term.
    uvar :: String -> t

    -- Wrap the given Var as a var term.
    var :: Var -> t

    ---
    -- Substitution of unification variables
    ---

    -- subst N x M = M[x := N] 
    -- Substitute all instances of a uvar x in M with N
    substitute :: t -> String -> t -> t

    ---
    -- Unification of terms
    ---

    -- Find a stream of substitutions that can unify the two terms given a base substitution.
    -- Signature comes from microKanren paper.
    unify :: t -> t -> USubst t -> Logic (USubst t)

    -- Find term corredponding to a uvar term, return itself on failure or if not a uvar term.
    -- Behavior and signature comes from microKanren paper.
    find :: t -> USubst t -> t    

    ---
    -- Reification
    --- 

    -- Use a substitution to replace each var subterm with the corresponding term in the substitution
    -- Should replace a var subterm with a wildcard if it does not exist in the substitution.
    replace :: t -> USubst t -> t

    -- Gets the full expanded term corresponding to the given var in a given substitution (no vars).
    getTerm :: USubst t  -> Var -> Maybe t
    getTerm subst v = Map.lookup v subst >>= return . \t -> replace t subst
