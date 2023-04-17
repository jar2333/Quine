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

class UTerm t where
    -- Pretty-print the term.
    pretty :: t -> String 

    -- Wrap the given string as a uvar term.
    uvar :: String -> t

    -- Substitute every uvar subterm which can be found in a binding map with the corresponding var.
    substUvar :: t -> Bind -> t

    -- Find a stream of substitutions that can unify the two terms given a base substitution.
    unify :: t -> t -> USubst t -> Logic (USubst t)

    -- Find term corredponding to a uvar term, return itself on failure or if not a uvar term.
    find :: t -> USubst t -> t    

    -- Use a substitution to replace each uvar subterm with the corresponding term in the substitution
    -- Should replace a uvar subterm with a wildcard if it does not exist in the substitution.
    replace :: t -> USubst t -> t

    -- Gets the full expanded term corresponding to the given var in a given substitution (no vars).
    getTerm :: USubst t  -> Var -> Maybe t
    getTerm subst v = Map.lookup v subst >>= return . \t -> replace t subst

