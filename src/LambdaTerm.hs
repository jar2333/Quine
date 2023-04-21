{-# LANGUAGE InstanceSigs #-}

module LambdaTerm (
    LambdaTerm,
    pretty,
    unify,
    getTerm,
    replace,
    find,
    uvar,
) where

import Control.Monad.Logic
import Type
import UTerm

type UVar = String
type LambdaVar = String
type Binder = (Var, Type)
data LambdaTerm
    = ID UVar
    | Var Var
    | LVar LambdaVar Type
    | Abs Binder LambdaTerm Type
    | App LambdaTerm LambdaTerm Type
    | Let Var LambdaTerm LambdaTerm Type
    | Pair LambdaTerm LambdaTerm Type
    | Fst LambdaTerm Type
    | Snd LambdaTerm Type
    deriving (Eq, Show)

type Subst = USubst LambdaTerm

instance UTerm LambdaTerm where
    -- Pretty-print the term.
    pretty :: LambdaTerm -> String
    pretty t = error "not implemented"

    -- subst N x M = M[x := N]
    -- Substitute all instances of a uvar x with N in M
    substitute :: LambdaTerm -> String -> LambdaTerm -> LambdaTerm
    substitute n x t = error "not implemented"

    -- Find a stream of substitutions that can unify the two terms given a base substitution.
    unify :: LambdaTerm -> LambdaTerm -> Subst -> Logic Subst
    unify u v s = error "not implemented"

    -- Use a substitution to replace each uvar subterm with the corresponding term in the substitution
    replace :: LambdaTerm -> Subst -> LambdaTerm
    replace t subst = error "not implemented"

    -- Find term corredponding to a uvar term, return itself on failure or if not a uvar term.
    find :: LambdaTerm -> Subst -> LambdaTerm
    find t s = error "not implemented"

    -- Wrap the given string as a uvar term.
    uvar :: String -> LambdaTerm
    uvar = ID

    -- Wrap the given Var as a var term.
    var :: Var -> LambdaTerm
    var = Var
