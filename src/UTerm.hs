module UTerm (
    UTerm (..),
    Bind,
    USubst,
) where


import Data.Map as Map (Map, lookup)
import Control.Monad.Logic

type Bind = Map.Map String Int
type USubst t = Map.Map Int t 

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

    -- Wrap the given int as an identifier term.
    ident :: Int -> t

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

    -- Use a substitution to replace each identifier subterm with the corresponding term in the substitution
    -- Should replace an identifier subterm with a wildcard if it does not exist in the substitution.
    replace :: t -> USubst t -> t

    -- Gets the full expanded term corresponding to the given identifier in a given substitution.
    getTerm :: USubst t  -> Int -> Maybe t
    getTerm subst v = Map.lookup v subst >>= return . \t -> replace t subst
