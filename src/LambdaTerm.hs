{-# LANGUAGE InstanceSigs #-}

module LambdaTerm (
    LambdaTerm (..),
    LambdaVar,
    pretty,
    unify,
    getTerm,
    replace,
    find,
    uvar,
) where

import Control.Monad
import Control.Monad.Logic
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import qualified Data.Maybe as Maybe
import Type
import UTerm (USubst, UTerm (..))

type UVar = String
type LambdaVar = String
type Binder = (LambdaVar, Type)

data LambdaTerm
    = UVar UVar
    | ID Int
    | Var LambdaVar Type
    | Abs Binder LambdaTerm Type
    | App LambdaTerm LambdaTerm Type
    | Let LambdaVar LambdaTerm LambdaTerm Type
    | Pair LambdaTerm LambdaTerm Type
    | Fst LambdaTerm Type
    | Snd LambdaTerm Type
    deriving (Eq)

instance Show LambdaTerm where
    show (UVar s) = s
    show (ID i) = show i
    show (Var lv ty) = show lv ++ " : " ++ show ty
    show (Abs (v, t) e ty) =
        "\\"
            ++ "("
            ++ show v
            ++ " : "
            ++ show t
            ++ ")"
            ++ " -> "
            ++ show e
            ++ " : "
            ++ show ty
    show (App e1 e2 ty) = show e1 ++ " on " ++ show e2 ++ " : " ++ show ty
    show (Let v e1 e2 ty) =
        "let"
            ++ v
            ++ "="
            ++ show e1
            ++ "in"
            ++ show e2
            ++ " : "
            ++ show ty
    show (Pair l r ty) = "(" ++ show l ++ ", " ++ show r ++ ")" ++ " : " ++ show ty
    show (Fst e ty) = "fst" ++ show e ++ " : " ++ show ty
    show (Snd e ty) = "snd" ++ show e ++ " : " ++ show ty

type Subst = USubst LambdaTerm

instance UTerm LambdaTerm where
    -- Pretty-print the term.
    pretty :: LambdaTerm -> String
    pretty = show

    -- subst N x M = M[x := N]
    -- Substitute all instances of a uvar x with N in M
    substitute :: LambdaTerm -> String -> LambdaTerm -> LambdaTerm
    substitute _ _ _ = error "not implemented"

    -- Find a stream of substitutions that can unify the two terms given a base substitution.
    unify :: LambdaTerm -> LambdaTerm -> Subst -> Logic Subst
    unify _ _ _ = mzero

    -- Use a substitution to replace each uvar subterm with the corresponding term in the substitution
    replace :: LambdaTerm -> Subst -> LambdaTerm
    replace (ID v) subst = fromMaybe (UVar "_") (getTerm subst v)
    replace (Abs bin term t) subst = Abs bin (replace term subst) t
    replace (App t1 t2 t) subst = App (replace t1 subst) (replace t2 subst) t
    replace (Let v t1 t2 t) subst = Let v (replace t1 subst) (replace t2 subst) t
    replace (Pair t1 t2 t) subst = Pair (replace t1 subst) (replace t2 subst) t
    replace (Fst e t) subst = Fst (replace e subst) t
    replace (Snd e t) subst = Snd (replace e subst) t
    replace x _ = x

    -- Find term corredponding to a uvar term, return itself on failure or if not a uvar term.
    find :: LambdaTerm -> Subst -> LambdaTerm
    find (ID u) s = Maybe.fromMaybe (ID u) (Map.lookup u s)
    find t _ = t

    -- Wrap the given string as a uvar term.
    uvar :: String -> LambdaTerm
    uvar = UVar

    -- Wrap the given ID as a ID term.
    ident :: Int -> LambdaTerm
    ident = ID
