{-# LANGUAGE InstanceSigs #-}

module LambdaTerm (
    LambdaTerm (..),
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
import qualified Data.Set as Set

type UVar = String
type LambdaVar = String
type Binder = (LambdaVar, Type)
data LambdaTerm
    = ID UVar
    | Var Var
    | LVar LambdaVar Type
    | Abs Binder LambdaTerm Type
    | App LambdaTerm LambdaTerm Type
    | Let LambdaVar LambdaTerm LambdaTerm Type
    | Pair LambdaTerm LambdaTerm Type
    | Fst LambdaTerm Type
    | Snd LambdaTerm Type
    deriving (Eq)

instance Show LambdaTerm where
    show (ID s) = s
    show (Var i) = show i
    show (LVar lv ty) = show lv ++ " : " ++ show ty
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


fv :: LambdaTerm -> Set.Set String
fv (LVar varid _) = Set.singleton varid
fv (Abs bin body _) = Set.difference (fv body) (Set.singleton $ fst bin)
fv (App fun arg _) = Set.union (fv fun) (fv arg)
fv (Let lv _ body _) = Set.difference (fv body) (Set.singleton lv)
fv (Pair l r _) = Set.union (fv l) (fv r)
fv (Fst term _) = fv term
fv (Snd term _) = fv term
fv  _ = Set.empty


instance UTerm LambdaTerm where
    -- Pretty-print the term.
    pretty :: LambdaTerm -> String
    pretty = show

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
