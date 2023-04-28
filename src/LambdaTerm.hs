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

import Control.Monad
import Control.Monad.Logic
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import Data.Maybe (fromMaybe)
import Type
import UTerm

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

instance UTerm LambdaTerm where
    -- Pretty-print the term.
    pretty :: LambdaTerm -> String
    pretty = show

    -- subst N x M = M[x := N]
    -- Substitute all instances of a uvar x with N in M
    substitute :: LambdaTerm -> String -> LambdaTerm -> LambdaTerm
    substitute n x v@(LVar y _) = if y == x then n else v
    substitute n x (App fun arg t) = App (substitute n x fun) (substitute n x arg) t
    substitute n x a@(Abs bin@(y, yt) body t)
        | y == x = a
        | y `Set.notMember` fv n = Abs bin (substitute n x body) t
        | otherwise = Abs bin' (substitute n x body') t
      where
        body' = substitute (LVar y' yt) y body
        bin' = (y', yt)
        y' = fresh fvMN y
        fvMN = Set.union (fv body) (fv n)
    -- TODO: I need to check let subsitution is correct
    substitute n x (Let v e1 e2 t) = Let v e1 (substitute n x e2) t
    substitute n x (Pair l r t) = Pair (substitute n x l) (substitute n x r) t
    substitute n x (Fst e t) = Fst (substitute n x e) t
    substitute n x (Snd e t) = Fst (substitute n x e) t
    substitute _ _ t = t

    -- Find a stream of substitutions that can unify the two terms given a base substitution.
    unify :: LambdaTerm -> LambdaTerm -> Subst -> Logic Subst
    unify u v s | normalize u == normalize v = return s
    unify _ _ _ = mzero

    -- Use a substitution to replace each uvar subterm with the corresponding term in the substitution
    replace :: LambdaTerm -> Subst -> LambdaTerm
    replace (Var v) subst = fromMaybe (ID "_") (getTerm subst v)
    replace (Abs bin term t) subst = Abs bin (replace term subst) t
    replace (App t1 t2 t) subst = App (replace t1 subst) (replace t2 subst) t
    replace (Let v t1 t2 t) subst = Let v (replace t1 subst) (replace t2 subst) t
    replace (Pair t1 t2 t) subst = Pair (replace t1 subst) (replace t2 subst) t
    replace (Fst e t) subst = Fst (replace e subst) t
    replace (Snd e t) subst = Snd (replace e subst) t
    replace x _ = x

    -- Find term corredponding to a uvar term, return itself on failure or if not a uvar term.
    find :: LambdaTerm -> Subst -> LambdaTerm
    find (Var u) s = Maybe.fromMaybe (Var u) (Map.lookup u s)
    find t _ = t

    -- Wrap the given string as a uvar term.
    uvar :: String -> LambdaTerm
    uvar = ID

    -- Wrap the given Var as a var term.
    var :: Var -> LambdaTerm
    var = Var


-- | Return the free variables in a lambda term
fv :: LambdaTerm -> Set.Set LambdaVar
fv (LVar varid _) = Set.singleton varid
fv (Abs bin body _) = Set.difference (fv body) (Set.singleton $ fst bin)
fv (App fun arg _) = Set.union (fv fun) (fv arg)
fv (Let lv _ body _) = Set.difference (fv body) (Set.singleton lv)
fv (Pair l r _) = Set.union (fv l) (fv r)
fv (Fst term _) = fv term
fv (Snd term _) = fv term
fv _ = Set.empty

{- | Return a "fresh" name not already in the set.
 Tries x' then x'', etc.
-}
fresh :: Set.Set LambdaVar -> LambdaVar -> LambdaVar
fresh s = fresh'
  where
    fresh' n | n `Set.notMember` s = n
    fresh' n = fresh' $ n ++ "'"

-- | Check if a term is normal
normal :: LambdaTerm -> Bool
normal Var{} = True
normal ID{} = True
normal LVar{} = True
normal Abs{} = True -- call-by-value
normal (Pair l r _) = normal l && normal r
normal _ = False

-- | Take a step towards being normalized
step :: LambdaTerm -> LambdaTerm
-- reduction rules
step (App (Abs bin m _) n _) = substitute n (fst bin) m
step (Fst (Pair l _ _) _) = l
step (Snd (Pair _ r _) _) = r
step (App l r t)
    | normal l = App l (step r) t
    | otherwise = App (step l) r t
step (Pair l r t)
    | normal l = Pair l (step r) t
    | otherwise = Pair (step l) r t
step (Fst e t) = Fst (step e) t
step (Snd e t) = Snd (step e) t
step x = x

-- | Normalize a term to the end
normalize :: LambdaTerm -> LambdaTerm
normalize t =
    if normal t
        then t
        else normalize $ step t
