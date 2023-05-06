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
import qualified Data.Set as Set
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import Data.Maybe (fromMaybe)

import Type
import UTerm (USubst, UTerm (..))
import qualified Data.Bifunctor

type UVar = String
type LambdaVar = String
type Binder = (LambdaVar, Type)

data LambdaTerm
    = UVar UVar
    | ID Int
    | ConstInt Int Type
    | ConstBool Bool Type
    | Var LambdaVar Type
    | Abs Binder LambdaTerm Type
    | App LambdaTerm LambdaTerm Type
    | Let LambdaVar LambdaTerm LambdaTerm Type
    | Pair LambdaTerm LambdaTerm Type
    | Fst LambdaTerm Type
    | Snd LambdaTerm Type
    deriving (Eq)

isRigid :: LambdaTerm -> Set.Set LambdaVar -> Bool
isRigid (Abs (v, _) t _) accum = isRigid t (Set.insert v accum)
isRigid (Var v _) accum = Set.mem v accum  -- check if it's bound
isRigid (Costant _) accum = True 

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
    show (ConstInt i ty) = "const"  ++ show i ++ " : " ++ show ty
    show (ConstBool b ty) = "const"  ++ show b ++ " : " ++ show ty


type Subst = USubst LambdaTerm

instance UTerm LambdaTerm where
    -- Pretty-print the term.
    pretty :: LambdaTerm -> String
    pretty = show

    -- subst N x M = M[x := N]
    -- Substitute all instances of a uvar x with N in M
    substitute :: LambdaTerm -> String -> LambdaTerm -> LambdaTerm
    substitute n x u@(UVar y) | y == x    = n
                              | otherwise = u
    substitute n x (Abs b t ty)      = Abs b (substitute n x t) ty
    substitute n x (App t1 t2 ty)    = App (substitute n x t1) (substitute n x t2) ty
    substitute n x (Let y t1 t2 ty)  = Let y (substitute n x t1) (substitute n x t2) ty
    substitute n x (Pair t1 t2 ty)   = Pair (substitute n x t1) (substitute n x t2) ty
    substitute n x (Fst t ty)        = Fst (substitute n x t) ty
    substitute n x (Snd t ty)        = Snd (substitute n x t) ty
    substitute _ _ m                 = m

    -- Find a stream of substitutions that can unify the two terms given a base substitution.
    unify :: LambdaTerm -> LambdaTerm -> Subst -> Logic Subst
    unify t1 t2 _ = simplify [(t1, t2)]

    -- Find term corredponding to an identifier term, return itself on failure or if not an identifier term
    find :: LambdaTerm -> Subst -> LambdaTerm
    find (ID u) s = Maybe.fromMaybe (ID u) (Map.lookup u s)
    find t _ = t

    -- Use a substitution to replace each uvar subterm with the corresponding term in the substitution
    replace :: LambdaTerm -> Subst -> LambdaTerm
    replace (ID v) s = fromMaybe (UVar "_") (getTerm s v)
    replace (Abs bin term t) s = Abs bin (replace term s) t
    replace (App t1 t2 t) s = App (replace t1 s) (replace t2 s) t
    replace (Let v t1 t2 t) s = Let v (replace t1 s) (replace t2 s) t
    replace (Pair t1 t2 t) s = Pair (replace t1 s) (replace t2 s) t
    replace (Fst e t) s = Fst (replace e s) t
    replace (Snd e t) s = Snd (replace e s) t
    replace x _ = x

    -- Wrap the given string as a uvar term.
    uvar :: String -> LambdaTerm
    uvar = UVar

    -- Wrap the given ID as a ID term.
    ident :: Int -> LambdaTerm
    ident = ID

---
-- Unification
---

simplify :: [(LambdaTerm, LambdaTerm)] -> Logic Subst
simplify pairs = do
    -- Beta/eta reduce every disagreement pair
    let reduced = map (Data.Bifunctor.bimap normalize normalize) pairs

    -- Eliminate all rigid/rigid pairs
    let simplified = eliminate reduced

    -- If a rigid/rigid pair cannot be unified, return failure (mzero)
    -- Otherwise, continue
    case simplified of
        Nothing -> mzero
        Just s -> do
            -- Swap all rigid/flexible pairs with the flexible/rigid reordering 
            let swapped = [if isRigid r && isFlexible f then (f, r) else pair | pair@(r, f) <- s]

            -- Continue the procedure by calling match on an arbitrary flexible/rigid member of the set of simplified pairs
            -- If only flexible/flexible pairs are left, directly construct a unifier
            case List.find (\(_, r) -> isRigid r) swapped of
                Nothing -> construct s 
                Just p  -> match p swapped

    where --  If both terms of any rigid/rigid pair 〈t, t′〉have different heads (modulo alpha-reduction), they cannot be unified, return failure (Nothing)
          --  Otherwise, when they have the same head, then replace〈t, t′〉by {〈λx1 . . . xn. ti, λx1 . . . xn. ui〉 | i ∈ [p]} (Just $ concatMap ...)
          eliminate :: [(LambdaTerm, LambdaTerm)] -> Maybe [(LambdaTerm, LambdaTerm)]
          eliminate set = concat <$> traverse (\p -> if differentHeads p then Nothing else Just $ expandArguments p) set 

          differentHeads :: (LambdaTerm, LambdaTerm) -> Bool
          differentHeads p = False

          expandArguments :: (LambdaTerm, LambdaTerm) -> [(LambdaTerm, LambdaTerm)]
          expandArguments p = [p]

          construct :: [(LambdaTerm, LambdaTerm)] -> Logic Subst
          construct set = return Map.empty
        


match :: (LambdaTerm, LambdaTerm) -> [(LambdaTerm, LambdaTerm)] -> Logic Subst
match pair prev = do
    -- Derive all possible substitutions for the head of the flexible term using the rigid term, use nondeterminism

    -- For each of those substitutions, apply it to the previous set of substitutions to get the new set of pairs for simplifying

    mzero

isRigid :: LambdaTerm -> Bool
isRigid t = walk t Set.empty
    where walk :: LambdaTerm -> Set.Set LambdaVar -> Bool
          walk (Abs (v, _) t _) accum = isRigid t (Set.insert v accum)
          walk (Var v _) accum = Set.member v accum  -- check if it's bound
          walk (ContInt _) accum = True 
          walk (ContBool_) accum = True 
          walk _ _ = False

isFlexible :: LambdaTerm -> Bool
isFlexible t = not $ isRigid t

---
-- Lambda Calculus Interpreter
---

-- | Normalize a term to the end
normalize :: LambdaTerm -> LambdaTerm
normalize t =
    if normal t
        then t
        else normalize $ step t

-- | Take a step towards being normalized
step :: LambdaTerm -> LambdaTerm
-- reduction rules
step (App (Abs bin m _) n _) = subst n (fst bin) m
step (Fst (Pair l _ _) _) = l
step (Snd (Pair _ r _) _) = r
step (Let v e1 e2 t)
    | normal e1 = subst e1 v e2
    | otherwise = Let v (step e1) e2 t
step (App l r t)
    | normal l = App l (step r) t
    | otherwise = App (step l) r t
step (Pair l r t)
    | normal l = Pair l (step r) t
    | otherwise = Pair (step l) r t
step (Fst e t) = Fst (step e) t
step (Snd e t) = Snd (step e) t
step x = x

-- | Check if a term is normal
normal :: LambdaTerm -> Bool
normal ID{} = True
normal UVar{} = True
normal Var{} = True
normal Abs{} = True -- call-by-value
normal (Pair l r _) = normal l && normal r
normal _ = False

-- subst N x M = M[x := N]
-- Substitute all instances of a lambda var x with N in M
subst :: LambdaTerm -> String -> LambdaTerm -> LambdaTerm
subst n x v@(Var y _) = if y == x then n else v
subst n x (App fun arg t) = App (subst n x fun) (subst n x arg) t
subst n x a@(Abs bin@(y, yt) body t)
    | y == x = a
    | y `Set.notMember` fv n = Abs bin (subst n x body) t
    | otherwise = Abs bin' (subst n x body') t
  where
    body' = subst (Var y' yt) y body
    bin' = (y', yt)
    y' = freshVar fvMN y
    fvMN = Set.union (fv body) (fv n)
-- TODO: I need to check let subsitution is correct
subst n x l@(Let y e1 e2 t)
    | y == x = l
    | y `Set.notMember` fv n = Let y e1 (subst n x e2) t
    | otherwise = Let y' e1 (subst n x e2') t
  where
    e2' = subst (Var y' (extract e1)) y e2'
    y' = freshVar fvMN y
    fvMN = Set.union (fv e2) (fv n)
subst n x (Pair l r t) = Pair (subst n x l) (subst n x r) t
subst n x (Fst e t) = Fst (subst n x e) t
subst n x (Snd e t) = Fst (subst n x e) t
subst _ _ t = t

{- | Return a "fresh" name not already in the set.
 Tries x' then x'', etc.
-}
freshVar :: Set.Set LambdaVar -> LambdaVar -> LambdaVar
freshVar s = fresh'
  where
    fresh' n | n `Set.notMember` s = n
    fresh' n = fresh' $ n ++ "'"

-- | Return the free variables in a lambda term
fv :: LambdaTerm -> Set.Set LambdaVar
fv (Var varid _) = Set.singleton varid
fv (Abs bin body _) = Set.difference (fv body) (Set.singleton $ fst bin)
fv (App fun arg _) = Set.union (fv fun) (fv arg)
fv (Let lv _ body _) = Set.difference (fv body) (Set.singleton lv)
fv (Pair l r _) = Set.union (fv l) (fv r)
fv (Fst term _) = fv term
fv (Snd term _) = fv term
fv _ = Set.empty

-- | Extract the type information from the term
extract :: LambdaTerm -> Type
extract (Var _ t) = t
extract (Abs _ _ t) = t
extract (App _ _ t) = t
extract (Let _ _ _ t) = t
extract (Pair _ _ t) = t
extract (Fst _ t) = t
extract (Snd _ t) = t
extract _ = Hole




