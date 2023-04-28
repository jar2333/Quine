module Interpreter (
    execute,
) where

import Control.Monad
import Control.Monad.IO.Class

import qualified Data.Set as Set
import Kanren
import LambdaTerm
import Parse (tryParse)
import Translator (translateStatement)
import Type

execute :: IO ()
execute = do
    putStrLn "QUINE 0.0.1"
    evalKanrenT repl initialEnv

repl :: KanrenT LambdaTerm IO ()
repl = do
    line <- liftIO getLine
    unless (line == ":q") $ do
        case tryParse line of
            Left err -> liftIO $ putStrLn $ "ERROR: " ++ err
            Right s -> translateStatement s
        repl

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

{- | Return a "fresh" name not already in the set.
 Tries x' then x'', etc.
-}
freshVar :: Set.Set LambdaVar -> LambdaVar -> LambdaVar
freshVar s = fresh'
  where
    fresh' n | n `Set.notMember` s = n
    fresh' n = fresh' $ n ++ "'"

-- subst N x M = M[x := N]
-- Substitute all instances of a lambda var x with N in M
substitute :: LambdaTerm -> String -> LambdaTerm -> LambdaTerm
substitute n x v@(Var y _) = if y == x then n else v
substitute n x (App fun arg t) = App (substitute n x fun) (substitute n x arg) t
substitute n x a@(Abs bin@(y, yt) body t)
    | y == x = a
    | y `Set.notMember` fv n = Abs bin (substitute n x body) t
    | otherwise = Abs bin' (substitute n x body') t
  where
    body' = substitute (Var y' yt) y body
    bin' = (y', yt)
    y' = freshVar fvMN y
    fvMN = Set.union (fv body) (fv n)
-- TODO: I need to check let subsitution is correct
substitute n x l@(Let y e1 e2 t)
    | y == x = l
    | y `Set.notMember` fv n = Let y e1 (substitute n x e2) t
    | otherwise = Let y' e1 (substitute n x e2') t
  where
    e2' = substitute (Var y' (extract e1)) y e2'
    y' = freshVar fvMN y
    fvMN = Set.union (fv e2) (fv n)
substitute n x (Pair l r t) = Pair (substitute n x l) (substitute n x r) t
substitute n x (Fst e t) = Fst (substitute n x e) t
substitute n x (Snd e t) = Fst (substitute n x e) t
substitute _ _ t = t

-- | Check if a term is normal
normal :: LambdaTerm -> Bool
normal ID{} = True
normal UVar{} = True
normal Var{} = True
normal Abs{} = True -- call-by-value
normal (Pair l r _) = normal l && normal r
normal _ = False

-- | Take a step towards being normalized
step :: LambdaTerm -> LambdaTerm
-- reduction rules
step (App (Abs bin m _) n _) = substitute n (fst bin) m
step (Fst (Pair l _ _) _) = l
step (Snd (Pair _ r _) _) = r
step (Let v e1 e2 t)
    | normal e1 = substitute e1 v e2
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

-- | Normalize a term to the end
normalize :: LambdaTerm -> LambdaTerm
normalize t =
    if normal t
        then t
        else normalize $ step t
