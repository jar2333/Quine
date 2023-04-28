module Semant (typeof) where

import AST as A
import Type as T

type Ctx = [(String, T.Type)]

typeof :: Ctx -> A.Term -> Maybe T.Type
typeof ctx (A.Var v) = lookup v ctx
typeof ctx (A.UVar uv) = lookup uv ctx
typeof ctx (A.App l r) = do
    T.Arrow t1 t2 <- typeof ctx l
    t <- typeof ctx r
    if t1 == t
        then return t2
        else Nothing
typeof ctx (A.Abs (v, t) e) = T.Arrow t <$> typeof ((v, t) : ctx) e
typeof ctx (A.Pair l r) = do
    t1 <- typeof ctx l
    t2 <- typeof ctx r
    return $ T.Prod t1 t2
typeof ctx (A.Fst e) = do
    Prod t _ <- typeof ctx e
    return t
typeof ctx (A.Snd e) = do
    Prod _ t <- typeof ctx e
    return t
typeof ctx (A.Let v e1 e2) =
    case t1 of
        Just t1' -> typeof ((v, t1') : ctx) e2
        _ -> Nothing
  where
    t1 = typeof ctx e1
