module Term
    ( Term(..)
    , Var
    , Bind
    , pretty
    , substID
    ) where

import Data.Map as Map ( lookup, Map )
import Data.Char ( toUpper, toLower )

type Var = Int

data Term = ID String | Var Var | Symbol String | Bool Bool | Nil | Pair Term Term deriving (Eq, Show)

pretty :: Term -> String
pretty (Pair t1 t2) = "(" ++ pretty t1 ++ ", " ++ pretty t2 ++ ")"
pretty (ID i) = map toUpper i
pretty (Var v) = show v
pretty (Symbol s) = s
pretty (Bool b) = map toLower $ show b
pretty Nil = "()"

type Bind  = Map.Map String Var

substID :: Term -> Bind -> Term
substID (ID i) b = maybe (error $ i ++ " NOT FOUND") Var (Map.lookup i b)
substID (Pair t1 t2) b = Pair (substID t1 b) (substID t2 b)
substID x _ = x

