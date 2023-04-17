module Term
    ( Term(..)
    , Var
    , pretty
    ) where


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