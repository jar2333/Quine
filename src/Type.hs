module Type (
    Type (..),
) where

data Type
    = UserDefined String
    | Arrow Type Type
    | Prod Type Type
    | Sum Type Type
    | List Type
    | Int
    | Bool
    | Hole
    deriving (Eq)

instance Show Type where
    show (UserDefined s) = s
    show Int = "Int"
    show Bool = "Bool"
    show (Arrow t1 t2) = show t1 ++ " -> " ++ show t2
    show (Prod t1 t2) = show t1 ++ " * " ++ show t2
    show (Sum t1 t2) = show t1 ++ " + " ++ show t2
    show (List t) = "[" ++ show t ++ "]"
    show Hole = "_"
