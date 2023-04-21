module AST
    ( Statement(..),
      Goal(..),
      Term(..),
      Arg(..)
    ) where

import Type ( Type )

type ID = String
type UVar = String
type Var = String
type Binder = (Var, Type)

data Statement = Rule ID [UVar] Goal
               | Query (Maybe Int) [UVar] Goal

data Arg = Param UVar
         | Term Term

data Goal = Disj Goal Goal   
          | Conj Goal Goal
          | Fresh [UVar] Goal
          | Equal Term Term
          | Relation ID [Arg]

data Term = UVar UVar
          | Var Var
          | Abs Binder Term
          | App Term Term
          | Let Var Term Term
          deriving (Eq)


instance Show Statement where
  show (Rule i vs g) = i ++ concatMap show vs ++ " = " ++ show g
  show (Query _ vs g) = concatMap show vs ++ show g

instance Show Goal where
  show (Disj g1 g2) = "(" ++ show g1 ++ " || " ++ show g2 ++ ")"
  show (Conj g1 g2) = "(" ++ show g1 ++ " && " ++ show g2 ++ ")"
  show (Fresh vs g) = concatMap show vs ++ show g
  show (Equal t1 t2) = "(" ++ show t1 ++ " == " ++ show t2 ++ ")"
  show (Relation i args) = i ++ concatMap show args

instance Show Arg where
  show (Param uvar) = uvar
  show (Term term) = show term

instance Show Term where
  show (UVar i) = i
  show (Var i) = i
  show (Abs (v, t) e) = "\\" ++ 
                        "("  ++ show v ++ " : " ++ show t ++ ")"
                             ++ " -> " ++ show e
  show (App e1 e2) = show e1 ++ show e2
  show (Let v e1 e2) = "let" ++ v ++ "=" ++ show e1 ++ 
                        "in" ++ show e2 
