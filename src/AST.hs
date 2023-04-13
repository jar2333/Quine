module AST
    ( Statement(..),
      Goal(..),
      Term(..),
      Arg(..)
    ) where


type ID = String
type UVar = String

data Statement = Rule ID [UVar] Goal
               | Query (Maybe Int) Goal

data Arg = Param UVar
         | Term Term

data Goal = Disj Goal Goal   
          | Conj Goal Goal
          | Fresh [UVar] Goal
          | Equal Term Term
          | Relation ID [Arg] Goal

type Var = String

data Term = UVar UVar
          | Abs [Var] Term
          | Var Var
          | Apply Term Term
          | Let Var Term Term