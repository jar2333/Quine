module AST
    ( Statement(..),
      Goal(..),
      Term(..),
      Arg(..)
    ) where


type ID = String
type UVar = String
type Var = String

data Statement = Rule ID [UVar] Goal
               | Query (Maybe Int) Goal

data Arg = Param UVar
         | Term Term

data Goal = Disj Goal Goal   
          | Conj Goal Goal
          | Fresh [UVar] Goal
          | Equal Term Term
          | Relation ID [Arg] Goal

data Term = UVar UVar
          | Var Var
          | Abs [Var] Term
          | App Term Term
          | Let Var Term Term
          deriving (Eq, Ord)


showAbs, showApp, showVar, showLet :: Term -> String
showAbs (Abs i e) = "\\" ++ concatMap show i ++ " . " ++ showAbs e
showAbs e = showApp e

showApp (App e1 e2) = showApp e1 ++ " " ++ showVar e2
showApp e = showVar e

showVar (Var i) = i
showVar e = "(" ++ showAbs e ++ ")"

showLet (Let v e1 e2) = "let" ++ v ++ "=" ++ showAbs e1 ++ 
                        "in"  ++ showAbs e2 
showLet e = "(" ++ showAbs e ++ ")"

instance Show Term where
  show e = showAbs e
