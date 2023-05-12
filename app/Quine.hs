module Main (main) where

import Interpreter

import qualified Translator as Trans
import Type as T
import LambdaTerm as L
import qualified AST as A



main :: IO ()
main = do
    let x = A.ConstBool True
        y = A.Var "y"
        z = A.Abs ("y", T.UserDefined "Int") x
        z' = A.App z y
        lz' = Trans.translateTerm z'
    print $ step lz'
