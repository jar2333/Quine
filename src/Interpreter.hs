module Interpreter 
    ( execute
    ) where

import Control.Monad
import Control.Monad.IO.Class

import Parse
import Kanren
import Translator
import LambdaTerm

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
        Right s  -> translateStatement s
    repl