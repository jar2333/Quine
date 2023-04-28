module Interpreter 
    ( execute
    , executeRepl
    ) where

import Control.Monad
import Control.Monad.IO.Class
import System.Environment

import qualified Data.Set as Set
import Kanren
import LambdaTerm
import Parse (tryParse)
import Translator (translateStatement)
import Type

execute :: IO ()
execute = do
  args <- getArgs
  case args of
    [] -> error "NO FILENAME PROVIDED!"
    _  -> do
      content <- readFile (args !! 0)
      evalKanrenT (runner content) initialEnv

executeRepl :: IO ()
executeRepl = do
  putStrLn "QUINE 0.0.1"
  evalKanrenT repl initialEnv

repl :: KanrenT LambdaTerm IO ()
repl = do
  line <- liftIO getLine
  unless (line == ":q") $ do
    runLine line
    repl

runner :: String -> KanrenT LambdaTerm IO ()
runner contents = mapM_ runLine $ lines contents

runLine :: String -> KanrenT LambdaTerm IO ()
runLine line = case tryParse line of
    Left err -> liftIO $ putStrLn $ "ERROR: " ++ err
    Right s  -> translateStatement s 
