module Interpreter 
    ( execute
    , executeRepl
    , runner
    , Option(..)
    ) where

import Control.Monad
import Control.Monad.IO.Class
import System.Environment

import Kanren
import LambdaTerm
import Parse (tryParse)
import Translator (translateStatement)


data Option = PrettyPrint | Execute

execute :: Option -> IO ()
execute opt = do
  args <- getArgs
  case args of
    []  -> error "NO FILENAME PROVIDED!"
    f:_ -> do
      content <- readFile f
      evalKanrenT (runner opt content) initialEnv
      

executeRepl :: Option -> IO ()
executeRepl opt = do
  putStrLn "QUINE 0.0.1"
  evalKanrenT (repl opt) initialEnv

repl :: Option -> KanrenT LambdaTerm IO ()
repl opt = do
  liftIO $ putStr "> "
  line <- liftIO getLine
  unless (line == ":q") $ do
    runLine opt line
    liftIO $ putStr "\n"
    repl opt

runner :: Option -> String -> KanrenT LambdaTerm IO ()
runner opt contents = mapM_ (runLine opt) $ lines contents

runLine :: Option -> String -> KanrenT LambdaTerm IO ()
runLine opt line = case tryParse line of
    Left err -> liftIO $ putStrLn $ "ERROR: " ++ err
    Right s  -> case opt of
      PrettyPrint -> liftIO $ print s 
      Execute -> translateStatement s 
