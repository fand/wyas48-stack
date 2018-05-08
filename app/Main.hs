module Main where

import           Env
import           Eval
import           Read
import           System.Environment
import           System.IO
import           Types

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do
  result <- prompt
  if pred result
    then return ()
    else action result >> until_ pred prompt action

primitiveBindings :: IO Env
primitiveBindings = nullEnv >>= flip bindVars (map makePrimitiveFunc primitives)
  where makePrimitiveFunc (var, func) = (var, PrimitiveFunc func)

runRepl :: IO ()
runRepl = primitiveBindings >>= until_ (== "quit") (readPrompt "Lisp>>> ") . evalAndPrint

-- 引数を、loadするファイル
-- $ stack exec wyas48-stack-exe "loadしたいlispプログラムのファイル名"  <lispプログラムの引数>
runOne :: [String] -> IO ()
runOne args = do
  env <- primitiveBindings >>= flip bindVars [("args", List $ map String $ drop 1 args)]
  runIOThrows (show <$> eval env (List [Atom "load", String (head args)]))
    >>= hPutStrLn stderr

main :: IO ()
main = do
  args <- getArgs
  if null args
    then runRepl
    else runOne args
