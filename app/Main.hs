module Main where

import           Eval
import           Read
import           System.Environment
import           Types

main :: IO ()
-- main = getArgs >>= print . eval . readExpr . head
main = do
  args <- getArgs
  evaled <- return $ fmap show $ readExpr (head args) >>= eval
  putStrLn $ extractValue $ trapError evaled
