module Main where

import           Eval
import           Read
import           System.Environment

main :: IO ()
main = getArgs >>= print . eval . readExpr . head
