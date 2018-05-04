module LispVal (
  LispVal(..)
) where

import           Data.Complex

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | Float Double
             | Ratio Rational
             | Complex (Complex Double)
             | String String
             | Bool Bool
             | Character Char

showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name)       = name
showVal (Number contents) = show contents
showVal (Bool True)       = "#t"
showVal (Bool False)      = "#f"
showVal (List contents)   = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"
showVal (Character content) = case content of
  ' '  -> "#\\space"
  '\n' -> "#\\newline"
  _    -> "#\\" ++ [content]
showVal (Float x) = show x
showVal (Ratio x) = show x
showVal (Complex x) = show x

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

instance Show LispVal where show = showVal

instance Eq LispVal where
  (==) a b = show a == show b
