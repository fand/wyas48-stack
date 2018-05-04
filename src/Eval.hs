module Eval (
  eval
) where

import           Control.Monad.Error
import           Types

eval :: LispVal -> ThrowsError LispVal
eval val@(String _) = return val
eval val@(Number _) = return val
eval val@(Bool _) = return val
eval (List [Atom "quote", val]) = return val
eval (List (Atom func : args)) = mapM eval args >>= apply func
eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe
  (throwError $ NotFunction "Unrecognized primitive function args" func)
  ($ args)
  (lookup func primitives)

isSymbol :: [LispVal] -> ThrowsError LispVal
isSymbol (x:y:xs) = throwError (Default "symbol? can't receive list")
isSymbol [Atom x] = Right (Bool True)
isSymbol _        = Right (Bool False)

isString :: [LispVal] -> ThrowsError LispVal
isString (x:y:xs)   = throwError (Default "string? can't receive list")
isString [String x] = Right (Bool True)
isString _          = Right (Bool False)

isNumber :: [LispVal] -> ThrowsError LispVal
isNumber (x:y:xs)   = throwError (Default "number? can't receive list")
isNumber [Number x] = Right (Bool True)
isNumber _          = Right (Bool False)

isBoolean :: [LispVal] -> ThrowsError LispVal
isBoolean (x:y:xs) = throwError (Default "boolean? can't receive list")
isBoolean [Bool x] = Right (Bool True)
isBoolean _        = Right (Bool False)

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [
  ("+", numericBinop (+)),
  ("-", numericBinop (-)),
  ("*", numericBinop (*)),
  ("/", numericBinop div),
  ("mod", numericBinop mod),
  ("quotient", numericBinop quot),
  ("remainder", numericBinop rem),
  ("symbol?", isSymbol),
  ("string?", isString),
  ("number?", isNumber),
  ("boolean?", isBoolean)
  ]

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params = fmap (Number . foldl1 op) (mapM unpackNum params)

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (String n) = let parsed = reads n in
  if null parsed
    then throwError $ TypeMismatch "number" $ String n
    else return $ fst $ head parsed
unpackNum (List [n]) = unpackNum n
unpackNum notNum = throwError $ TypeMismatch "number" notNum
