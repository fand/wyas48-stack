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
eval (List [Atom "if", pred, conseq, alt]) = do
  cond <- eval pred
  case cond of
    Bool False -> eval alt
    _          -> eval conseq
eval (List (Atom func : args)) = mapM eval args >>= apply func

eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe
  (throwError $ NotFunction "Unrecognized primitive function args" func)
  ($ args)
  (lookup func primitives)

isSymbol :: [LispVal] -> ThrowsError LispVal
isSymbol args@(x:y:xs) = throwError $ NumArgs 1 args
isSymbol [Atom x]      = Right (Bool True)
isSymbol _             = Right (Bool False)

isString :: [LispVal] -> ThrowsError LispVal
isString args@(x:y:xs) = throwError $ NumArgs 1 args
isString [String x]    = Right (Bool True)
isString _             = Right (Bool False)

isNumber :: [LispVal] -> ThrowsError LispVal

isNumber args@(x:y:xs) = throwError $ NumArgs 1 args
isNumber [Number x]    = Right (Bool True)
isNumber _             = Right (Bool False)

isBoolean :: [LispVal] -> ThrowsError LispVal
isBoolean args@(x:y:xs) = throwError $ NumArgs 1 args
isBoolean [Bool x]      = Right (Bool True)
isBoolean _             = Right (Bool False)

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
  ("boolean?", isBoolean),
  ("=", numBoolBinop (==)),
  ("<", numBoolBinop (<)),
  (">", numBoolBinop (>)),
  ("/=", numBoolBinop (/=)),
  (">=", numBoolBinop (>=)),
  ("<=", numBoolBinop (<=)),
  ("&&", boolBoolBinop (&&)),
  ("||", boolBoolBinop (||)),
  ("string=?", strBoolBinop (==)),
  ("string<?", strBoolBinop (<)),
  ("string>?", strBoolBinop (>)),
  ("string<=?", strBoolBinop (<=)),
  ("string>=?", strBoolBinop (>=))
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

boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinop unpacker op args = if length args /= 2
  then throwError $ NumArgs 2 args
  else do
    left <- unpacker $ head args
    right <- unpacker $ args !! 1
    return $ Bool $ left `op` right

numBoolBinop = boolBinop unpackNum
strBoolBinop = boolBinop unpackStr
boolBoolBinop = boolBinop unpackBool

unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr (Number s) = return $ show s
unpackStr (Bool s)   = return $ show s
unpackStr notString  = throwError $ TypeMismatch "string" notString

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool  = throwError $ TypeMismatch "boolean" notBool
