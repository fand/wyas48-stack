module Types (
  LispVal(..),
  LispError(..),
  ThrowsError,
  trapError,
  extractValue,
  Env,
  nullEnv,
  IOThrowsError,
  liftThrows,
  runIOThrows,
  getVar,
  setVar,
  defineVar,
  bindVars,
  makeNormalFunc,
  makeVarargs
) where

import           Control.Monad.Error
import           Data.Complex
import           Data.IORef
import           Data.Maybe
import           Text.ParserCombinators.Parsec

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
             | PrimitiveFunc ([LispVal] -> ThrowsError LispVal)
             | Func {
               params  :: [String],
               vararg  :: Maybe String,
               body    :: [LispVal],
               closure :: Env }

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
showVal (PrimitiveFunc _) = "<primitive>"
showVal Func { params = args, vararg = varargs, body = body, closure = env } =
  "(lambda (" ++ unwords (map show args) ++ (
    case varargs of
      Nothing  -> ""
      Just arg -> " . " ++arg
  ) ++ ") ...)"

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

instance Show LispVal where show = showVal
instance Eq LispVal where
  (==) a b = show a == show b


data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String

showError :: LispError -> String
showError (UnboundVar message varname) = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func) = message ++ ": " ++ show func
showError (NumArgs expected found) =
  "Expected " ++ show expected
  ++ " args; found values " ++ unwordsList found
showError (TypeMismatch expected found) =
  "Invalid type: expected " ++ expected
  ++ ", found " ++ show found
showError (Parser parseErr) = "Parse error at " ++ show parseErr
showError (Default err) = "LispError: " ++ show err

instance Show LispError where show = showError
instance Error LispError where
  noMsg = Default "An error has occurred"
  strMsg = Default

type ThrowsError = Either LispError

trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

type Env = IORef [(String, IORef LispVal)]

nullEnv :: IO Env
nullEnv = newIORef []

type IOThrowsError = ErrorT LispError IO

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err)  = throwError err
liftThrows (Right val) = return val

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = fmap extractValue (runErrorT (trapError action))

isBound :: Env -> String -> IO Bool
isBound envRef var = isJust . lookup var <$> readIORef envRef

-- Env, 変数名を受け取り、IORefから変数を取り出す
getVar :: Env -> String -> IOThrowsError LispVal
getVar envRef var = do
  env <- liftIO $ readIORef envRef
  maybe (throwError $ UnboundVar "Getting an unbound variable: " var)
    (liftIO . readIORef)
    (lookup var env)

setVar :: Env -> String -> LispVal -> IOThrowsError LispVal
setVar envRef var value = do
  env <- liftIO $ readIORef envRef
  maybe (throwError $ UnboundVar "Setting an unbound variable: " var)
    (liftIO . flip writeIORef value)
    (lookup var env)
  return value

-- 環境、変数名、値、結果
defineVar :: Env -> String -> LispVal -> IOThrowsError LispVal
defineVar envRef var value = do
  alreadyDefined <- liftIO $ isBound envRef var
  if alreadyDefined
    then setVar envRef var value >> return value
    else liftIO $ do
      valueRef <- newIORef value
      env <- readIORef envRef
      writeIORef envRef ((var, valueRef) : env)
      return value

-- 複数の変数を一度に束縛する
bindVars :: Env -> [(String, LispVal)] -> IO Env
bindVars envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef
  where
    extendEnv bindings env = fmap (++ env) (mapM addBinding bindings)
    addBinding (var, value) = do
      ref <- newIORef value
      return (var, ref)

makeFunc :: Maybe String -> Env -> [LispVal] -> [LispVal] -> IOThrowsError LispVal
makeFunc varargs env params body = return $ Func (fmap showVal params) varargs body env
makeNormalFunc = makeFunc Nothing
makeVarargs = makeFunc . Just . showVal
