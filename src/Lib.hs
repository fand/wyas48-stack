module Lib (
  readExpr,
  eval,
  LispVal(..)
) where

import           Control.Monad
import           Data.Complex
import           Data.Ratio
import           Numeric
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

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

escapedChar :: Parser Char
escapedChar = char '\\' >> oneOf "\\\"nrt"

parseString :: Parser LispVal
parseString = do
  char '"'
  x <- many $ escapedChar <|> noneOf "\"\\"
  char '"'
  return $ String x

parseBool :: Parser LispVal
parseBool = do
  string "#"
  x <- oneOf "tf"
  return $ case x of
    't' -> Bool True
    'f' -> Bool False

parseAtom :: Parser LispVal
parseAtom = do
  first <- letter <|> symbol
  rest <- many (letter <|> digit <|> symbol)
  return $ Atom $ first:rest

parseHex :: Parser LispVal
parseHex = do
  try $ string "#x"
  x <- many1 hexDigit
  return $ Number $ fst $ head $ readHex x

parseDigits :: Parser LispVal
parseDigits = do
  try $ char '#'
  second <- oneOf "odb"
  rest <- many1 digit
  return $ Number $ case second of
    'o' -> fst $ head $ readOct rest
    'd' -> read rest
    'b' -> parseBin 0 rest

parseBin :: Integer -> String -> Integer
parseBin acc ""     = acc
parseBin acc (x:xs) = parseBin (acc * 2 + (if x == '0' then 0 else 1)) xs

parseDecimal :: Parser LispVal
parseDecimal = (Number . read) <$> many1 digit

parseNumber :: Parser LispVal
parseNumber = try $ parseHex <|> parseDecimal <|> parseDigits
-- tryを外すと、 #t を解釈できずに落ちるので注意

parseCharacter :: Parser LispVal
parseCharacter = do
  try $ string "#\\"
  value <- try (string "newline" <|> string "space")
    <|> do
      x <- anyChar
      notFollowedBy alphaNum
      return [x]
  return $ Character $ case value of
    "newline" -> '\n'
    "space"   -> ' '
    _         -> head value

parseFloat :: Parser LispVal
parseFloat = do
  x <- many1 digit
  char '.'
  y <- many1 digit
  return $ Float $ fst . head $ readFloat (x ++ "." ++ y)

parseRatio :: Parser LispVal
parseRatio = do
  x <- many1 digit
  char '/'
  y <- many1 digit
  return $ Ratio $ read x % read y

parseComplex :: Parser LispVal
parseComplex = do
  x <- try parseFloat <|> parseDecimal
  char '+'
  y <- try parseFloat <|> parseDecimal
  char 'i'
  return $ Complex (toDouble x :+ toDouble y)

toDouble :: LispVal -> Double
toDouble(Float f)  = f
toDouble(Number n) = fromIntegral n

parseList ::Parser LispVal
parseList = List <$> sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
  head <- endBy parseExpr spaces
  tail <- char '.' >> spaces >> parseExpr
  return $ DottedList head tail

parseQuoted :: Parser LispVal
parseQuoted = do
  char '\''
  x <- parseExpr
  return $ List [Atom "quote", x]

parseExpr :: Parser LispVal
parseExpr = parseAtom
  <|> parseString
  <|> try parseComplex
  <|> try parseFloat
  <|> try parseRatio
  <|> try parseNumber
  <|> parseQuoted
  <|> do
    char '('
    x <- try parseList <|> parseDottedList
    char ')'
    return x
  <|> try parseBool
  <|> try parseCharacter


-- Evaluator
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

eval ::LispVal -> LispVal
eval val@(String _)             = val
eval val@(Number _)             = val
eval val@(Bool _)               = val
eval (List [Atom "quote", val]) = val
eval (List (Atom func : args))  = apply func $ map eval args

apply :: String -> [LispVal] -> LispVal
apply func args = maybe (Bool False) ($ args) $ lookup func primitives

primitives :: [(String, [LispVal] -> LispVal)]
primitives = [
  ("+", numericBinop (+)),
  ("-", numericBinop (-)),
  ("*", numericBinop (*)),
  ("/", numericBinop div),
  ("mod", numericBinop mod),
  ("quotient", numericBinop quot),
  ("remainder", numericBinop rem),
  ("symbol?", \(x:xs) -> case x of
    Atom x -> Bool True
    _      -> Bool False
  ),
  ("string?", \(x:xs) -> case x of
    String x -> Bool True
    _        -> Bool False
  ),
  ("number?", \(x:xs) -> case x of
    Number x -> Bool True
    _        -> Bool False
  ),
  ("boolean?", \(x:xs) -> case x of
    Bool x -> Bool True
    _      -> Bool False
  )]

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericBinop op params = Number $ foldl1 op $ map unpackNum params

unpackNum :: LispVal -> Integer
unpackNum (Number n) = n
unpackNum (String n) = let parsed = reads n in
  if null parsed
    then 0
    else fst $ head parsed
unpackNum (List [n])     = unpackNum n
unpackNum _ = 0

readExpr :: String -> LispVal
readExpr input = case parse parseExpr "lisp" input of
  Left err  -> String $ "No match: " ++ show err
  Right val -> val
