module Main where
import           Control.Monad
import           Numeric
import           System.Environment
import           Text.ParserCombinators.Parsec

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool
             | Character Char
             deriving Show

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

escapedChar :: Parser Char
escapedChar = char '\\' >> oneOf "\\\"nrt"

parseString :: Parser LispVal
parseString = do
  char '"'
  x <- many $ noneOf "\"" <|> escapedChar
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
parseBin acc (x:xs) = acc * 2 + parseBin (if x == '0' then 0 else 1) xs

parseDecimal :: Parser LispVal
parseDecimal = (Number . read) <$> many1 digit

parseNumber :: Parser LispVal
parseNumber = try $ parseHex <|> parseDecimal <|> parseDigits
-- tryを外すと、 #t を解釈できずに落ちるので注意

parseCharacter :: Parser LispVal
parseCharacter = do
  try $string "#\\"
  value <- try (string "newline" <|> string "space")
    <|> do
      x <- anyChar
      notFollowedBy alphaNum
      return [x]
  return $ Character $ case value of
    "newline" -> '\n'
    "space"   -> ' '
    _         -> head value

parseExpr :: Parser LispVal
parseExpr = parseAtom
  <|> parseString
  <|> try parseNumber
  <|> try parseBool
  <|> try parseCharacter

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
  Left err  -> "No match: " ++ show err
  Right val -> "Found value: " ++ show val

main :: IO ()
main = do
  args <- getArgs
  putStrLn $ readExpr $ head args
