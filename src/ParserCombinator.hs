-- ParserCombinator.hs
-- Thư viện bộ kết hợp phân tích cú pháp (Parser Combinator) tự xây dựng

{-# LANGUAGE BangPatterns #-}

module ParserCombinator
  ( ParseError(..)
  , Parser(..)
  , parse
  , satisfy
  , char
  , string
  , spaces
  , spaces1
  , token
  , symbol
  , integer
  , natural
  , identifier
  , restOfLine
  , choice
  , between
  , sepBy
  , sepBy1
  , option
  , eof
  ) where

import Control.Applicative (Alternative(..))
import Data.Char (isDigit, isAlpha, isAlphaNum, isSpace)

-- Trong quá trình chạy, errPos lưu độ dài chuỗi còn lại dưới dạng số âm;
-- hàm parse quy đổi về chỉ mục ký tự tính từ đầu chuỗi.
data ParseError = ParseError
  { errPos :: !Int
  , errMsg :: !String
  } deriving (Eq, Show)

failAt :: String -> String -> ParseError
failAt rest msg = ParseError (negate (length rest)) msg

newtype Parser a = Parser
  { runParser :: String -> Either ParseError (a, String)
  }

instance Functor Parser where
  fmap f (Parser p) = Parser $ \input -> case p input of
    Left err        -> Left err
    Right (a, rest) -> Right (f a, rest)

instance Applicative Parser where
  pure a = Parser $ \input -> Right (a, input)
  Parser pf <*> Parser pa = Parser $ \input -> case pf input of
    Left err        -> Left err
    Right (f, rest) -> case pa rest of
      Left err'         -> Left err'
      Right (a, rest')  -> Right (f a, rest')

instance Monad Parser where
  return = pure
  Parser pa >>= f = Parser $ \input -> case pa input of
    Left err        -> Left err
    Right (a, rest) -> runParser (f a) rest

instance Alternative Parser where
  empty = Parser $ \input -> Left (failAt input "Cu phap khong hop le")
  -- Khi cả hai nhánh đều thất bại, ưu tiên nhánh tiến xa hơn trong chuỗi đầu vào
  Parser p1 <|> Parser p2 = Parser $ \input -> case p1 input of
    Right res -> Right res
    Left e1   -> case p2 input of
      Right res -> Right res
      Left e2   -> Left (if errPos e1 > errPos e2 then e1 else e2)

parse :: Parser a -> String -> Either ParseError a
parse (Parser p) input = case p input of
  Left err      -> Left err { errPos = length input + errPos err }
  Right (a, "") -> Right a
  Right (a, rest) ->
    let !trimmed = dropWhile isSpace rest
    in if null trimmed
         then Right a
         else Left (ParseError (length input - length rest) ("Ky tu du thua: " ++ take 20 rest))

satisfy :: (Char -> Bool) -> Parser Char
satisfy predicate = Parser $ \input -> case input of
  [] -> Left (failAt input "Ket thuc chuoi bat ngo")
  (c:cs)
    | predicate c -> Right (c, cs)
    | otherwise   -> Left (failAt input ("Ky tu khong mong doi: '" ++ [c] ++ "'"))

char :: Char -> Parser Char
char expected = satisfy (== expected)

string :: String -> Parser String
string []     = pure []
string (c:cs) = do
  _ <- char c
  _ <- string cs
  return (c:cs)

spaces :: Parser String
spaces = many (satisfy isSpace)

spaces1 :: Parser String
spaces1 = some (satisfy isSpace)

token :: Parser a -> Parser a
token p = do
  a <- p
  _ <- spaces
  return a

symbol :: String -> Parser String
symbol str = token (string str)

natural :: Parser Int
natural = do
  digits <- some (satisfy isDigit)
  return (foldl (\acc d -> acc * 10 + (fromEnum d - fromEnum '0')) 0 digits)

integer :: Parser Int
integer = (char '-' >> fmap negate natural) <|> (char '+' >> natural) <|> natural

identifier :: Parser String
identifier = token $ do
  firstChar <- satisfy (\c -> isAlpha c || c == '_')
  restChars <- many (satisfy (\c -> isAlphaNum c || c `elem` "_-@."))
  return (firstChar : restChars)

restOfLine :: Parser String
restOfLine = Parser $ \input ->
  let (line, rest) = break (\c -> c == '\n' || c == '\r') input
      rest' = case rest of
        ('\r':'\n':cs) -> cs
        ('\n':cs)      -> cs
        ('\r':cs)      -> cs
        cs             -> cs
  in Right (line, rest')

choice :: [Parser a] -> Parser a
choice = foldr (<|>) empty

between :: Parser open -> Parser close -> Parser a -> Parser a
between open close p = do
  _ <- open
  a <- p
  _ <- close
  return a

sepBy :: Parser a -> Parser sep -> Parser [a]
sepBy p sep = sepBy1 p sep <|> pure []

sepBy1 :: Parser a -> Parser sep -> Parser [a]
sepBy1 p sep = do
  a <- p
  as <- many (sep >> p)
  return (a : as)

option :: a -> Parser a -> Parser a
option def p = p <|> pure def

eof :: Parser ()
eof = Parser $ \input ->
  let trimmed = dropWhile isSpace input
  in if null trimmed
       then Right ((), "")
       else Left (failAt trimmed ("Chua doc het chuoi, con lai: " ++ take 20 trimmed))
