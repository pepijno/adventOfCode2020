module Parser
  ( Parser,
    parse,
    unsafeParse,
    char,
    anyChar,
    string,
    stringLiteral,
    integer,
    natural,
    whiteSpace,
    letters,
    (<|>),
    (<|),
    many,
    many1,
    munch,
    munch1,
    between,
    choice,
    optional,
    withDefault,
    sepBy,
    sepBy1,
    manyUntil,
    eof,
  )
where

import Control.Applicative hiding (many, optional)
import Control.Monad
import Data.Char
import Data.Function
import Data.List

newtype Parser a = Parser {parse :: String -> [(a, String)]}

unsafeParse :: (Show a) => Parser a -> String -> a
unsafeParse p s = case parse p s of
  [] -> error ("Unable to parse \"" ++ s ++ "\".")
  r -> fst $ minimumBy (compare `on` snd) r

instance Functor Parser where
  fmap f (Parser p) = Parser $ \input -> do
    (x, rest) <- p input
    return (f x, rest)

instance Applicative Parser where
  pure x = Parser $ \input -> [(x, input)]
  (Parser p) <*> (Parser q) = Parser $ \input -> do
    (f, rest) <- p input
    (x, rest') <- q rest
    return (f x, rest')

instance Alternative Parser where
  empty = Parser $ const []
  (Parser p) <|> (Parser q) = Parser $ \input -> p input <|> q input

instance Monad Parser where
  (Parser p) >>= f = Parser $ \input -> do
    (x, rest) <- p input
    (x', rest') <- parse (f x) rest
    return (x', rest')

(<|) :: Parser a -> Parser a -> Parser a
(Parser p) <| (Parser q) = Parser $ \input -> case p input of
  [] -> q input
  a -> a

anyChar :: Parser Char
anyChar = Parser $ \case
  [] -> []
  (c : cs) -> [(c, cs)]

satisfy :: (Char -> Bool) -> Parser Char
satisfy f = Parser $ \case
  [] -> []
  (c : cs)
    | f c -> [(c, cs)]
    | otherwise -> []

munch :: (Char -> Bool) -> Parser String
munch f = Parser $ \input -> do
  return $ span f input

munch1 :: (Char -> Bool) -> Parser String
munch1 f = Parser $ \input ->
  let (p, rest) = span f input
   in case p of
        [] -> []
        _ -> return (p, rest)

many :: Parser a -> Parser [a]
many p = return [] <|> many1 p

many1 :: Parser a -> Parser [a]
many1 p = liftM2 (:) p (many p)

choice :: [Parser a] -> Parser a
choice [] = empty
choice [p] = p
choice (x : xs) = x <|> choice xs

between :: Parser a -> Parser b -> Parser c -> Parser c
between begin end p = begin *> p <* end

optional :: Parser a -> Parser ()
optional p = void p <|> return ()

withDefault :: Parser a -> a -> Parser a
withDefault p def = p <| pure def

sepBy :: Parser a -> Parser b -> Parser [a]
sepBy p sep = sepBy1 p sep <|> return []

sepBy1 :: Parser a -> Parser b -> Parser [a]
sepBy1 p sep = liftM2 (:) p (many (sep >> p))

manyUntil :: Parser a -> Parser b -> Parser [a]
manyUntil p un = scan
  where
    scan = (un >> return []) <| liftM2 (:) p scan

eof :: Parser ()
eof = Parser $ \case
  [] -> return ((), [])
  _ -> []

char :: Char -> Parser Char
char c = satisfy (== c)

string :: String -> Parser String
string = traverse char

stringLiteral :: Parser String
stringLiteral = munch1 isAlphaNum

natural :: Parser Int
natural = read <$> munch1 isDigit

integer :: Parser Int
integer = f <$> char '-' `withDefault` '+' <*> munch1 isDigit
  where
    f '-' s = (-1) * read s
    f _ s = read s

whiteSpace :: Parser Char
whiteSpace = satisfy isSpace

letters :: Parser String
letters = munch1 isAlpha
