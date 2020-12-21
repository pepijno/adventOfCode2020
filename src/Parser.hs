{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RankNTypes #-}

module Parser
  ( Parser,
    parse,
    unsafeParse,
    char,
    anyChar,
    string,
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

import Control.Applicative hiding (many, optional, some)
import Control.Monad
import Data.Char

data P a
  = Get (Char -> P a)
  | Look (String -> P a)
  | Fail
  | Result a (P a)
  | Final [(a, String)]
  deriving (Functor)

instance Applicative P where
  pure x = Result x Fail
  (<*>) = ap

instance Monad P where
  (Get f) >>= g = Get $ \h -> f h >>= g
  (Look f) >>= g = Look $ \h -> f h >>= g
  Fail >>= _ = Fail
  (Result a p) >>= g = g a <|> (p >>= g)
  (Final r) >>= g = final [ys' | (x, s) <- r, ys' <- run (g x) s]

instance Alternative P where
  empty = Fail

  (Get f1) <|> (Get f2) = Get $ \f -> f1 f <|> f2 f
  (Result a p) <|> p' = Result a (p <|> p')
  p <|> (Result a p') = Result a (p <|> p')
  Fail <|> p = p
  p <|> Fail = p
  (Final r) <|> (Final r') = Final (r ++ r')
  (Final r) <|> (Look f) = Look $ \g -> Final $ r ++ run (f g) g
  (Final r) <|> p = Look $ \g -> Final $ r ++ run p g
  (Look f) <|> (Final r) = Look $ \g -> Final $ run (f g) g ++ r
  p <|> (Final r) = Look $ \g -> Final $ run p g ++ r
  (Look f1) <|> (Look f2) = Look $ \g -> f1 g <|> f2 g
  (Look f) <|> p = Look $ \g -> f g <|> p
  p <|> (Look f) = Look $ \g -> p <|> f g

newtype Parser a = Parser (forall b. (a -> P b) -> P b)

instance Functor Parser where
  fmap f (Parser p) = Parser $ \g -> p (g . f)

instance Applicative Parser where
  pure x = Parser $ \g -> g x
  (<*>) = ap

instance Alternative Parser where
  empty = pfail
  (Parser f) <|> (Parser g) = Parser (\h -> f h <|> g h)

instance Monad Parser where
  (Parser p) >>= f = Parser $ \g ->
    p
      ( \a ->
          let (Parser p') = f a
           in p' g
      )

final :: [(a, String)] -> P a
final [] = Fail
final xs = Final xs

get :: Parser Char
get = Parser Get

look :: Parser String
look = Parser Look

pfail :: Parser a
pfail = Parser $ \_ -> Fail

run :: P a -> String -> [(a, String)]
run (Get f) (c : cs) = run (f c) cs
run (Look f) cs = run (f cs) cs
run (Result a p) cs = (a, cs) : run p cs
run (Final r) _ = r
run _ _ = []

parse :: Parser a -> String -> [(a, String)]
parse (Parser p) = run (p return)

(<|) :: Parser a -> Parser a -> Parser a
(Parser p) <| (Parser q) = do
  s <- look
  probe (p return) s 0
  where
    probe (Get f) (c : cs) n = probe (f c) cs (n + 1)
    probe (Look f) s n = probe (f s) s n
    probe p@(Result _ _) _ n = discard n >> (Parser (p >>=))
    probe (Final r) _ _ = Parser (Final r >>=)
    probe _ _ _ = Parser q
    discard 0 = return ()
    discard n = get >> discard (n - 1)

unsafeParse :: Parser a -> String -> a
unsafeParse p s
  | length parsed == 0 = error ("Cannot parse: \"" ++ s ++ "\".")
  | otherwise = fst $ last parsed
  where
    parsed = parse p s

satisfy :: (Char -> Bool) -> Parser Char
satisfy f = do
  c <- get
  if f c
    then return c
    else pfail

munch :: (Char -> Bool) -> Parser String
munch f = do
  s <- look
  scan s
  where
    scan (c : cs) | f c = do _ <- get; s <- scan cs; return (c : s)
    scan _ = do return ""

munch1 :: (Char -> Bool) -> Parser String
munch1 f = do
  c <- get
  if f c
    then do s <- munch f; return (c : s)
    else pfail

choice :: [Parser a] -> Parser a
choice [] = pfail
choice [p] = p
choice (p : ps) = p <|> choice ps

between :: Parser a -> Parser b -> Parser c -> Parser c
between open close p = do
  _ <- open
  x <- p
  _ <- close
  return x

optional :: Parser a -> Parser ()
optional p = (p >> return ()) <|> return ()

withDefault :: a -> Parser a -> Parser a
withDefault d p = p <|> return d

many :: Parser a -> Parser [a]
many p = return [] <|> many1 p

many1 :: Parser a -> Parser [a]
many1 p = liftM2 (:) p (many p)

sepBy :: Parser b -> Parser a -> Parser [a]
sepBy sep p = sepBy1 sep p <|> return []

sepBy1 :: Parser b -> Parser a -> Parser [a]
sepBy1 sep p = liftM2 (:) p (many (sep >> p))

manyUntil :: Parser a -> Parser b -> Parser [a]
manyUntil p end = scan
  where
    scan = (end >> return []) <| (liftM2 (:) p scan)

char :: Char -> Parser Char
char c = satisfy (== c)

whiteSpace :: Parser Char
whiteSpace = satisfy isSpace

string :: String -> Parser String
string s = do
  s' <- look
  scan s s'
  where
    scan [] _ = do return s
    scan (x : xs) (y : ys) | x == y = do _ <- get; scan xs ys
    scan _ _ = do pfail

natural :: Parser Int
natural = read <$> munch1 isDigit

integer :: Parser Int
integer = signed <$> withDefault '+' (char '-') <*> (read <$> munch1 isDigit)
  where
    signed '-' i = (-1) * i
    signed _ i = i

anyChar :: Parser Char
anyChar = get

letters :: Parser String
letters = munch1 isAlpha

eof :: Parser ()
eof = do
  s <- look
  if null s then return () else pfail
