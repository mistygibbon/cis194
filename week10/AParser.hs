{- CIS 194 HW 10
   due Monday, 1 April
-}

module AParser where

import           Control.Applicative

import           Data.Char

-- A parser for a value of type a is a function which takes a String
-- represnting the input to be parsed, and succeeds or fails; if it
-- succeeds, it returns the parsed value along with the remainder of
-- the input.
newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

-- For example, 'satisfy' takes a predicate on Char, and constructs a
-- parser which succeeds only if it sees a Char that satisfies the
-- predicate (which it then returns).  If it encounters a Char that
-- does not satisfy the predicate (or an empty input), it fails.
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
  where
    f [] = Nothing    -- fail on the empty input
    f (x:xs)          -- check if x satisfies the predicate
                        -- if so, return x along with the remainder
                        -- of the input (that is, xs)
        | p x       = Just (x, xs)
        | otherwise = Nothing  -- otherwise, fail

-- Using satisfy, we can define the parser 'char c' which expects to
-- see exactly the character c, and fails otherwise.
char :: Char -> Parser Char
char c = satisfy (== c)

{- For example:

*Parser> runParser (satisfy isUpper) "ABC"
Just ('A',"BC")
*Parser> runParser (satisfy isUpper) "abc"
Nothing
*Parser> runParser (char 'x') "xyz"
Just ('x',"yz")

-}

-- For convenience, we've also provided a parser for positive
-- integers.
posInt :: Parser Integer
posInt = Parser f
  where
    f xs
      | null ns   = Nothing
      | otherwise = Just (read ns, rest)
      where (ns, rest) = span isDigit xs

------------------------------------------------------------
-- Your code goes below here
------------------------------------------------------------

-- Exercise 1

first :: (a -> b) -> (a,c) -> (b,c)
first f (a,c) = (f a, c)

instance Functor Parser where
  -- fmap :: (a->b) -> (Parser a) -> (Parser b)
  fmap func (Parser f) = Parser (fmap (first func) .  f)

-- Exercise 2

instance Applicative Parser where
  pure a = Parser f
           where f x = Just (a,x)

  p1 <*> p2 = Parser f
              where
                f input = case runParser p1 input of
                  Nothing -> Nothing
                  Just (func,remainder) -> first func <$> runParser p2 remainder


-- parseName :: Parser Name

-- parsePhone :: Parser String

-- Emp <$> parseName <*> parsePhone :: Parser Employee

-- Exercise 3

abParser :: Parser (Char, Char)
abParser =  (,) <$> char 'a' <*> char 'b'

-- testParser = Parser f where
--   f [] = Nothing
--   f (x:y:xs)
--     | (x=='a' && y=='b') = Just ((x,y),xs)
--     | otherwise = Nothing

abParser_ :: Parser ()
abParser_ = (\x->()) <$> abParser

intPair = (\a b c -> [a,c]) <$> posInt <*> char ' ' <*> posInt

-- Exercise 4

-- class Applicative f => Alternative f where
-- empty :: f a
-- (<|>) :: f a -> f a -> f a

instance Alternative Parser where
  empty = Parser (\x -> Nothing)
  p1 <|> p2 = Parser (\x -> case (runParser p1 x) of
                              Just a -> Just a
                              Nothing -> (runParser p2 x)
                     )
-- Exercise 5

intOrUppercase :: Parser ()
intOrUppercase = (const () <$> posInt) <|>  (const () <$> (satisfy isUpper))
