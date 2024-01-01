{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-missing-methods #-}

-- Exercise 1

fib :: Integer -> Integer
fib n
  | n == 0 = 0
  | n == 1 = 1
  | otherwise = (fib (n - 1)) + (fib (n - 2))

fibs1 :: [Integer]
fibs1 = map fib [0 ..]

-- Exercise 2

fibs2 :: [Integer]
fibs2 = 0 : 1 : zipWith (+) fibs2 ((\list -> case list of
                                       (_:ys) -> ys
                                       []->[]) fibs2)

-- fib2 :: [Integer] -> [Integer]
-- fib2 list@(_:_:_) = fib2 (list ++ [sumLast2 list])
-- fib2 _ = []

-- sumLast2 :: [Integer]->Integer
-- sumLast2 list = sum (drop ((length list) - 2) list)

-- sum' :: [Integer] -> Integer
-- sum' [x,y] = x+y
-- sum' _ = 0

-- fib2 :: Integer -> [Integer] -> [Integer]
-- fib2 n list = if (n > (fromIntegral lengthOfList)) then fib2 n (list ++ [(sum (drop ((lengthOfList) - 2) list))]) else list where lengthOfList = length list

-- Exercise 3

data Stream a = Cons a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Cons a stream) = a : streamToList stream

stream1 :: Stream Integer
stream1 = Cons 1 stream1

instance (Show a) => Show (Stream a) where
  show y = show $ take 20 (streamToList y)

-- Exercise 4

streamRepeat :: a -> Stream a
streamRepeat y = Cons y (streamRepeat y)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons y stream) = Cons (f y) (streamMap f stream)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f seed = Cons seed (streamFromSeed f (f seed))

-- Exercise 5

nats :: Stream Integer
nats = streamFromSeed (1 +) 1

-- https://stackoverflow.com/questions/25561092/haskell-not-lazy-evaluating-interleaving
-- interleaveStream :: Stream a -> Stream a -> Stream a
-- interleaveStream (Cons a streama) (Cons b streamb) = (Cons a (Cons b (interleaveStream streama streamb)))

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Cons y ys) zs = Cons y (interleaveStreams zs ys)

-- evens :: Stream Integer
-- evens = streamFromSeed (2+) 2

-- getPow :: Integer -> Integer -> Integer
-- getPow n y
--   | (y `mod` 2 == 0) =  getPow (n+1) (y `div` 2)
--   | otherwise = n

ruler :: Stream Integer
ruler = startRuler 0

startRuler :: Integer -> Stream Integer
startRuler n = interleaveStreams (streamRepeat n) (startRuler (n + 1))

-- Exercise 6

x :: Stream Integer
x = Cons 0 (Cons 1 (streamRepeat 0))

instance Num (Stream Integer) where
  fromInteger y = Cons y (streamRepeat 0)
  negate = streamMap negate
  (Cons a streama) + (Cons b streamb) = Cons (a + b) (streama + streamb)
  (Cons a streama) * bs@(Cons b streamb) = Cons (a * b) ((streamMap (a *) streamb) + (streama * bs))

instance Fractional (Stream Integer) where
  as@(Cons a streama) / bs@(Cons b streamb) = Cons (a `div` b) (streamMap ((1 `div` b) *) (streama - (q * streamb))) where q = as / bs

fibs3 :: Stream Integer
fibs3 = x / (1 - x - (x * x))

-- Exercise 7

data Matrix = Matrix Integer Integer Integer Integer
  deriving (Show)

instance Num Matrix where
  (Matrix a1 a2 a3 a4) * (Matrix b1 b2 b3 b4) = Matrix ((a1 * b1) + (a2 * b3)) ((a1 * b2) + (a2 * b4)) ((a3 * b1) + (a4 * b3)) ((a3 * b2) + (a4 * b4))

fib4 :: Integer -> Integer
fib4 int
  | int == 0 = 0
  | otherwise = (\(Matrix _ b _ _) -> b) ((Matrix 1 1 1 0) ^ int)
