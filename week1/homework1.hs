-- Exercise 1
toDigits:: Integer -> [Integer]
toDigits n
  | n<=0 = []
  | n>0 && n<10 = [n]
  | n `mod` 10==0 = toDigits (div n 10)
  | n>10 = toDigits (div n 10) ++ [n `mod` 10]

toDigitsRev:: Integer -> [Integer]
toDigitsRev n
  | n<=0 = []
  | n>0 && n<10 = [n]
  | n `mod` 10==0 = toDigitsRev (div n 10)
  | n>10 = (n`mod`10) : toDigitsRev (div n 10)

-- Exercise 2
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther [x] = [x]
doubleEveryOther [x, y] = [2*x,y]
doubleEveryOther (x:y:zs)
  | (length zs)`mod`2==1 = x:(2*y):doubleEveryOther (zs)
  | otherwise =(2*x):y:doubleEveryOther zs

-- Exercise 3
sumDigits :: [Integer] -> Integer
sumDigits [x]
  | (x<10&&x>0) = x
  | (x>=10) = sumDigits (toDigits x)
  | otherwise = 0
sumDigits (x:xs) = sumDigits (toDigits x) + sumDigits xs

-- Exercise 4
validate :: Integer -> Bool
validate x = (sumDigits (doubleEveryOther (toDigits x)))`mod`10 == 0

-- Exercise 5
type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n a b c
  | n==1 = [(a,b)]
  | n==2 = [(a, c),(a, b),(c, b)]
  | n>2 = (hanoi (n-1) a c b) ++ (hanoi 1 a b c) ++ (hanoi (n-1) c b a)

-- Exercise 6 (incomplete)
-- hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
-- hanoi4 n a b c d
--   | n==1 = [(a,b)]
--   | n==2 = [(a, c),(a, b),(c, b)]
--   | n==3 = [(a, c),(a, d),(a, b), (d, b), (c, b)]
--   | n>3 = 
