-- Exercise 1

fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x : xs)
  | even x = (x - 2) * fun1 xs
  | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' = (foldr (*) 1) . (map (subtract 2)) . (filter even)

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n
  | even n = n + fun2 (n `div` 2)
  | otherwise = fun2 (3 * n + 1)

fun2' :: Integer -> Integer
fun2' = (foldr (+) 0) . (filter even) . (takeWhile (1 /=)) . (iterate (\x -> if even x then x `div` 2 else (3 * x + 1)))

-- Exercise 2

data Tree a
  = Leaf
  | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)

foldTree :: [a] -> Tree a
foldTree = foldr (insertBalanced) (Leaf)

insertBalanced :: a -> Tree a -> Tree a
insertBalanced elem Leaf = Node 0 Leaf elem Leaf
insertBalanced elem tree@(Node int ltree a rtree) =
  if lheight > rheight
    then Node ((height newrtree 0)) ltree a newrtree
    else Node ((height newltree 0)) newltree a rtree
  where
    newltree = (insertBalanced elem ltree)
    newrtree = (insertBalanced elem rtree)
    lheight = (height ltree 0)
    rheight = (height rtree 0)

height :: Tree a -> Integer -> Integer
height Leaf h = h
height tree@(Node int ltree a rtree) h = max lheight rheight
  where
    lheight = (height ltree h + 1)
    rheight = (height rtree h + 1)

-- Exercise 3

xor :: [Bool] -> Bool
xor = foldr (\x y -> if y then (not x) else x) False

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x y -> (f x) : y) []

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f base = (foldr (\x y -> f y x) base . reverse)

-- Exercise 4

sieveSundaram :: Integer -> [Integer]
sieveSundaram int = (map (\x -> 2 * x + 1) . filter (\x -> notElem x list)) [1 .. int]
  where
    mylist = [1 .. (ceiling . sqrt . fromIntegral) int]
    ij = filter (\x -> (snd x) >= (fst x)) (cartProd mylist mylist)
    list = map (\x -> (fst x) + (snd x) + 2 * (fst x) * (snd x)) ij

cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x, y) | x <- xs, y <- ys]
