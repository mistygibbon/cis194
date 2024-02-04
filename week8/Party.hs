{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}

module Party where

import Data.List
import Data.Tree
import Employee

-- Exercise 1

glCons :: Employee -> GuestList -> GuestList
glCons employee@(Emp {empFun = eFun}) (GL list fun) = GL (employee : list) (eFun + fun)

instance Semigroup GuestList where
  (GL list1 fun1) <> (GL list2 fun2) = GL (list1 ++ list2) (fun1 + fun2)

instance Monoid GuestList where
  mempty = GL [] 0

moreFun :: GuestList -> GuestList -> GuestList
moreFun list1@(GL _ fun1) list2@(GL _ fun2)
  | fun1 > fun2 = list1
  | otherwise = list2

-- Exercise 2

treeFold :: (b -> a -> b) -> b -> Tree a -> b
treeFold fn acc tree@(Node {rootLabel = label, subForest = subForest}) = fn (foldl' (treeFold fn) acc subForest) label

-- Exercise 3

nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel boss lists = (glCons boss (bestWBoss), bestWOBoss)
  where
    bestWBoss = foldl' (<>) (GL [] 0) (map snd lists)
    bestWOBoss = foldl' (<>) (GL [] 0) (map fst lists)

-- Exercise 4

maxFun :: Tree Employee -> GuestList
maxFun tree = uncurry (moreFun) (maxFunH tree)

maxFunH :: Tree Employee -> (GuestList, GuestList)
maxFunH tree
  | subtrees == [] = (glCons (rootLabel tree) (GL [] 0), GL [] 0)
  | otherwise = nextLevel (rootLabel tree) (map maxFunH subtrees)
  where
    subtrees = subForest tree

-- Exercise 5

getEmployees :: GuestList -> [Employee]
getEmployees (GL x _) = x

getFun :: GuestList -> Integer
getFun (GL _ x) = x

main :: IO ()
main = do
  x <- readFile "./company.txt"
  let tree = read x
  let guestlist = maxFun tree
  let fun = getFun guestlist
  let employees = getEmployees guestlist
  let names = sort $ map empName employees
  putStr "Total fun: "
  print fun

  mapM_ putStrLn names
