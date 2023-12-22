module Golf where

import Data.List

-- Exercise 1
skips :: [a] -> [[a]]
skips [x] = [[x]]
skips xs = map (getMultiple xs) [1 .. (length xs)]

getMultiple :: [a] -> Int -> [a]
getMultiple list n = case element of
  (Just value) -> value : getMultiple (drop n list) n
  Nothing -> []
  where
    element = safeIndex list (n - 1)

safeIndex :: [a] -> Int -> Maybe a
safeIndex xs n =
  if n < length xs && n >= 0
    then Just (xs !! n)
    else Nothing

-- Exercise 2
localMaxima :: [Integer] -> [Integer]
localMaxima list = case list of
  x : y : z : _ ->
    if y > x && y > z
      then y : localMaxima (drop 1 list)
      else localMaxima (drop 1 list)
  _ -> []

-- Exercise 3
histogram :: [Integer] -> String
histogram list = case list of
  [] -> "==========\n0123456789\n"
  x -> countString ++ histogram (recoverList (map (\x -> if x == (maximum count) then subtract 1 x else x) count))
  where
    count = map (getCount list) [0 .. 9]
    countString = map (toChar (maximum count)) count ++ "\n"

recoverList :: [Integer] -> [Integer]
recoverList countList = concatMap (uncurry replicate) zippedList
  where
    zippedList = zip (map fromIntegral countList) [0 .. (fromIntegral (length countList) - 1)]

getCount :: [Integer] -> Integer -> Integer
getCount list x = toInteger (length (filter (x ==) list))

toChar :: Integer -> Integer -> Char
toChar x y
  | x == y = '*'
  | otherwise = ' '
