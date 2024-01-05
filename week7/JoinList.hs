{-# OPTIONS_GHC -Wall #-}

module JoinList where

import Buffer
import Data.List (foldl')
import Editor
import Scrabble
import Sized

-- Exercise 1

data JoinList m a
  = Empty
  | Single m a
  | Append m (JoinList m a) (JoinList m a)
  deriving (Eq, Show)

tag :: (Monoid m) => JoinList m a -> m
tag (Single m _) = m
tag (Append m _ _) = m
tag Empty = mempty

(+++) :: (Monoid m) => JoinList m a -> JoinList m a -> JoinList m a
l1 +++ l2 = Append (tag l1 <> tag l2) l1 l2

-- Exercise 2

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ index (Append m left right)
  | index >= (getSize . size $ m) = Nothing
  | index < leftSize = indexJ index left
  | otherwise = indexJ (index - leftSize) right
  where
    leftSize = getSize . size $ tag left
indexJ int (Single _ a)
  | int == 0 = Just a
  | otherwise = Nothing
indexJ _ Empty = Nothing

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ int (Append m left right)
  | int >= listSize = Empty
  | int < leftSize = Append (tag newLeft <> tag right) newLeft right
  | otherwise = newRight
  where
    listSize = getSize . size $ m
    leftSize = getSize . size $ tag left
    newLeft = dropJ int left
    newRight = dropJ (int - leftSize) right
dropJ _ Empty = Empty
dropJ int single
  | int == 1 = Empty
  | otherwise = single

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ int list@(Append m left right)
  | int <= 0 = Empty
  | int >= listSize = list
  | int > leftSize = Append (tag left <> tag newRight) left newRight
  | int == leftSize = left
  | otherwise = newLeft
  where
    leftSize = getSize . size $ tag left
    listSize = getSize . size $ m
    newLeft = takeJ int left
    newRight = takeJ (int - leftSize) right
takeJ _ Empty = Empty
takeJ int single
  | int == 1 = single
  | otherwise = Empty

-- Exercise 3

scoreLine :: String -> JoinList Score String
scoreLine string = Single (scoreString string) string

-- Exercise 4

instance Buffer (JoinList (Score, Size) String) where
  toString (Append _ left right) = toString left ++ toString right
  toString Empty = ""
  toString (Single _ string) = string
  fromString string = foldl' (\a b -> a +++ Single (scoreString b, Size 1) b) Empty (lines string)
  line = indexJ
  replaceLine int string list = takeJ int list +++ fromString string +++ dropJ (int + 1) list
  numLines list = getSize . size $ tag list
  value = numLines

main = runEditor editor (Single (Score 1, Size 1) "Hello world!")
