{-# OPTIONS_GHC -Wall #-}

module Scrabble where

import Data.Char (toLower)

newtype Score = Score Integer
  deriving (Eq, Show, Ord, Num)

instance Monoid Score where
  mempty = Score 0

instance Semigroup Score where
  (Score a) <> (Score b) = Score (a + b)

score :: Char -> Score
score char
  | d `elem` "eaionrtlsu" = 1
  | d `elem` "dg" = 2
  | d `elem` "bcmp" = 3
  | d `elem` "fhvwy" = 4
  | d == 'k' = 5
  | d `elem` "jx" = 8
  | d `elem` "qz" = 10
  | otherwise = 0
  where
    d = toLower char

scoreString :: String -> Score
scoreString = foldr (\a b -> score a + b) 0
