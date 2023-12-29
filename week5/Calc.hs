{-# LANGUAGE FlexibleInstances #-}

module Calc where

import Data.Map qualified as M
import Data.Maybe
import ExprT qualified
import Parser
import StackVM qualified

-- Exercise 1

eval :: ExprT.ExprT -> Integer
eval exp = case exp of
  ExprT.Mul exp1 exp2 -> eval exp1 * eval exp2
  ExprT.Add exp1 exp2 -> eval exp1 + eval exp2
  ExprT.Lit int -> int

-- Exercise 2

evalString :: String -> Maybe Integer
evalString string = case parseExp ExprT.Lit ExprT.Add ExprT.Mul string of
  Just exp -> Just (eval exp)
  Nothing -> Nothing

-- Exercise 3

class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

instance Expr ExprT.ExprT where
  lit = ExprT.Lit
  add = ExprT.Add
  mul = ExprT.Mul

reify :: ExprT.ExprT -> ExprT.ExprT
reify = id

-- Exercise 4

instance Expr Integer where
  lit x = x
  add x y = x + y
  mul x y = x * y

instance Expr Bool where
  lit x = x > 0
  add x y = x || y
  mul x y = x && y

instance Expr MinMax where
  add (MinMax x) (MinMax y) = MinMax (max x y)
  mul (MinMax x) (MinMax y) = MinMax (min x y)
  lit = MinMax

instance Expr Mod7 where
  add (Mod7 x) (Mod7 y) = Mod7 ((x + y) `mod` 7)
  mul (Mod7 x) (Mod7 y) = Mod7 (x * y `mod` 7)
  lit = Mod7

newtype MinMax = MinMax Integer deriving (Eq, Show)

newtype Mod7 = Mod7 Integer deriving (Eq, Show)

testExp :: (Expr a) => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

testInteger = testExp :: Maybe Integer

testBool = testExp :: Maybe Bool

testMM = testExp :: Maybe MinMax

testSat = testExp :: Maybe Mod7

-- Exercise 5

instance Expr StackVM.Program where
  lit x = [StackVM.PushI x]
  add x y = x ++ y ++ [StackVM.Add]
  mul x y = x ++ y ++ [StackVM.Mul]

compile :: String -> Maybe StackVM.Program
compile = parseExp lit add mul

-- Exercise 6

class HasVars a where
  var :: String -> a

data VarExprT
  = Lit Integer
  | Add VarExprT VarExprT
  | Mul VarExprT VarExprT
  | Var String
  deriving (Show, Eq)

instance Expr VarExprT where
  lit = Lit
  add = Add
  mul = Mul

instance HasVars VarExprT where
  var = Var

instance HasVars (M.Map String Integer -> Maybe Integer) where
  var = M.lookup

instance Expr (M.Map String Integer -> Maybe Integer) where
  add x y =
    \z -> case (x z, y z) of
        (Just int1, Just int2) -> Just (int1 + int2)
        _ -> Nothing
  mul x y =
    \z -> case (x z, y z) of
        (Just int1, Just int2) -> Just (int1 * int2)
        _ -> Nothing
  lit x = \z -> Just x

withVars ::
  [(String, Integer)] ->
  (M.Map String Integer -> Maybe Integer) ->
  Maybe Integer
withVars vs exp = exp $ M.fromList vs
