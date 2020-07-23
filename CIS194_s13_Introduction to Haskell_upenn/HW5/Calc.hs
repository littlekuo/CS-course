{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Calc where

import ExprT
import Parser
import StackVM
import Data.Maybe
import qualified Data.Map as M


eval :: ExprT -> Integer
eval (ExprT.Lit i) = i
eval (ExprT.Add a b) = eval a + eval b
eval (ExprT.Mul a b) = eval a * eval b

evalStr :: String -> Maybe Integer
evalStr = fmap eval . parseExp ExprT.Lit ExprT.Add ExprT.Mul


class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

instance Expr ExprT where
  lit = ExprT.Lit
  add = ExprT.Add
  mul = ExprT.Mul

reify :: ExprT -> ExprT
reify = id

--ex4
instance Expr Integer where
  lit = id  
  add = (+)
  mul = (*)

instance Expr Bool where
  lit x  
        | x <= 0    = False
        | otherwise = True
  add = (||)
  mul = (&&)

newtype MinMax  = MinMax Integer deriving (Eq, Show)

instance Expr MinMax where
  lit = MinMax
  add (MinMax x) (MinMax y)= MinMax (max x y)
  mul (MinMax x) (MinMax y)= MinMax (min x y)

newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr Mod7 where
  lit x = Mod7 (x `mod` 7)
  add (Mod7 x) (Mod7 y)= Mod7 ((x + y) `mod` 7)
  mul (Mod7 x) (Mod7 y)= Mod7 ((x * y) `mod` 7)

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3*-4) + 5"

testInteger  = testExp :: Maybe Integer
testBool     = testExp :: Maybe Bool
testMM       = testExp :: Maybe MinMax
testSat      = testExp :: Maybe Mod7

--ex5

compile :: String -> Maybe Program
compile = parseExp lit add mul

instance Expr StackVM.Program where
  lit i = [StackVM.PushI i]
  add a b = a ++ b ++ [StackVM.Add]
  mul a b = a ++ b ++ [StackVM.Mul]


testProg :: Maybe StackVM.Program
testProg = testExp

compile2 :: String -> Either String StackVal
compile2 = stackVM . fromMaybe [] . compile


--ex6
class HasVars a where
  var :: String -> a

data VarExprT = Lit Integer
                | Var String
                | Add VarExprT VarExprT
                | Mul VarExprT VarExprT  deriving (Show,Eq)

instance Expr VarExprT where
  lit = Calc.Lit
  add = Calc.Add
  mul = Calc.Mul

instance HasVars VarExprT where
  var = Var


instance HasVars (M.Map String Integer -> Maybe Integer) where
        var = M.lookup


instance Expr (M.Map String Integer -> Maybe Integer) where
        lit a = \_ -> Just a
        add f g = \m -> case (isNothing (f m) || isNothing (g m)) of
                    True -> Nothing
                    _    -> Just (fromJust (f m) + fromJust (g m))
        mul f g = \m -> case (isNothing (f m) || isNothing (g m)) of
                    True -> Nothing
                    _    -> Just (fromJust (f m) * fromJust (g m))

withVars :: [(String, Integer)]-> (M.Map String Integer -> Maybe Integer)-> Maybe Integer
withVars vs exp = exp $ M.fromList vs

