module HW2.T4 where

import HW2.T1 (Annotated (..), mapAnnotated)
import Control.Monad

data State s a = S { runS :: s -> Annotated s a }

mapState :: (a -> b) -> State s a -> State s b
mapState f state = S {runS = (\s -> (mapAnnotated f ((runS state) s)))}


wrapState :: a -> State s a
wrapState a = S {runS = (a :#)}

getA :: Annotated s a -> a
getA (a :# s) = a

getS :: Annotated s a -> s
getS (a :# s) = s


joinState :: State s (State s a) -> State s a
joinState state = S {runS = (\s -> (runS (getA (runS state s)) (getS (runS state s))))}

modifyState :: (s -> s) -> State s ()
modifyState f = S {runS = \s -> () :# (f s)}

instance Functor (State s) where
  fmap = mapState

instance Applicative (State s) where
  pure = wrapState
  p <*> q = Control.Monad.ap p q

instance Monad (State s) where
  m >>= f = joinState (fmap f m)

data Prim a =
    Add a a      -- (+)
  | Sub a a      -- (-)
  | Mul a a      -- (*)
  | Div a a      -- (/)
  | Abs a        -- abs
  | Sgn a        -- signum

data Expr = Val Double | Op (Prim Expr)

instance Num Expr where
  x + y = Op (Add x y)
  x * y = Op (Mul x y)
  x - y = Op (Sub x y)
  abs x = Op (Abs x)
  signum x = Op (Sgn x)
  fromInteger x = Val (fromInteger x)

instance Fractional Expr where
   x / y = Op (Div x y)
   fromRational x = Val (fromRational x)

eval :: Expr -> State [Prim Double] Double
eval (Val x)= return x
eval (Op (Add x y)) = evalBin x y Add (+)
eval (Op (Sub x y)) = evalBin x y Sub (-)
eval (Op (Mul x y)) = evalBin x y Mul (*)
eval (Op (Div x y)) = evalBin x y Div (/)

eval (Op (Sgn x)) = evalMon x Sgn (signum)
eval (Op (Abs x)) = evalMon x Abs (abs)

evalBin :: Expr -> Expr -> (Double -> Double -> Prim Double) -> (Double -> Double -> Double) -> State [Prim Double] Double
evalBin x y expr oper =
  do
      xx <- eval x
      yy <- eval y

      modifyState (expr xx yy :)
      return (oper xx yy)

evalMon :: Expr -> (Double -> Prim Double) -> (Double -> Double) -> State [Prim Double] Double
evalMon x expr oper =
  do
      xx <- eval x
      modifyState (expr xx :)
      return (oper xx)


