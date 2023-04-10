{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}

module HW2.T5 where

import HW2.T1 (Annotated (..), Except (..))
import HW2.T4 (Prim (..), Expr (..))

import Control.Monad

data ExceptState e s a = ES { runES :: s -> Except e (Annotated s a) }

mapExceptState :: (a -> b) -> ExceptState e s a -> ExceptState e s b
mapExceptState f state = ES {runES = mapState f . runES state}

mapState :: (a -> b) -> Except e (Annotated s a) -> Except e (Annotated s b)
mapState f (Error e) = Error e
mapState f (Success (a :# s)) = Success ((f a) :# s)

wrapExceptState :: a -> ExceptState e s a
wrapExceptState a = ES {runES = \s -> Success (a :# s)}

joinExceptState :: ExceptState e s (ExceptState e s a) -> ExceptState e s a
joinExceptState (ES f) = ES {runES = \case
  (f -> Error e) -> Error e
  (f -> Success (ES f' :# s)) -> f' s}


modifyExceptState :: (s -> s) -> ExceptState e s ()
modifyExceptState f = ES {runES = \s -> Success (() :# (f s))}

throwExceptState :: e -> ExceptState e s a
throwExceptState exception = ES {runES = \_ -> Error exception}

instance Functor (ExceptState e s) where
  fmap = mapExceptState

instance Applicative (ExceptState e s) where
  pure = wrapExceptState
  p <*> q = Control.Monad.ap p q

instance Monad (ExceptState e s) where
  m >>= f = joinExceptState (fmap f m)


data EvaluationError = DivideByZero
eval :: Expr -> ExceptState EvaluationError [Prim Double] Double
eval (Val x)= return x
eval (Op (Add x y)) = evalBin x y Add (+)
eval (Op (Sub x y)) = evalBin x y Sub (-)
eval (Op (Mul x y)) = evalBin x y Mul (*)
eval (Op (Div x y)) = evalDiv x y

eval (Op (Sgn x)) = evalMon x Sgn (signum)
eval (Op (Abs x)) = evalMon x Abs (abs)


evalDiv :: Expr -> Expr -> ExceptState EvaluationError [Prim Double] Double
evalDiv x y =
  do
      xx <- eval x
      yy <- eval y
      when (yy == 0) (throwExceptState DivideByZero)
      modifyExceptState (Div xx yy :)
      return (xx / yy)

evalBin :: Expr -> Expr -> (Double -> Double -> Prim Double) -> (Double -> Double -> Double) -> ExceptState EvaluationError [Prim Double] Double
evalBin x y expr oper =
  do
      xx <- eval x
      yy <- eval y
      modifyExceptState (expr xx yy :)
      return (oper xx yy)

evalMon :: Expr -> (Double -> Prim Double) -> (Double -> Double) -> ExceptState EvaluationError [Prim Double] Double
evalMon x expr oper =
  do
      xx <- eval x
      modifyExceptState (expr xx :)
      return (oper xx)
