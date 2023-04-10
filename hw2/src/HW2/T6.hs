{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE BlockArguments #-}
module HW2.T6 where

import GHC.Natural
import Control.Applicative
import Control.Monad
import HW2.T5 (ExceptState (..))
import HW2.T4 (Expr (..), Prim (..))
import HW2.T1 (Annotated (..), Except (..))

data ParseError = ErrorAtPos Natural

newtype Parser a = P (ExceptState ParseError (Natural, String) a)
  deriving newtype (Functor, Applicative, Monad)

runP :: Parser a -> String -> Except ParseError a
runP (P state) s = case runES state (0, s) of
  (Error e) -> Error e
  (Success (a :# s')) -> Success a

pChar :: Parser Char
pChar = P $ ES $ \(pos, s) ->
  case s of
    []     -> Error (ErrorAtPos pos)
    (c:cs) -> Success (c :# (pos + 1, cs))

parseError :: Parser a
parseError = P $ ES \(pos, s) -> Error (ErrorAtPos pos)

instance Alternative Parser where
  empty = parseError
  (<|>) (P p) (P q) = undefined

instance MonadPlus Parser   -- No methods.



pEof :: Parser ()
pEof = P $ ES $ \(pos, s) ->
  case s of
    []     -> Success (() :# (pos, s))
    (c:cs) -> Error (ErrorAtPos pos)


parseExpr :: String -> Except ParseError Expr
parseExpr expr = runP parser expr

parser :: Parser Expr
parser = undefined






