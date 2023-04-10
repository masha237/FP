module HW3.Base (HiFun(..),
                 HiValue(..),
                 HiExpr(..),
                 HiError(..)) where


data HiFun =    -- function names (e.g. div, sort, length, ...)
  HiFunDiv
  | HiFunMul
  | HiFunAdd
  | HiFunSub
  | HiFunNot
  | HiFunAnd
  | HiFunOr
  | HiFunLessThan
  | HiFunGreaterThan
  | HiFunEquals
  | HiFunNotLessThan
  | HiFunNotGreaterThan
  | HiFunNotEquals
  | HiFunIf
  deriving (Eq, Ord)
data HiValue =  -- values (numbers, booleans, strings, ...)
  HiValueBool Bool
  | HiValueNumber Rational
  | HiValueFunction HiFun 
  deriving (Eq, Ord)

data HiExpr =   -- expressions (literals, function calls, ...)
  HiExprValue HiValue
  | HiExprApply HiExpr [HiExpr]
data HiError =  -- evaluation errors (invalid arguments, ...)
  HiErrorInvalidArgument
  | HiErrorInvalidFunction
  | HiErrorArityMismatch
  | HiErrorDivideByZero
  deriving (Show)