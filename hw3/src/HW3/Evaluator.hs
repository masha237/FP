module HW3.Evaluator (eval) where

import HW3.Base
    ( HiValue(..),
      HiExpr(..),
      HiFun(..),
      HiError(HiErrorInvalidFunction, HiErrorArityMismatch,
              HiErrorDivideByZero, HiErrorInvalidArgument) )
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import Control.Monad (unless)

eval :: Monad m => HiExpr -> m (Either HiError HiValue)
eval expressions = runExceptT (evalHiMonad expressions)


evalHiMonad :: Monad m => HiExpr -> ExceptM m
evalHiMonad (HiExprValue x) = return x
evalHiMonad (HiExprApply fun list) = evalHiFun fun list



type ExceptM m = ExceptT HiError m HiValue

evalHiFun :: Monad m => HiExpr -> [HiExpr] -> ExceptM m
evalHiFun expressions arguments = do
    evalFun <- evalHiMonad expressions
    case evalFun of
      HiValueFunction f -> do
          unless (checkArity f (length arguments)) (throwE HiErrorArityMismatch)    
          case f of 
            HiFunAnd -> do
              first <- evalHiMonad (head arguments)
              
              case first of
                (HiValueBool False) -> return first
                _ -> evalHiMonad (arguments !! 1)
            HiFunOr -> do
              first <- evalHiMonad (head arguments)
              
              case first of
                (HiValueBool True) -> return first
                _ -> evalHiMonad (arguments !! 1)
            HiFunIf -> do
              first <- evalHiMonad (head arguments)
              
              case first of
                (HiValueBool True) -> evalHiMonad (arguments !! 1)
                _ -> evalHiMonad (arguments !! 2)
            _ -> do
                evalArguments <- traverse evalHiMonad arguments
                case (f, evalArguments) of
                        (HiFunAdd, [HiValueNumber a, HiValueNumber b]) -> return (HiValueNumber (a + b))
                        (HiFunSub, [HiValueNumber a, HiValueNumber b]) -> return (HiValueNumber (a - b))
                        (HiFunMul, [HiValueNumber a, HiValueNumber b]) -> return (HiValueNumber (a * b))
                        (HiFunDiv, [HiValueNumber a, HiValueNumber b]) -> 
                            case b of
                              0 -> throwE HiErrorDivideByZero
                              _ -> return (HiValueNumber (a / b))
                        (HiFunNot, [HiValueBool x]) -> return (HiValueBool (not x))
                        (HiFunLessThan, [a, b]) -> return (HiValueBool (a < b))
                        (HiFunGreaterThan, [a, b]) -> return (HiValueBool (a > b))
                        (HiFunNotLessThan, [a, b]) -> return (HiValueBool (a >= b))
                        (HiFunNotGreaterThan, [a, b]) -> return (HiValueBool (a <= b))
                        (HiFunEquals, [a, b]) -> return (HiValueBool (a == b))
                        (HiFunNotEquals, [a, b]) -> return (HiValueBool (a /= b))
                        _ -> throwE HiErrorInvalidArgument
      _ ->  throwE HiErrorInvalidFunction         



checkArity :: HiFun -> Int -> Bool
checkArity HiFunAdd 2 = True
checkArity HiFunSub 2 = True
checkArity HiFunMul 2 = True
checkArity HiFunDiv 2 = True
checkArity HiFunOr 2 = True
checkArity HiFunAnd 2 = True
checkArity HiFunNot 1 = True
checkArity HiFunLessThan 2 = True
checkArity HiFunGreaterThan 2 = True
checkArity HiFunNotLessThan 2 = True
checkArity HiFunNotGreaterThan 2 = True
checkArity HiFunEquals 2 = True
checkArity HiFunNotEquals 2 = True
checkArity HiFunIf 3 = True
checkArity _ _ = False




 


