{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module ArithEval2 where

import Control.Monad.Error
import Arith

eval :: Exp -> Either EvalError Integer
eval exp =
    case exp of Const n ->
                    return n
                BinOpCall op exp1 exp2 ->
                    f (eval exp1) (eval exp2)
                    where f = case op of Add -> liftM2 (+)
                                         Sub -> liftM2 (-)
                                         Mul -> liftM2 (*)
                                         Div -> \mx my -> do x <- mx
                                                             y <- my
                                                             (if y == 0
                                                              then divisionByZero
                                                              else return $ div x y)
                          divisionByZero = throwError DivisionByZero

data EvalError = DivisionByZero |
                 GeneralError String deriving (Eq, Ord, Show)

instance Error EvalError where
    strMsg = GeneralError

