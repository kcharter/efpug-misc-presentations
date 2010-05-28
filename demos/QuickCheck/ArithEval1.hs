module ArithEval1 where

import Arith

-- | Evaluates an expression. Division by zero will throw
-- an exception.
eval :: Exp -> Integer
eval exp =
    case exp of Const n -> n
                BinOpCall op exp1 exp2 ->
                    let n1 = eval exp1
                        n2 = eval exp2
                    in case op of Add -> n1 + n2
                                  Sub -> n1 - n2
                                  Mul -> n1 * n2
                                  Div -> n1 `div` n2

