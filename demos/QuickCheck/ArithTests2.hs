module ArithTests2 where


import Test.QuickCheck

import Arith
import ArithEval2
import ArithQuickCheck

runTests :: IO ()
runTests = mapM_ quickCheck [prop_evalAddAdds,
                             prop_evalSubSubs,
                             prop_evalMulMuls,
                             prop_evalDivDivs]


prop_evalAddAdds = prop_evalSafeBinOp Add (+)
prop_evalSubSubs = prop_evalSafeBinOp Sub (-)
prop_evalMulMuls = prop_evalSafeBinOp Mul (*)
prop_evalDivDivs = prop_evalBinOp (\err _ y -> err == DivisionByZero && y == 0) Div div

prop_evalSafeBinOp = prop_evalBinOp (\_ _ _ -> False)

prop_evalBinOp errorExpected op f (exp1,exp2) =
    case r1 of
      Left _ ->
          r == r1
      Right x1 ->
          case r2 of
            Left _ ->
                r == r2
            Right x2 ->
                case r of
                  Left err ->
                      errorExpected err x1 x2
                  Right x ->
                      x == f x1 x2
    where r1 = eval exp1
          r2 = eval exp2
          r  = eval (BinOpCall op exp1 exp2)
