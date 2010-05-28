module ArithTests1 where

import Test.QuickCheck

import Arith
import ArithEval1
import ArithQuickCheck

runTests :: IO ()
runTests = mapM_ quickCheck [prop_evalAddAdds,
                             prop_evalSubSubs,
                             prop_evalMulMuls,
                             prop_evalDivDivs]


prop_evalAddAdds = prop_evalBinOp Add (+)
prop_evalSubSubs = prop_evalBinOp Sub (-)
prop_evalMulMuls = prop_evalBinOp Mul (*)
prop_evalDivDivs = prop_evalBinOp Div div

prop_evalBinOp op f (exp1,exp2) =
    eval (BinOpCall op exp1 exp2) == f (eval exp1) (eval exp2)