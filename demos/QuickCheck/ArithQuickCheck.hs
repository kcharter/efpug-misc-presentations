-- | QuickCheck instances and generators for the Arith types.

module ArithQuickCheck where

import Control.Monad (liftM, liftM3)
import Test.QuickCheck

import Arith

instance Arbitrary BinOp where
    arbitrary = oneof $ map return [Add,Sub,Mul,Div]
    shrink = shrinkNothing

instance Arbitrary Exp where
    arbitrary = sized expsOfSize
    shrink (Const n) = map Const $ shrink n
    shrink (BinOpCall op e1 e2) = [e1,e2]

expsOfSize :: Int -> Gen Exp
expsOfSize size =
    if size <= 1
    then constants
    else frequency [(1, constants), (4, binOpCalls)]
    where constants = Const `liftM` arbitrary
          binOpCalls = liftM3 BinOpCall arbitrary subExps subExps
          subExps = expsOfSize (size `div` 2)


