module CpuArrow.Logic where

import Control.Arrow
import Control.Monad
import CpuArrow.Base

class BoolAlgebra a where
    top :: a
    bottom :: a

    bAnd :: a -> a -> a
    bOr :: a -> a -> a
    bNot :: a -> a
    bNand :: a -> a -> a
    bNand x y = bNot (bAnd x y)
    bXor :: a -> a -> a
    bXor x y = bOr (bAnd x (bNot y)) (bAnd (bNot x) y)

    dAnd :: Bool -> a -> a
    dAnd b x = if b then x else bottom
    dOr :: Bool -> a -> a
    dOr b x = if b then top else x

instance BoolAlgebra Bool where
    top = True
    bottom = False
    bAnd = (&&)
    bOr = (||)
    bNot = not

instance BoolAlgebra a => BoolAlgebra [a] where
    top = top:top
    bottom = bottom:bottom
    bAnd = liftM2 bAnd
    bOr = liftM2 bOr
    bNot = liftM bNot

aAnd :: BoolAlgebra a => Circuit (a, a) a
aAnd = arr $ uncurry bAnd

aOr :: BoolAlgebra a => Circuit (a, a) a
aOr = arr $ uncurry bOr

aNot :: BoolAlgebra a => Circuit a a
aNot = arr bNot

aNand :: BoolAlgebra a => Circuit (a, a) a
aNand = aAnd >>> aNot

aXor :: BoolAlgebra a => Circuit (a, a) a
aXor = aNand &&& aOr >>> aAnd
