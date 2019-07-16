module CpuArrow.Logic where

import Control.Arrow
import Control.Monad
import CpuArrow.Base
import CpuArrow.BoolAlgebra

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
