module CpuArrow.Logic where

import Control.Arrow
import CpuArrow.Base

aAnd :: Circuit (Bool, Bool) Bool
aAnd = arr $ uncurry (&&)

aOr :: Circuit (Bool, Bool) Bool
aOr = arr $ uncurry (||)

aNot :: Circuit Bool Bool
aNot = arr not

aNand :: Circuit (Bool, Bool) Bool
aNand = aNot *** aNot >>> aAnd

aXor :: Circuit (Bool, Bool) Bool
aXor = aNand &&& aOr >>> aAnd
