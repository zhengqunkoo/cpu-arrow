module CpuArrow.Advanced.HalfAdder where

import Control.Arrow
import CpuArrow.Base
import CpuArrow.BoolAlgebra
import CpuArrow.Logic

aHalfAdder :: BoolAlgebra a => Circuit (a, a) (a, a)
aHalfAdder = aAnd &&& aXor
