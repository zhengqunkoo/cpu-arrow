module CpuArrow.Advanced.FullAdder where

import Control.Arrow
import CpuArrow.Advanced.HalfAdder
import CpuArrow.Base
import CpuArrow.BoolAlgebra
import CpuArrow.Logic

aFullAdder :: BoolAlgebra a => Circuit ((a, a), a) (a, a)
aFullAdder =
  (arr fst >>> aHalfAdder) &&& arr snd >>>
  ((arr (\((_, s'), c) -> (s', c)) >>> aHalfAdder) &&& arr (\((c', _), _) -> c')) >>>
  ((arr (\((c'', _), c') -> (c'', c')) >>> aOr) &&& arr (\((_, s''), _) -> s''))
