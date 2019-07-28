module CpuArrow.Advanced.FullAdder where

import Control.Arrow
import CpuArrow.Advanced.HalfAdder
import CpuArrow.Base
import CpuArrow.BoolAlgebra
import CpuArrow.Half
import CpuArrow.Logic

aFullAdder :: BoolAlgebra a => Circuit ((a, a), a) (a, a)
aFullAdder =
  (arr fst >>> aHalfAdder) &&& arr snd >>>
  ((arr (\((_, s'), c) -> (s', c)) >>> aHalfAdder) &&& arr (\((c', _), _) -> c')) >>>
  ((arr (\((c'', _), c') -> (c'', c')) >>> aOr) &&& arr (\((_, s''), _) -> s''))

aFullAdder2 :: BoolAlgebra a => Circuit ((T2 a, T2 a), a) (a, T2 a)
aFullAdder2 =
  (arr fst >>> arr (halfFst . fst) &&& arr (halfFst . snd)) &&&
  (first (arr (halfSnd . fst) &&& arr (halfSnd . snd)) >>> aFullAdder) >>>
  (second (arr fst) >>> aFullAdder) &&& arr (snd . snd) >>>
  arr (\((c, s), s') -> (c, halfMerge (s, s')))

aFullAdder3 :: BoolAlgebra a => Circuit ((T4 a, T4 a), a) (a, T4 a)
aFullAdder3 =
  (arr fst >>> arr (halfFst . fst) &&& arr (halfFst . snd)) &&&
  (first (arr (halfSnd . fst) &&& arr (halfSnd . snd)) >>> aFullAdder2) >>>
  (second (arr fst) >>> aFullAdder2) &&& arr (snd . snd) >>>
  arr (\((c, s), s') -> (c, halfMerge (s, s')))
