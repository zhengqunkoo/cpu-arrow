module CpuArrow.Advanced.Demux where

import Control.Arrow
import CpuArrow.Base
import CpuArrow.BoolAlgebra
import CpuArrow.Half
import CpuArrow.Logic

aDemux :: BoolAlgebra a => Circuit (a, a) (T2 a)
aDemux = aAnd &&& (second aNot >>> aAnd)

aDemux2 :: BoolAlgebra a => Circuit (a, (a, a)) (T4 a)
aDemux2 =
  (second (arr fst) >>> aDemux) &&& arr (snd . snd) >>>
  (first (arr halfFst) >>> aDemux) &&& (first (arr halfSnd) >>> aDemux) >>>
  arr halfMerge

aDemux3 :: BoolAlgebra a => Circuit (a, (a, a, a)) (T8 a)
aDemux3 =
  (second (arr (\(a, _, _) -> a)) >>> aDemux) &&&
  arr (\(_, (_, b, c)) -> (b, c)) >>>
  (first (arr halfFst) >>> aDemux2) &&& (first (arr halfSnd) >>> aDemux2) >>>
  arr halfMerge
