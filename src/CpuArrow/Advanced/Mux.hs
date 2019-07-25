module CpuArrow.Advanced.Mux where

import Control.Arrow
import CpuArrow.Base
import CpuArrow.BoolAlgebra
import CpuArrow.Half
import CpuArrow.Logic

aMux :: BoolAlgebra a => Circuit (T2 a, a) a
aMux =
  (first (arr snd) >>> aAnd) &&& (first (arr fst) >>> second aNot >>> aAnd) >>>
  aOr

aMux2 :: BoolAlgebra a => Circuit (T4 a, (a, a)) a
aMux2 =
  ((second (arr snd) >>>
    ((first (arr halfFst) >>> aMux) &&& (first (arr halfSnd) >>> aMux))) &&&
   second (arr fst)) >>>
  (second (arr snd) >>> aMux)

aMux3 :: BoolAlgebra a => Circuit (T8 a, (a, a, a)) a
aMux3 =
  ((second (arr (\(_, a, b) -> (a, b))) >>>
    ((first (arr halfFst) >>> aMux2) &&& (first (arr halfSnd) >>> aMux2))) &&&
   second (arr (\(a, _, _) -> a))) >>>
  (second (arr snd) >>> aMux)
