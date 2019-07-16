module CpuArrow.Base where

import Control.Arrow
import Control.Category

newtype Circuit b c =
  Circuit
    { unCircuit :: b -> c
    }

instance Category Circuit where
  id = Circuit Prelude.id
  (.) (Circuit f) (Circuit g) = Circuit $ (Prelude..) f g

instance Arrow Circuit where
  arr f = Circuit f
  first (Circuit f) =
    Circuit $
    (\(b, d) ->
       let c = f b
        in (c, d))

runCircuit :: Circuit b c -> b -> c
runCircuit cir b = f b
  where
    f = unCircuit cir

instance ArrowApply Circuit where
  app = arr $ uncurry runCircuit
