module CpuArrow.Base where

import Control.Arrow
import Control.Category

newtype Circuit b c =
  Circuit
    { unCircuit :: [b] -> [c]
    }

instance Category Circuit where
  id = Circuit Prelude.id
  (.) (Circuit f) (Circuit g) = Circuit $ (Prelude..) f g

instance Arrow Circuit where
  arr f = Circuit $ fmap f
  first (Circuit f) =
    Circuit $ uncurry zip `dot` (\(b, d) -> (f b, d)) `dot` unzip
    where
      dot = (Control.Category..)

on :: [Bool]
on = True : on

off :: [Bool]
off = False : off

runCircuit :: Circuit b c -> [b] -> [c]
runCircuit cir bs = f bs
  where
    f = unCircuit cir
