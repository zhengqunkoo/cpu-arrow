{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

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

type T2 a = (a, a)

type T4 a = (a, a, a, a)

type T8 a = (a, a, a, a, a, a, a, a)

type T16 a = (a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a)

class Half a b where
  halfFst :: a -> b
  halfSnd :: a -> b

instance Half (T2 a) a where
  halfFst = fst
  halfSnd = snd

instance Half (T4 a) (T2 a) where
  halfFst (a1, a2, a3, a4) = (a1, a2)
  halfSnd (a1, a2, a3, a4) = (a3, a4)

instance Half (T8 a) (T4 a) where
  halfFst (a1, a2, a3, a4, a5, a6, a7, a8) = (a1, a2, a3, a4)
  halfSnd (a1, a2, a3, a4, a5, a6, a7, a8) = (a5, a6, a7, a8)

instance Half (T16 a) (T8 a) where
  halfFst (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16) =
    (a1, a2, a3, a4, a5, a6, a7, a8)
  halfSnd (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16) =
    (a9, a10, a11, a12, a13, a14, a15, a16)

on :: [Bool]
on = True : on

off :: [Bool]
off = False : off

runCircuit :: Circuit b c -> [b] -> [c]
runCircuit cir bs = f bs
  where
    f = unCircuit cir
