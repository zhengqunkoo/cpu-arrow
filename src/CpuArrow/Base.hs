module CpuArrow.Base where

import Control.Arrow
import Control.Category

data Seq a =
  Cons a (Seq a)

instance Functor Seq where
  fmap f (Cons a as) = Cons (f a) $ fmap f as

class Zippable f where
  zipTo :: (f a, f b) -> f (a, b)
  zipFrom :: f (a, b) -> (f a, f b)

instance Zippable Seq where
  zipTo (Cons a as, Cons b bs) = Cons (a, b) $ zipTo (as, bs)
  zipFrom (Cons (a, b) ts) = (Cons a as, Cons b bs)
    where
      (as, bs) = zipFrom ts

newtype Circuit b c =
  Circuit
    { unCircuit :: Seq b -> Seq c
    }

instance Category Circuit where
  id = Circuit Prelude.id
  (.) (Circuit f) (Circuit g) = Circuit $ (Prelude..) f g

instance Arrow Circuit where
  arr f = Circuit $ fmap f
  first (Circuit f) = Circuit $ zipTo `dot` (\(b, d) -> (f b, d)) `dot` zipFrom
    where
      dot = (Control.Category..)

on :: Seq Bool
on = Cons True on

off :: Seq Bool
off = Cons False off

runCircuit :: Circuit b c -> Seq b -> Seq c
runCircuit cir bs = f bs
  where
    f = unCircuit cir
