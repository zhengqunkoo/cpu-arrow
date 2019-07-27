{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module CpuArrow.Half where

type T2 a = (a, a)

type T4 a = (a, a, a, a)

type T8 a = (a, a, a, a, a, a, a, a)

type T16 a = (a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a)

class Half a b where
  halfFst :: a -> b
  halfSnd :: a -> b
  halfMerge :: (b, b) -> a

instance Half (T2 a) a where
  halfFst = fst
  halfSnd = snd
  halfMerge = id

instance Half (T4 a) (T2 a) where
  halfFst (a1, a2, a3, a4) = (a1, a2)
  halfSnd (a1, a2, a3, a4) = (a3, a4)
  halfMerge ((a1, a2), (a3, a4)) = (a1, a2, a3, a4)

instance Half (T8 a) (T4 a) where
  halfFst (a1, a2, a3, a4, a5, a6, a7, a8) = (a1, a2, a3, a4)
  halfSnd (a1, a2, a3, a4, a5, a6, a7, a8) = (a5, a6, a7, a8)
  halfMerge ((a1, a2, a3, a4), (a5, a6, a7, a8)) =
    (a1, a2, a3, a4, a5, a6, a7, a8)

instance Half (T16 a) (T8 a) where
  halfFst (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16) =
    (a1, a2, a3, a4, a5, a6, a7, a8)
  halfSnd (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16) =
    (a9, a10, a11, a12, a13, a14, a15, a16)
  halfMerge ((a1, a2, a3, a4, a5, a6, a7, a8), (a9, a10, a11, a12, a13, a14, a15, a16)) =
    (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16)
