{-# LANGUAGE BangPatterns, CPP #-}
module Check.BenchX where

#define BenchX   1

import Common.GHC.Integer.Prim

import qualified NewX.GHC.Integer.Type as X

#if (Bench3 || Bench4)
import qualified NewX.Integer as X
#endif

-- | addSmallLoop : Benchmarking function to test the speed of addition/
-- subtraction of Integer values where the inputs,t all intermediate and the
-- final values fit in a single machine Word.
--
-- First parameter is a tuple to prevent the possibility of benchmarking the
-- partial application of a function.
addSmallLoop :: (Int, Int, Int) -> X.Integer
addSmallLoop (count, up, down) =
    loop count True (X.smallInteger 42#)
  where
    loop :: Int -> Bool -> X.Integer -> X.Integer
    loop !0 !_ !n = n
    loop !k !True  !n = loop (k - 1) False (X.plusInteger n upInteger)
    loop !k !False !n = loop (k - 1) True  (X.minusInteger n downInteger)

    upInteger :: X.Integer
    upInteger = X.smallInteger (unboxInt up)

    downInteger :: X.Integer
    downInteger = X.smallInteger (unboxInt down)


addBigLoop :: (Int, [Int], [Int]) -> X.Integer
addBigLoop (count, up, down) =
    loop count True (X.timesInteger upInteger (X.smallInteger 3#))
  where
    loop :: Int -> Bool -> X.Integer -> X.Integer
    loop !0 !_ !n = n
    loop !k !True  !n = loop (k - 1) False (X.plusInteger n upInteger)
    loop !k !False !n = loop (k - 1) True  (X.minusInteger n downInteger)

    upInteger :: X.Integer
    upInteger = X.mkInteger True up

    downInteger :: X.Integer
    downInteger = X.mkInteger False down
