{-# LANGUAGE BangPatterns, CPP #-}
module Check.BenchX
    ( addSmallLoop
    , addBigLoop
    , timesSmallLoop
    , timesSmallBigLoop
    , timesBigLoop
    ) where

#define BenchX   1

import Common.GHC.Integer.Prim

import qualified NewX.GHC.Integer as X

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


timesSmallLoop :: Int -> X.Integer
timesSmallLoop iter =
    loop iter count value
  where
    loop :: Int -> Int -> X.Integer -> X.Integer
    loop !0 !0 !accum = accum
    loop !k !0 !_ = loop (k - 1) count value
    loop !k !j !accum = loop k (j - 1) (X.timesInteger accum value)

    value = X.smallInteger 3#
    count = 32      -- 3 ^ 32 < 0x7fffffffffffffff


timesSmallBigLoop :: Int -> X.Integer
timesSmallBigLoop iter =
    loop iter count value
  where
    loop :: Int -> Int -> X.Integer -> X.Integer
    loop !0 !0 !accum = accum
    loop !k !0 !_ = loop (k - 1) count value
    loop !k !j !accum = loop k (j - 1) (X.timesInteger accum value)

    value = X.smallInteger (unboxInt maxBound)
    count = 10


timesBigLoop :: Int -> X.Integer
timesBigLoop iter =
    loop iter count value
  where
    loop :: Int -> Int -> X.Integer -> X.Integer
    loop !0 !0 !accum = accum
    loop !k !0 !_ = loop (k - 1) count value
    loop !k !j !accum = loop k (j - 1) (X.timesInteger accum value)

    value = X.mkInteger True [ 0x300 .. 0x3ff ]
    count = 10
