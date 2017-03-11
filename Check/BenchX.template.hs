{-# LANGUAGE BangPatterns, CPP #-}
module Check.BenchX
    ( addSmallLoop
    , addBigLoop
    , timesSmallLoop
    , timesSmallBigLoop
    , timesMediumLoop
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
{-# NOINLINE addSmallLoop #-}
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


{-# NOINLINE addBigLoop #-}
addBigLoop :: ([Int], [Int]) -> X.Integer
addBigLoop (up, down) =
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

    count = 500

{-# NOINLINE  timesSmallLoop #-}
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

{-# NOINLINE  timesSmallBigLoop #-}
timesSmallBigLoop :: Int -> X.Integer
timesSmallBigLoop iter =
    loop iter count value
  where
    loop :: Int -> Int -> X.Integer -> X.Integer
    loop !0 !0 !accum = accum
    loop !k !0 !_ = loop (k - 1) count value
    loop !k !j !accum = loop k (j - 1) (X.timesInteger accum value)

    value = X.smallInteger (unboxInt maxBound)
    count = 100

{-# NOINLINE  timesMediumLoop #-}
timesMediumLoop :: Int -> Int
timesMediumLoop iter =
    loop iter count 0
  where
    loop :: Int -> Int -> Int -> Int
    loop !0 !0 !accum = accum
    loop !k !0 !_ = loop (k - 1) count 0
    loop !k !j !accum = loop k (j - 1) (max accum $ boxInt# (X.integerToInt (test j)))

    test !i = X.timesInteger v1 (X.plusInteger v2 (X.smallInteger (unboxInt i)))

    v1 = X.mkInteger True [1 .. len]
    v2 = X.mkInteger True $ replicate len 0xff

    -- mkInteger takes a list of 31 bit values, but we want `v1` and `v2` to
    -- be exactly 4 machine words in length regardless of machine Word size.
    len = if wordSizeInBits == 64 then 8 else 4

    count = 100

{-# NOINLINE timesBigLoop #-}
timesBigLoop :: Int -> X.Integer
timesBigLoop iter =
    loop iter count value
  where
    loop :: Int -> Int -> X.Integer -> X.Integer
    loop !0 !0 !accum = accum
    loop !k !0 !_ = loop (k - 1) count value
    loop !k !j !accum = loop k (j - 1) (X.timesInteger accum value)

    value = X.mkInteger True [ 0x300 .. 0x3ff ]
    count = 20
