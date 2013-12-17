{-# LANGUAGE CPP, MagicHash, ForeignFunctionInterface, NoImplicitPrelude,
             BangPatterns, UnboxedTuples, UnliftedFFITypes #-}


#include "MachDeps.h"

module New.GHC.Integer.Type where

import Prelude (all, error, otherwise, return, show, succ, (++))

import Control.Monad.Primitive
import Data.Bits
import Data.Primitive.Array

import GHC.Classes
import GHC.Prim
import GHC.Types
import GHC.Tuple ()
#if WORD_SIZE_IN_BITS < 64
import GHC.IntWord64
#endif

#if !defined(__HADDOCK__)

data Integer
    = Small
        {-# UNPACK #-} !Int
    | Large !Sign
        {-# UNPACK #-} !Int
        {-# UNPACK #-} !(Array Word)

data Sign = Pos# | Neg#

-------------------------------------------------------------------
-- The hard work is done on positive numbers

mkInteger :: Bool   -- non-negative?
          -> [Int]  -- absolute value in 31 bit chunks, least significant first
                    -- ideally these would be Words rather than Ints, but
                    -- we don't have Word available at the moment.
          -> Integer
mkInteger _ [] = smallInteger 0#
mkInteger True [I# i] = smallInteger i
mkInteger False [I# i] = smallInteger (negateInt# i)
mkInteger nonNegative is =
    let abs = f is
    in if nonNegative
        then abs
        else negateInteger abs
  where
    f [] = smallInteger 0#
    f [I# x] = smallInteger x
    f (x : xs) = mkLarge x `orInteger` shiftLInteger (f xs) 31#

{-# NOINLINE smallInteger #-}
smallInteger :: Int# -> Integer
smallInteger i = Small (I# i)


{-# NOINLINE wordToInteger #-}
wordToInteger :: Word# -> Integer
wordToInteger = error ("New/GHC/Integer/Type.hs: line " ++ show (__LINE__ :: Int))

{-# NOINLINE integerToWord #-}
integerToWord :: Integer -> Word#
integerToWord = error ("New/GHC/Integer/Type.hs: line " ++ show (__LINE__ :: Int))

{-# NOINLINE integerToInt #-}
integerToInt :: Integer -> Int#
integerToInt (Small (I# i)) = i
integerToInt _ = error ("New/GHC/Integer/Type.hs: line " ++ show (__LINE__ :: Int))

#if WORD_SIZE_IN_BITS == 64
-- Nothing
#elif WORD_SIZE_IN_BITS == 32
{-# NOINLINE integerToWord64 #-}
integerToWord64 :: Integer -> Word64#
integerToWord64 = error ("New/GHC/Integer/Type.hs: line " ++ show (__LINE__ :: Int))

{-# NOINLINE word64ToInteger #-}
word64ToInteger:: Word64# -> Integer
word64ToInteger = error ("New/GHC/Integer/Type.hs: line " ++ show (__LINE__ :: Int))

{-# NOINLINE integerToInt64 #-}
integerToInt64 :: Integer -> Int64#
integerToInt64 = error ("New/GHC/Integer/Type.hs: line " ++ show (__LINE__ :: Int))

{-# NOINLINE int64ToInteger #-}
int64ToInteger :: Int64# -> Integer
int64ToInteger = error ("New/GHC/Integer/Type.hs: line " ++ show (__LINE__ :: Int))
#else
#error WORD_SIZE_IN_BITS not supported
#endif

oneInteger :: Integer
oneInteger = Small 1

negativeOneInteger :: Integer
negativeOneInteger = error ("New/GHC/Integer/Type.hs: line " ++ show (__LINE__ :: Int))

twoToTheThirtytwoInteger :: Integer
twoToTheThirtytwoInteger = error ("New/GHC/Integer/Type.hs: line " ++ show (__LINE__ :: Int))

{-# NOINLINE encodeDoubleInteger #-}
encodeDoubleInteger :: Integer -> Int# -> Double#
encodeDoubleInteger = error ("New/GHC/Integer/Type.hs: line " ++ show (__LINE__ :: Int))

{-# NOINLINE encodeFloatInteger #-}
encodeFloatInteger :: Integer -> Int# -> Float#
encodeFloatInteger = error ("New/GHC/Integer/Type.hs: line " ++ show (__LINE__ :: Int))

{-# NOINLINE decodeFloatInteger #-}
decodeFloatInteger :: Float# -> (# Integer, Int# #)
decodeFloatInteger = error ("New/GHC/Integer/Type.hs: line " ++ show (__LINE__ :: Int))

-- XXX This could be optimised better, by either (word-size dependent)
-- using single 64bit value for the mantissa, or doing the multiplication
-- by just building the Digits directly
{-# NOINLINE decodeDoubleInteger #-}
decodeDoubleInteger :: Double# -> (# Integer, Int# #)
decodeDoubleInteger = error ("New/GHC/Integer/Type.hs: line " ++ show (__LINE__ :: Int))

{-# NOINLINE doubleFromInteger #-}
doubleFromInteger :: Integer -> Double#
doubleFromInteger = error ("New/GHC/Integer/Type.hs: line " ++ show (__LINE__ :: Int))

{-# NOINLINE floatFromInteger #-}
floatFromInteger :: Integer -> Float#
floatFromInteger = error ("New/GHC/Integer/Type.hs: line " ++ show (__LINE__ :: Int))

{-# NOINLINE andInteger #-}
andInteger :: Integer -> Integer -> Integer
andInteger = error ("New/GHC/Integer/Type.hs: line " ++ show (__LINE__ :: Int))

{-# NOINLINE orInteger #-}
orInteger :: Integer -> Integer -> Integer
orInteger (Small a) (Small b) = Small ( a .|. b)
orInteger _ _ = error ("New/GHC/Integer/Type.hs: line " ++ show (__LINE__ :: Int))


{-# NOINLINE xorInteger #-}
xorInteger :: Integer -> Integer -> Integer
xorInteger = error ("New/GHC/Integer/Type.hs: line " ++ show (__LINE__ :: Int))

{-# NOINLINE complementInteger #-}
complementInteger :: Integer -> Integer
complementInteger = error ("New/GHC/Integer/Type.hs: line " ++ show (__LINE__ :: Int))

{-# NOINLINE shiftLInteger #-}
shiftLInteger :: Integer -> Int# -> Integer
shiftLInteger (Small a) b = shiftLInteger (mkLarge a) b
shiftLInteger (Large s n arr) b =
    let (!n1, !narr) = unsafeInlinePrim (shiftLArray arr n b)
    in Large s n1 narr

shiftLArray :: Array Word -> Int -> Int# -> IO (Int, Array Word)
shiftLArray arr n i
    | i <# WORD_SIZE_IN_BITS# = do
                narr <- smallShiftLArray arr (# i, WORD_SIZE_IN_BITS# -# i #)
                return (succ n, narr)
    | True = error ("New/GHC/Integer/Type.hs: line " ++ show (__LINE__ :: Int))

smallShiftLArray :: Array Word -> (# Int#, Int# #) -> IO (Array Word)
smallShiftLArray _arr (# _i, _j #) = error ("New/GHC/Integer/Type.hs: line " ++ show (__LINE__ :: Int))

{-
shiftLArray iarr i
    | i <# WORD_SIZE_IN_BITS# = do

    | True =
    = if i >=# WORD_SIZE_IN_BITS#
      then shiftLArray (Some 0## p) (i -# WORD_SIZE_IN_BITS#)
      else smallShiftLArray p i
-}


{-# NOINLINE shiftRInteger #-}
shiftRInteger :: Integer -> Int# -> Integer
shiftRInteger = error ("New/GHC/Integer/Type.hs: line " ++ show (__LINE__ :: Int))

{-# NOINLINE negateInteger #-}
negateInteger :: Integer -> Integer
negateInteger = error ("New/GHC/Integer/Type.hs: line " ++ show (__LINE__ :: Int))

-- Note [Avoid patError]
{-# NOINLINE plusInteger #-}
plusInteger :: Integer -> Integer -> Integer
plusInteger = error ("New/GHC/Integer/Type.hs: line " ++ show (__LINE__ :: Int))

{-# NOINLINE minusInteger #-}
minusInteger :: Integer -> Integer -> Integer
minusInteger = error ("New/GHC/Integer/Type.hs: line " ++ show (__LINE__ :: Int))

{-# NOINLINE timesInteger #-}
timesInteger :: Integer -> Integer -> Integer
timesInteger = error ("New/GHC/Integer/Type.hs: line " ++ show (__LINE__ :: Int))

{-# NOINLINE divModInteger #-}
divModInteger :: Integer -> Integer -> (# Integer, Integer #)
divModInteger = error ("New/GHC/Integer/Type.hs: line " ++ show (__LINE__ :: Int))

{-# NOINLINE quotRemInteger #-}
quotRemInteger :: Integer -> Integer -> (# Integer, Integer #)
quotRemInteger = error ("New/GHC/Integer/Type.hs: line " ++ show (__LINE__ :: Int))

{-# NOINLINE quotInteger #-}
quotInteger :: Integer -> Integer -> Integer
quotInteger = error ("New/GHC/Integer/Type.hs: line " ++ show (__LINE__ :: Int))

{-# NOINLINE remInteger #-}
remInteger :: Integer -> Integer -> Integer
remInteger = error ("New/GHC/Integer/Type.hs: line " ++ show (__LINE__ :: Int))

{-# NOINLINE compareInteger #-}
compareInteger :: Integer -> Integer -> Ordering
compareInteger = error ("New/GHC/Integer/Type.hs: line " ++ show (__LINE__ :: Int))

{-# NOINLINE eqInteger #-}
eqInteger :: Integer -> Integer -> Bool
eqInteger = error ("New/GHC/Integer/Type.hs: line " ++ show (__LINE__ :: Int))

{-# NOINLINE neqInteger #-}
neqInteger :: Integer -> Integer -> Bool
neqInteger = error ("New/GHC/Integer/Type.hs: line " ++ show (__LINE__ :: Int))

instance  Eq Integer  where
    (==) = eqInteger
    (/=) = neqInteger

{-# NOINLINE ltInteger #-}
ltInteger :: Integer -> Integer -> Bool
ltInteger (Small a) (Small b) = a < b
ltInteger _ _ = error ("New/GHC/Integer/Type.hs: line " ++ show (__LINE__ :: Int))

{-# NOINLINE gtInteger #-}
gtInteger :: Integer -> Integer -> Bool
gtInteger = error ("New/GHC/Integer/Type.hs: line " ++ show (__LINE__ :: Int))

{-# NOINLINE leInteger #-}
leInteger :: Integer -> Integer -> Bool
leInteger = error ("New/GHC/Integer/Type.hs: line " ++ show (__LINE__ :: Int))

{-# NOINLINE geInteger #-}
geInteger :: Integer -> Integer -> Bool
geInteger = error ("New/GHC/Integer/Type.hs: line " ++ show (__LINE__ :: Int))

instance Ord Integer where
    (<=) = leInteger
    (>)  = gtInteger
    (<)  = ltInteger
    (>=) = geInteger
    compare = compareInteger

{-# NOINLINE absInteger #-}
absInteger :: Integer -> Integer
absInteger = error ("New/GHC/Integer/Type.hs: line " ++ show (__LINE__ :: Int))

{-# NOINLINE signumInteger #-}
signumInteger :: Integer -> Integer
signumInteger = error ("New/GHC/Integer/Type.hs: line " ++ show (__LINE__ :: Int))

{-# NOINLINE hashInteger #-}
hashInteger :: Integer -> Int#
hashInteger = integerToInt

--------------------------------------------------------------------------------
-- Helpers (not part of the API).

mkLarge :: Int -> Integer
mkLarge (I# i) =
    let !sign = if i <# 0# then Neg# else Pos#
    in Large sign 1 (unsafeInlinePrim (mkArray (W# (int2Word# i))))
  where
    mkArray :: Word -> IO (Array Word)
    mkArray x = do
            arr <- newArray 1 x
            unsafeFreezeArray arr


#endif
