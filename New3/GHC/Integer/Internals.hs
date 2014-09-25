{-# LANGUAGE CPP, MagicHash, ForeignFunctionInterface, NoImplicitPrelude,
             BangPatterns, UnboxedTuples, UnliftedFFITypes #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}


#include "MachDeps.h"

module New3.GHC.Integer.Internals
{-
    ( Integer (..)
    , mkInteger, smallInteger, wordToInteger, integerToWord, integerToInt
#if WORD_SIZE_IN_BITS < 64
    , integerToWord64, word64ToInteger
    , integerToInt64, int64ToInteger
#endif
    , plusInteger, minusInteger, timesInteger, negateInteger
    , eqInteger, neqInteger, absInteger, signumInteger
    , leInteger, gtInteger, ltInteger, geInteger, compareInteger
    , divModInteger, quotRemInteger, quotInteger, remInteger
    , encodeFloatInteger, decodeFloatInteger, floatFromInteger
    , encodeDoubleInteger, decodeDoubleInteger, doubleFromInteger
    -- , gcdInteger, lcmInteger -- XXX
    , andInteger, orInteger, xorInteger, complementInteger
    , shiftLInteger, shiftRInteger
    , hashInteger


    , toList

    ) where
-}
    where

import Prelude hiding (Integer, abs, pi, sum, rem, succ) -- (all, error, otherwise, return, show, (++))

import Data.Bits

import GHC.Prim
import GHC.Types
import GHC.Tuple ()
#if WORD_SIZE_IN_BITS < 64
import GHC.IntWord64
#endif

import Numeric (showHex) -- TODO: Remove when its working.

import Common.GHC.Integer.Prim
import Common.GHC.Integer.StrictPrim
import New3.GHC.Integer.Natural
import New3.GHC.Integer.Sign
import New3.GHC.Integer.Type
import New3.GHC.Integer.WordArray

#if !defined(__HADDOCK__)

--------------------------------------------------------------------------------

mkInteger :: Bool   -- non-negative?
          -> [Int]  -- absolute value in 31 bit chunks, least significant first
                    -- ideally these would be Words rather than Ints, but
                    -- we don't have Word available at the moment.
          -> Integer
mkInteger _ [] = zeroInteger
mkInteger True [I# i] = smallInteger i
mkInteger False [I# i] = smallInteger (negateInt# i)
mkInteger nonNegative is =
    let abs = f is
    in if nonNegative
        then abs
        else negateInteger abs
  where
    f [] = zeroInteger
    f [I# x] = smallInteger x
    f (I# x : xs) = smallInteger x `orInteger` shiftLInteger (f xs) 31#

{-# NOINLINE smallInteger #-}
smallInteger :: Int# -> Integer
smallInteger i
    | isTrue# (i ==# 0#) = zeroInteger
    | isTrue# (i <# 0#) = SmallNeg (int2Word# (negateInt# i))
    | otherwise = SmallPos (int2Word# i)

{-# NOINLINE wordToInteger #-}
wordToInteger :: Word# -> Integer
wordToInteger w = SmallPos w

{-# NOINLINE integerToWord #-}
integerToWord :: Integer -> Word#
integerToWord (SmallPos w) = w
integerToWord (SmallNeg w) = w
integerToWord (Positive _ arr) = unboxWord (indexWordArray arr 0)
integerToWord (Negative _ arr) = unboxWord (indexWordArray arr 0)

{-# NOINLINE integerToInt #-}
integerToInt :: Integer -> Int#
integerToInt (SmallPos w) = word2Int# w
integerToInt (SmallNeg w) = negateInt# (word2Int# w)
integerToInt (Positive _ arr) = firstWordAsInt Pos arr
integerToInt (Negative _ arr) = firstWordAsInt Neg arr

firstWordAsInt :: Sign -> WordArray -> Int#
firstWordAsInt s arr =
    let i = word2Int# (unboxWord (indexWordArray arr 0))
    in case s of
        Pos -> i
        Neg -> negateInt# i

#if WORD_SIZE_IN_BITS == 64
-- Nothing
#elif WORD_SIZE_IN_BITS == 32
{-# NOINLINE integerToWord64 #-}
integerToWord64 :: Integer -> Word64#
integerToWord64 = error ("New3/GHC/Integer/Internals.hs: line " ++ show (__LINE__ :: Int))

{-# NOINLINE word64ToInteger #-}
word64ToInteger:: Word64# -> Integer
word64ToInteger = error ("New3/GHC/Integer/Internals.hs: line " ++ show (__LINE__ :: Int))

{-# NOINLINE integerToInt64 #-}
integerToInt64 :: Integer -> Int64#
integerToInt64 = error ("New3/GHC/Integer/Internals.hs: line " ++ show (__LINE__ :: Int))

{-# NOINLINE int64ToInteger #-}
int64ToInteger :: Int64# -> Integer
int64ToInteger = error ("New3/GHC/Integer/Internals.hs: line " ++ show (__LINE__ :: Int))
#else
#error WORD_SIZE_IN_BITS not supported
#endif

{-# NOINLINE encodeDoubleInteger #-}
encodeDoubleInteger :: Integer -> Int# -> Double#
encodeDoubleInteger (SmallPos w) i = encodeDouble# w i
encodeDoubleInteger (SmallNeg w) i = negateDouble# (encodeDouble# w i)

encodeDoubleInteger (Positive n arr) s = encodeDoubleNatural (Natural n arr) s
encodeDoubleInteger (Negative n arr) s = negateDouble# (encodeDoubleNatural (Natural n arr) s)

{-# NOINLINE decodeDoubleInteger #-}
decodeDoubleInteger :: Double# -> (# Integer, Int# #)
decodeDoubleInteger d =
    case decodeDouble_2Int# d of
        (# mantSign, mantHigh, mantLow, expn #) ->
            let !signf = if isTrue# (mantSign ># 0#) then SmallPos else SmallNeg
            in  (# signf (plusWord# mantLow (uncheckedShiftL# mantHigh 32#))
                , expn #)

{-# NOINLINE encodeFloatInteger #-}
encodeFloatInteger :: Integer -> Int# -> Float#
encodeFloatInteger = error ("New3/GHC/Integer/Internals.hs: line " ++ show (__LINE__ :: Int))

{-# NOINLINE decodeFloatInteger #-}
decodeFloatInteger :: Float# -> (# Integer, Int# #)
decodeFloatInteger = error ("New3/GHC/Integer/Internals.hs: line " ++ show (__LINE__ :: Int))

{-# NOINLINE doubleFromInteger #-}
doubleFromInteger :: Integer -> Double#
doubleFromInteger = error ("New3/GHC/Integer/Internals.hs: line " ++ show (__LINE__ :: Int))

{-# NOINLINE floatFromInteger #-}
floatFromInteger :: Integer -> Float#
floatFromInteger = error ("New3/GHC/Integer/Internals.hs: line " ++ show (__LINE__ :: Int))

{-# NOINLINE andInteger #-}
andInteger :: Integer -> Integer -> Integer
andInteger (SmallPos a) (SmallPos b) = fromSmall SmallPos (boxWord# (and# a b))
andInteger (SmallPos a) (SmallNeg b) = fromSmall SmallPos (boxWord# (and# a (not# (minusWord# b 1##))))
andInteger (SmallNeg a) (SmallPos b) = fromSmall SmallPos (boxWord# (not# (and# (minusWord# a 1##) b)))
andInteger (SmallNeg a) (SmallNeg b) = fromSmall SmallNeg (boxWord# (plusWord# (or# (minusWord# a 1##) (minusWord# b 1##)) 1##))

andInteger !(SmallPos 0##) _ = zeroInteger
andInteger _ !(SmallPos 0##) = zeroInteger

andInteger (SmallPos a) (Positive n arr) = fromSmall SmallPos ((W# a) .&. zerothWordOfNatural (Natural n arr))
andInteger (Positive n arr) (SmallPos b) = fromSmall SmallPos (zerothWordOfNatural (Natural n arr) .&. (W# b))


andInteger (Positive n1 arr1) (Positive n2 arr2) = fromNatural Pos (andNatural (Natural n1 arr1) (Natural n2 arr2))
andInteger _ _ = error ("New3/GHC/Integer/Internals.hs: line " ++ show (__LINE__ :: Int))

{-# NOINLINE orInteger #-}
orInteger :: Integer -> Integer -> Integer
orInteger (SmallPos a) (SmallPos b) = SmallPos (or# a b)
orInteger (SmallPos a) (SmallNeg b) = SmallNeg (plusWord# 1## (and# (not# a) (minusWord# b 1##)))
orInteger (SmallNeg a) (SmallPos b) = SmallNeg (plusWord# 1## (and# (minusWord# a 1##) (not# b)))
orInteger (SmallNeg a) (SmallNeg b) = SmallNeg (plusWord# 1## (and# (minusWord# a 1##) (minusWord# b 1##)))

orInteger (SmallPos a) (Positive n arr) = fromNatural Pos (orNaturalW (Natural n arr) (W# a))
orInteger (Positive n arr) (SmallPos b) = fromNatural Pos (orNaturalW (Natural n arr) (W# b))

orInteger (Positive n1 arr1) (Positive n2 arr2) = fromNatural Pos (orNatural (Natural n1 arr1) (Natural n2 arr2))

orInteger _ _ = error ("New3/GHC/Integer/Internals.hs: line " ++ show (__LINE__ :: Int))

{-# NOINLINE xorInteger #-}
xorInteger :: Integer -> Integer -> Integer
-- xorInteger (Positive (Natural n1 arr1)) (Positive (Natural n2 arr2)) = Positive (xorArray n1 arr1 n2 arr2)

xorInteger _ _ = error ("New3/GHC/Integer/Internals.hs: line " ++ show (__LINE__ :: Int))


{-# NOINLINE complementInteger #-}
complementInteger :: Integer -> Integer
complementInteger !(SmallPos !a) = fromSmall SmallNeg ((W# a) + 1)
complementInteger !(SmallNeg !a) = fromSmall SmallPos ((W# a) - 1)
complementInteger !(Positive !n !arr) = fromNatural Neg (plusNaturalW (Natural n arr) 1)
complementInteger !(Negative !n !arr) = fromNatural Pos (minusNaturalW (Natural n arr) 1)


{-# NOINLINE shiftLInteger #-}
shiftLInteger :: Integer -> Int# -> Integer
shiftLInteger !a 0# = a
shiftLInteger !(SmallPos !a) b
    | isTrue# (eqWord# a 0##) = zeroInteger
    | isTrue# (b >=# WORD_SIZE_IN_BITS#) = fromNatural Pos (shiftLNatural (mkSingletonNat (W# a)) (I# b))
    | otherwise =
        let !lo = unsafeShiftL (W# a) (I# b)
            hi = unsafeShiftR (W# a) (I# ( WORD_SIZE_IN_BITS# -# b))
        in if hi == 0
            then SmallPos (unboxWord lo)
            else mkPair Positive lo hi

shiftLInteger !(SmallNeg !a) !b = fromNatural Neg (shiftLNatural (mkSingletonNat (W# a)) (I# b))
shiftLInteger !(Positive !n !arr) !b = fromNatural Pos (shiftLNatural (Natural n arr) (I# b))
shiftLInteger !(Negative !n !arr) !b = fromNatural Neg (shiftLNatural (Natural n arr) (I# b))

smallShiftLArray :: Int -> WordArray -> (# Int, Int #) -> Natural
smallShiftLArray !n !arr (# !si, !sj #) = runStrictPrim $ do
    marr <- newWordArray (n + 1)
    nlen <- loop marr 0 0
    narr <- unsafeFreezeWordArray marr
    return $! Natural nlen narr
  where
    loop !marr !i !mem
        | i < n =  do
            x <- indexWordArrayM arr i
            writeWordArray marr i ((unsafeShiftL x si) .|. mem)
            loop marr (i + 1) (unsafeShiftR x sj)
        | mem /= 0 = do
            writeWordArray marr i mem
            return $ i + 1
        | otherwise = return n

-- | TODO : Use copy here? Check benchmark results.
wordShiftLArray :: Int -> WordArray -> Int -> Natural
wordShiftLArray !n !arr !q = runStrictPrim $ do
    marr <- newWordArray (n + q)
    loop1 marr 0
    narr <- unsafeFreezeWordArray marr
    return $! Natural (n + q) narr
  where
    loop1 !marr !i
        | i < q = do
            writeWordArray marr i 0
            loop1 marr (i + 1)
        | otherwise = loop2 marr 0
    loop2 !marr !i
        | i < n =  do
            x <- indexWordArrayM arr i
            writeWordArray marr (q + i) x
            loop2 marr (i + 1)
        | otherwise = return ()

largeShiftLArray :: Int -> WordArray-> (# Int, Int, Int #) -> Natural
largeShiftLArray !n !arr (# !q, !si, !sj #) = runStrictPrim $ do
    marr <- newWordArray (n + q + 1)
    setWordArray marr 0 q 0
    nlen <- loop1 marr 0 0
    narr <- unsafeFreezeWordArray marr
    return $! Natural nlen narr
  where
    loop1 !marr !i !mem
        | i < n =  do
            x <- indexWordArrayM arr i
            writeWordArray marr (q + i) ((unsafeShiftL x si) .|. mem)
            loop1 marr (i + 1) (unsafeShiftR x sj)
        | mem /= 0 = do
            writeWordArray marr (q + i) mem
            return (q + i + 1)
        | otherwise = return (q + i)


{-# NOINLINE shiftRInteger #-}
shiftRInteger :: Integer -> Int# -> Integer
shiftRInteger !(SmallPos 0##) _ = zeroInteger
shiftRInteger !a 0# = a
shiftRInteger !(SmallPos !a) !b
    | isTrue# (b >=# WORD_SIZE_IN_BITS#) = zeroInteger
    | otherwise = fromSmall SmallPos (shiftRWord (boxWord# a) (I# b))
shiftRInteger !(SmallNeg !a) !b
    | isTrue# (b >=# WORD_SIZE_IN_BITS#) = minusOneInteger
    | otherwise = fromSmall SmallNeg ((shiftRWord ((boxWord# a) - 1) (I# b)) + 1)

shiftRInteger !(Positive !n !arr) !b = fromNatural Pos (shiftRNatural (Natural n arr) (I# b))
shiftRInteger !(Negative !n !arr) !b =
    let !nat@(Natural !nx _) = shiftRNatural (minusNaturalW (Natural n arr) 1) (I# b)
    in if nx == 0
        then minusOneInteger
        else fromNatural Neg (plusNaturalW nat 1)

{-# NOINLINE negateInteger #-}
negateInteger :: Integer -> Integer
negateInteger !(SmallPos 0##) = zeroInteger
negateInteger !(SmallPos !a) = SmallNeg a
negateInteger !(SmallNeg !a) = SmallPos a
negateInteger !(Positive !n !arr) = Negative n arr
negateInteger !(Negative !n !arr) = Positive n arr

{-# NOINLINE plusInteger #-}
plusInteger :: Integer -> Integer -> Integer
plusInteger !x !y = case (# x, y #) of
    (# !SmallPos !a, !SmallPos !b #) -> safePlusWord Pos a b
    (# !SmallPos !a, !SmallNeg !b #) -> safeMinusWord a b
    (# !SmallNeg !a, !SmallPos !b #) -> safeMinusWord b a
    (# !SmallNeg !a, !SmallNeg !b #) -> safePlusWord Neg a b

    (# !SmallPos !a, !Positive !n !arr #) -> fromNatural Pos (plusNaturalW (Natural n arr) (W# a))
    (# !SmallPos !a, !Negative !n !arr #) -> fromNatural Neg (minusNaturalW (Natural n arr) (W# a))
    (# !SmallNeg !a, !Positive !n !arr #) -> fromNatural Pos (minusNaturalW (Natural n arr) (W# a))
    (# !SmallNeg !a, !Negative !n !arr #) -> fromNatural Neg (plusNaturalW (Natural n arr) (W# a))

    (# !Positive !n !arr, !SmallPos !b #) -> fromNatural Pos (plusNaturalW (Natural n arr) (W# b))
    (# !Positive !n !arr, !SmallNeg !b #) -> fromNatural Pos (minusNaturalW (Natural n arr) (W# b))
    (# !Negative !n !arr, !SmallPos !b #) -> fromNatural Neg (minusNaturalW (Natural n arr) (W# b))
    (# !Negative !n !arr, !SmallNeg !b #) -> fromNatural Neg (plusNaturalW (Natural n arr) (W# b))

    (# !Positive !n1 !arr1, !Positive !n2 !arr2 #) -> fromNatural Pos (plusNatural (Natural n1 arr1) (Natural n2 arr2))
    (# !Positive !n1 !arr1, !Negative !n2 !arr2 #) -> plusMinusNatural (Natural n1 arr1) (Natural n2 arr2)
    (# !Negative !n1 !arr1, !Positive !n2 !arr2 #) -> plusMinusNatural (Natural n2 arr2) (Natural n1 arr1)
    (# !Negative !n1 !arr1, !Negative !n2 !arr2 #) -> fromNatural Neg (plusNatural (Natural n1 arr1) (Natural n2 arr2))


{-# NOINLINE plusMinusNatural #-}
plusMinusNatural :: Natural -> Natural -> Integer
plusMinusNatural !a !b =
    case compareNatural a b of
        EQ -> zeroInteger
        GT -> fromNatural Pos (minusNatural a b)
        LT -> fromNatural Neg (minusNatural b a)

{-# INLINE safePlusWord #-}
safePlusWord :: Sign -> Word# -> Word# -> Integer
safePlusWord !sign !w1 !w2 =
    case plusWord2# w1 w2 of
        (# 0##, s #) -> case sign of
                        Pos -> SmallPos s
                        Neg -> SmallNeg s
        (# c, s #) -> case sign of
                        Pos -> mkPair Positive (W# s) (W# c)
                        Neg -> mkPair Negative (W# s) (W# c)

{-# INLINE safeMinusWord #-}
safeMinusWord :: Word# -> Word# -> Integer
safeMinusWord !a !b =
    case isTrue# (geWord# a b) of
        True -> SmallPos (minusWord# a b)
        False -> SmallNeg (minusWord# b a)

{-# INLINE minusInteger #-}
minusInteger :: Integer -> Integer -> Integer
minusInteger !x !y = case (# x, y #) of
    (# !SmallPos !a, !SmallPos !b #) -> safeMinusWord a b
    (# !SmallPos !a, !SmallNeg !b #) -> safePlusWord Pos a b
    (# !SmallNeg !a, !SmallPos !b #) -> safePlusWord Neg a b
    (# !SmallNeg !a, !SmallNeg !b #) -> safeMinusWord b a

    (# !SmallPos !a, !Positive !n !arr #) -> fromNatural Neg (minusNaturalW (Natural n arr) (W# a))
    (# !SmallPos !a, !Negative !n !arr #) -> fromNatural Pos (plusNaturalW (Natural n arr) (W# a))
    (# !SmallNeg !a, !Positive !n !arr #) -> fromNatural Neg (plusNaturalW (Natural n arr) (W# a))
    (# !SmallNeg !a, !Negative !n !arr #) -> fromNatural Pos (minusNaturalW (Natural n arr) (W# a))

    (# !Positive !n !arr, !SmallPos !b #) -> fromNatural Pos (minusNaturalW (Natural n arr) (W# b))
    (# !Positive !n !arr, !SmallNeg !b #) -> fromNatural Pos (plusNaturalW (Natural n arr) (W# b))
    (# !Negative !n !arr, !SmallPos !b #) -> fromNatural Neg (plusNaturalW (Natural n arr) (W# b))
    (# !Negative !n !arr, !SmallNeg !b #) -> fromNatural Neg (minusNaturalW (Natural n arr) (W# b))

    (# !Positive !n1 !arr1, !Positive !n2 !arr2 #) -> plusMinusNatural (Natural n1 arr1) (Natural n2 arr2)
    (# !Positive !n1 !arr1, !Negative !n2 !arr2 #) -> fromNatural Pos (plusNatural (Natural n1 arr1) (Natural n2 arr2))
    (# !Negative !n1 !arr1, !Positive !n2 !arr2 #) -> fromNatural Neg (plusNatural (Natural n1 arr1) (Natural n2 arr2))
    (# !Negative !n1 !arr1, !Negative !n2 !arr2 #) -> plusMinusNatural (Natural n2 arr2) (Natural n1 arr1)

{-# NOINLINE timesInteger #-}
timesInteger :: Integer -> Integer -> Integer
timesInteger !x !y = case (# x, y #) of
    (# SmallPos a, SmallPos b #) -> safeTimesWord Pos a b
    (# SmallPos a, SmallNeg b #) -> safeTimesWord Neg a b
    (# SmallNeg a, SmallPos b #) -> safeTimesWord Neg a b
    (# SmallNeg a, SmallNeg b #) -> safeTimesWord Pos a b

    (# SmallPos 0##, _ #) -> zeroInteger
    (# _, SmallPos 0## #) -> zeroInteger

    (# SmallPos a, Positive n arr #) -> fromNatural Pos (timesNaturalW (Natural n arr) (W# a))
    (# SmallPos a, Negative n arr #) -> fromNatural Neg (timesNaturalW (Natural n arr) (W# a))
    (# SmallNeg a, Positive n arr #) -> fromNatural Neg (timesNaturalW (Natural n arr) (W# a))
    (# SmallNeg a, Negative n arr #) -> fromNatural Pos (timesNaturalW (Natural n arr) (W# a))

    (# Positive n arr, SmallPos b #) -> fromNatural Pos (timesNaturalW (Natural n arr) (W# b))
    (# Positive n arr, SmallNeg b #) -> fromNatural Neg (timesNaturalW (Natural n arr) (W# b))
    (# Negative n arr, SmallPos b #) -> fromNatural Neg (timesNaturalW (Natural n arr) (W# b))
    (# Negative n arr, SmallNeg b #) -> fromNatural Pos (timesNaturalW (Natural n arr) (W# b))

    (# Positive n1 arr1, Positive n2 arr2 #) -> fromNatural Pos (timesNatural (Natural n1 arr1) (Natural n2 arr2))
    (# Positive n1 arr1, Negative n2 arr2 #) -> fromNatural Neg (timesNatural (Natural n1 arr1) (Natural n2 arr2))

    (# Negative n1 arr1, Positive n2 arr2 #) -> fromNatural Neg (timesNatural (Natural n1 arr1) (Natural n2 arr2))
    (# Negative n1 arr1, Negative n2 arr2 #) -> fromNatural Pos (timesNatural (Natural n1 arr1) (Natural n2 arr2))


{-# INLINE safeTimesWord #-}
safeTimesWord :: Sign -> Word# -> Word# -> Integer
safeTimesWord !sign !w1 !w2 =
    let (# !ovf, !prod #) = timesWord2 (W# w1) (W# w2)
    in case (# ovf == 0, sign #) of
        (# False, Pos #) -> mkPair Positive prod ovf
        (# False, Neg #) -> mkPair Negative prod ovf
        (# True, Pos #) -> SmallPos (unboxWord prod)
        (# True, Neg #) -> SmallNeg (unboxWord prod)


{- divModInteger should be implemented in terms of quotRemInteger -}
{-# NOINLINE divModInteger #-}
divModInteger :: Integer -> Integer -> (# Integer, Integer #)
divModInteger _ (SmallPos 0##) = error ("New3/GHC/Integer/Internals.hs: line " ++ show (__LINE__ :: Int) ++ " divide by zero")
divModInteger (SmallPos 0##) (SmallPos _) = (# zeroInteger, zeroInteger #)
divModInteger (SmallPos !a) (SmallPos !b) = let (!q, !r) = divMod (W# a) (W# b) in (# if q == 0 then zeroInteger else SmallPos (unboxWord q), if r == 0 then zeroInteger else SmallPos (unboxWord r) #)
divModInteger (SmallNeg !a) (SmallNeg !b) = let (!q, !r) = divMod (W# a) (W# b) in (# if q == 0 then zeroInteger else SmallPos (unboxWord q), if r == 0 then zeroInteger else SmallNeg (unboxWord r) #)
divModInteger (SmallPos !a) (SmallNeg !b) =
    let (!q, !r) = divMod (W# a) (W# b)
    in if r == 0
        then (# SmallNeg (unboxWord q), zeroInteger #)
        else (# SmallNeg (unboxWord (q + 1)), SmallPos (unboxWord ((W# b) - r)) #)
divModInteger (SmallNeg !a) (SmallPos !b) =
    let (!q, !r) = divMod (W# a) (W# b)
    in case (q == 0, r == 0) of
        ( False, False )    -> (# SmallNeg (unboxWord (q + 1)), SmallPos (unboxWord ((W# b) - r)) #)
        ( True, False )     -> (# SmallNeg 1##, SmallPos (unboxWord ((W# b) - r)) #)
        ( False, True )     -> (# SmallNeg (unboxWord q), zeroInteger #)
        _                   -> (# SmallPos 3##, SmallPos 3## #)

divModInteger _ _ = error ("New3/GHC/Integer/Internals.hs: line " ++ show (__LINE__ :: Int))


divInteger :: Integer -> Integer -> Integer
divInteger a b =
    case divModInteger a b of
        (# d, _ #) -> d


{-# NOINLINE quotRemInteger #-}
quotRemInteger :: Integer -> Integer -> (# Integer, Integer #)
quotRemInteger _ (SmallPos 0##) = error ("New3/GHC/Integer/Internals.hs: line " ++ show (__LINE__ :: Int) ++ " divide by zero")
quotRemInteger (SmallPos 0##) (SmallPos _) = (# zeroInteger, zeroInteger #)
quotRemInteger (SmallPos !a) (SmallPos !b) = let (# !q, !r #) = quotRemWord (W# a) (W# b) in (# if q == 0 then zeroInteger else SmallPos (unboxWord q), if r == 0 then zeroInteger else SmallPos (unboxWord r) #)
quotRemInteger (SmallNeg !a) (SmallNeg !b) = let (# !q, !r #) = quotRemWord (W# a) (W# b) in (# if q == 0 then zeroInteger else SmallPos (unboxWord q), if r == 0 then zeroInteger else SmallNeg (unboxWord r) #)
quotRemInteger (SmallPos !a) (SmallNeg !b) = let (# !q, !r #) = quotRemWord (W# a) (W# b) in (# if q == 0 then zeroInteger else SmallNeg (unboxWord q), if r == 0 then zeroInteger else SmallPos (unboxWord r) #)

quotRemInteger (SmallNeg !a) (SmallPos !b) =
    let (# !q, !r #) = quotRemWord (W# a) (W# b)
    in case (q == 0, r == 0) of
        ( False, False )    -> (# SmallNeg (unboxWord q), SmallNeg (unboxWord r) #)
        ( True, False )     -> (# zeroInteger, SmallNeg (unboxWord r) #)
        ( False, True )     -> (# SmallNeg (unboxWord q), zeroInteger #)
        _                   -> (# SmallPos 3##, SmallPos 3## #)

quotRemInteger (Positive n arr) (SmallPos !b) = let (!q, !r) = quotRemNaturalW (Natural n arr) (W# b) in (# fromNatural Pos q, if r == 0 then zeroInteger else SmallPos (unboxWord r) #)
quotRemInteger (Negative n arr) (SmallPos !b) = let (!q, !r) = quotRemNaturalW (Natural n arr) (W# b) in (# fromNatural Neg q, if r == 0 then zeroInteger else SmallNeg (unboxWord r) #)
quotRemInteger (Positive n arr) (SmallNeg !b) = let (!q, !r) = quotRemNaturalW (Natural n arr) (W# b) in (# fromNatural Neg q, if r == 0 then zeroInteger else SmallPos (unboxWord r) #)
quotRemInteger (Negative n arr) (SmallNeg !b) = let (!q, !r) = quotRemNaturalW (Natural n arr) (W# b) in (# fromNatural Pos q, if r == 0 then zeroInteger else SmallNeg (unboxWord r) #)

quotRemInteger (Positive n1 arr1) (Positive n2 arr2) = let (!q, !r) = quotRemNatural (Natural n1 arr1) (Natural n2 arr2) in (# fromNatural Pos q, fromNatural Pos r #)
quotRemInteger (Positive n1 arr1) (Negative n2 arr2) = let (!q, !r) = quotRemNatural (Natural n1 arr1) (Natural n2 arr2) in (# fromNatural Neg q, fromNatural Pos r #)
quotRemInteger (Negative n1 arr1) (Positive n2 arr2) = let (!q, !r) = quotRemNatural (Natural n1 arr1) (Natural n2 arr2) in (# fromNatural Neg q, fromNatural Neg r #)
quotRemInteger (Negative n1 arr1) (Negative n2 arr2) = let (!q, !r) = quotRemNatural (Natural n1 arr1) (Natural n2 arr2) in (# fromNatural Pos q, fromNatural Neg r #)

quotRemInteger _ _ = error ("New3/GHC/Integer/Internals.hs: line " ++ show (__LINE__ :: Int))

{-# NOINLINE quotInteger #-}
quotInteger :: Integer -> Integer -> Integer
quotInteger a b =
    case quotRemInteger a b of
        (# q, _ #) -> q


{-# NOINLINE remInteger #-}
remInteger :: Integer -> Integer -> Integer
remInteger = error ("New3/GHC/Integer/Internals.hs: line " ++ show (__LINE__ :: Int))

{-# NOINLINE compareInteger #-}
compareInteger :: Integer -> Integer -> Ordering
compareInteger = error ("New3/GHC/Integer/Internals.hs: line " ++ show (__LINE__ :: Int))

{-# NOINLINE eqInteger #-}
eqInteger :: Integer -> Integer -> Bool
eqInteger !(SmallPos !a) !(SmallPos !b) = isTrue# (eqWord# a b)
eqInteger !(SmallNeg !a) !(SmallNeg !b) = isTrue# (eqWord# a b)
eqInteger !(Positive n1 arr1) !(Positive n2 arr2) = eqNatural (Natural n1 arr1) (Natural n2 arr2)
eqInteger !(Negative n1 arr1) !(Negative n2 arr2) = eqNatural (Natural n1 arr1) (Natural n2 arr2)
eqInteger _ _ = False

{-# NOINLINE neqInteger #-}
neqInteger :: Integer -> Integer -> Bool
neqInteger !a !b = not (eqInteger a b)

instance  Eq Integer  where
    (==) = eqInteger
    (/=) = neqInteger

{-# NOINLINE ltInteger #-}
ltInteger :: Integer -> Integer -> Bool
ltInteger !(SmallPos !a) !(SmallPos !b) = isTrue# (ltWord# a b)
ltInteger !(SmallNeg !a) !(SmallNeg !b) = isTrue# (ltWord# b a)
ltInteger !(SmallPos _) !(SmallNeg _) = False
ltInteger !(SmallNeg _) !(SmallPos _) = True

ltInteger !(SmallPos _) !(Positive _ _) = True
ltInteger !(SmallPos _) !(Negative _ _) = False
ltInteger !(SmallNeg _) !(Positive _ _) = True
ltInteger !(SmallNeg _) !(Negative _ _) = False

ltInteger !(Positive _ _) !(SmallPos _) = False
ltInteger !(Positive _ _) !(SmallNeg _) = False
ltInteger !(Positive _ _) !(Negative _ _) = False

ltInteger !(Negative _ _) !(SmallPos _) = True
ltInteger !(Negative _ _) !(SmallNeg _) = True
ltInteger !(Negative _ _) !(Positive _ _) = True

ltInteger !(Positive n1 arr1) !(Positive n2 arr2) = ltNatural (Natural n1 arr1) (Natural n2 arr2)
ltInteger !(Negative n1 arr1) !(Negative n2 arr2) = ltNatural (Natural n2 arr2) (Natural n1 arr1)

{-# NOINLINE gtInteger #-}
gtInteger :: Integer -> Integer -> Bool
gtInteger !(SmallPos !a) !(SmallPos !b) = isTrue# (gtWord# a b)
gtInteger !(SmallNeg !a) !(SmallNeg !b) = isTrue# (gtWord# b a)
gtInteger !(SmallPos _) !(SmallNeg _) = True
gtInteger !(SmallNeg _) !(SmallPos _) = False

gtInteger !(SmallPos _) !(Positive _ _) = False
gtInteger !(SmallPos _) !(Negative _ _) = True
gtInteger !(SmallNeg _) !(Positive _ _) = False
gtInteger !(SmallNeg _) !(Negative _ _) = True

gtInteger !(Positive _ _) !(SmallPos _) = True
gtInteger !(Positive _ _) !(SmallNeg _) = True
gtInteger !(Positive _ _) !(Negative _ _) = True

gtInteger !(Negative _ _) !(SmallPos _) = False
gtInteger !(Negative _ _) !(SmallNeg _) = False
gtInteger !(Negative _ _) !(Positive _ _) = False

gtInteger !(Positive n1 arr1) !(Positive n2 arr2) = gtNatural (Natural n1 arr1) (Natural n2 arr2)
gtInteger !(Negative n1 arr1) !(Negative n2 arr2) = gtNatural (Natural n2 arr2) (Natural n1 arr1)


leInteger :: Integer -> Integer -> Bool
leInteger !a !b = not (gtInteger a b)

geInteger :: Integer -> Integer -> Bool
geInteger !a !b = not (ltInteger a b)

instance Ord Integer where
    (<=) = leInteger
    (>)  = gtInteger
    (<)  = ltInteger
    (>=) = geInteger
    compare = compareInteger

{-# NOINLINE absInteger #-}
absInteger :: Integer -> Integer
absInteger !a@(SmallPos _) = a
absInteger !(SmallNeg !a) = SmallPos a
absInteger !a@(Positive _ _) = a
absInteger !(Negative n arr) = Positive n arr

{-# NOINLINE signumInteger #-}
signumInteger :: Integer -> Integer
signumInteger (SmallPos _) = oneInteger
signumInteger (SmallNeg _) = minusOneInteger
signumInteger (Positive _ _) = oneInteger
signumInteger (Negative _ _) = minusOneInteger

{-# NOINLINE hashInteger #-}
hashInteger :: Integer -> Int#
hashInteger = integerToInt

--------------------------------------------------------------------------------
-- Helpers (not part of the API).

{-# INLINE fromSmall #-}
fromSmall :: (Word# -> Integer) -> Word -> Integer
fromSmall !ctor !(W# w#)
    | (W# w#) == 0 = zeroInteger
    | otherwise = ctor w#

{-# INLINE fromNatural #-}
fromNatural :: Sign -> Natural -> Integer
fromNatural !s !(Natural !n !arr)
    | n == 0 = zeroInteger
    | n == 1 && indexWordArray arr 0 == 0 = zeroInteger -- TODO: See if this can be removed.
    | s == Pos = Positive n arr
    | otherwise = Negative n arr

mkPair :: (Int -> WordArray -> Integer) -> Word -> Word -> Integer
mkPair !ctor !sm !carry = runStrictPrim mkNatPair
  where
    mkNatPair :: StrictPrim s Integer
    mkNatPair = do
        marr <- newWordArray 2
        writeWordArray marr 0 sm
        writeWordArray marr 1 carry
        narr <- unsafeFreezeWordArray marr
        return $ ctor 2 narr


zeroInteger, oneInteger, minusOneInteger :: Integer
zeroInteger = SmallPos 0##
oneInteger = SmallPos 1##
minusOneInteger = SmallNeg 1##


toList :: Integer -> [Word]
toList ii =
    case ii of
        SmallPos w -> [W# w]
        SmallNeg w -> [W# w]
        Positive n arr -> natList n arr
        Negative n arr -> natList n arr
  where
    natList n arr = unpackArray 0
        where
            unpackArray i
                | i < n = do
                    let xs = unpackArray (i + 1)
                        x = indexWordArray arr i
                    x : xs
                | otherwise = []

arrayShow :: Int -> WordArray -> String
arrayShow !len !arr =
    let hexify w =
            let x = showHex w ""
            in replicate (16 - length x) '0' ++ x
        digits = dropWhile (== '0') . concatMap hexify . reverse $ unpackArray 0
    in if null digits then "0x0" else "0x" ++ digits
  where
    unpackArray i
        | i < len = do
                let xs = unpackArray (i + 1)
                    x = indexWordArray arr i
                x : xs
        | otherwise = []


signShow :: Sign -> String
signShow Pos = "Pos"
signShow Neg = "Neg"

absInt :: Int -> Int
absInt x = if x < 0 then -x else x

debugWriteWordArray :: Int -> MutableWordArray (StrictPrim s) -> Int -> Word -> StrictPrim s ()
# if 0
debugWriteWordArray line marr i x = do
    debugPrint line $ "writing " ++ hexShowW x ++ " at " ++ show i
    writeWordArray marr i x
#else
debugWriteWordArray _ marr i x = writeWordArray marr i x
#endif

isSmall :: Integer -> Bool
isSmall (SmallPos _) = True
isSmall (SmallNeg _) = True
isSmall _ = False

errorLine :: Int -> String -> a
errorLine linenum s = error $ "Line " ++ show linenum ++ ": " ++ s

isMinimal :: Integer -> Bool
isMinimal i =
    case i of
        SmallPos _ -> True
        SmallNeg a -> isTrue# (neWord# a 0##)
        Positive n arr -> isMinimalNatural n arr
        Negative n arr -> isMinimalNatural n arr
  where
    isMinimalNatural 0 _ = False
    isMinimalNatural n arr = indexWordArray arr (n - 1) /= 0

#endif
