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


    , toList, mkNatural

    ) where
-}
    where

import Prelude hiding (Integer, abs, pi, sum, rem) -- (all, error, otherwise, return, show, succ, (++))

import Data.Bits

import GHC.Prim
import GHC.Types
import GHC.Tuple ()
#if WORD_SIZE_IN_BITS < 64
import GHC.IntWord64
#endif

import Numeric (showHex) -- TODO: Remove when its working.
import Debug.Trace

import New3.GHC.Integer.Natural
import New3.GHC.Integer.Prim
import New3.GHC.Integer.Sign
import New3.GHC.Integer.StrictPrim
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

mkNatural :: [Int] -> Natural
mkNatural ws =
    case mkInteger True ws of
        Positive a -> a
        Negative a -> a
        SmallPos x -> mkSingletonNat x
        SmallNeg x -> mkSingletonNat x

{-# NOINLINE smallInteger #-}
smallInteger :: Int# -> Integer
smallInteger i
    | isTrue# (i ==# 0#) = zeroInteger
    | isTrue# (i <# 0#) = SmallNeg (W# (int2Word# (negateInt# i)))
    | otherwise = SmallPos (W# (int2Word# i))

{-# NOINLINE wordToInteger #-}
wordToInteger :: Word# -> Integer
wordToInteger w = SmallPos (W# w)

{-# NOINLINE integerToWord #-}
integerToWord :: Integer -> Word#
integerToWord (SmallPos (W# w)) = w
integerToWord (SmallNeg (W# w)) = w
integerToWord (Positive (Natural _ arr)) = unboxWord (indexWordArray arr 0)
integerToWord (Negative (Natural _ arr)) = unboxWord (indexWordArray arr 0)

{-# NOINLINE integerToInt #-}
integerToInt :: Integer -> Int#
integerToInt (SmallPos (W# w)) = word2Int# w
integerToInt (SmallNeg (W# w)) = negateInt# (word2Int# w)
integerToInt (Positive (Natural _ arr)) = firstWordAsInt Pos arr
integerToInt (Negative (Natural _ arr)) = firstWordAsInt Neg arr

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
encodeDoubleInteger (SmallPos (W# w)) i = encodeDouble# w i
encodeDoubleInteger (SmallNeg (W# w)) i = negateDouble# (encodeDouble# w i)

encodeDoubleInteger (Positive n) s = encodeDoubleNatural n s
encodeDoubleInteger (Negative n) s = negateDouble# (encodeDoubleNatural n s)

{-# NOINLINE decodeDoubleInteger #-}
decodeDoubleInteger :: Double# -> (# Integer, Int# #)
decodeDoubleInteger d =
    case decodeDouble_2Int# d of
        (# mantSign, mantHigh, mantLow, expn #) ->
            let !signf = if isTrue# (mantSign ># 0#) then SmallPos else SmallNeg
            in  (# signf (W# (plusWord# mantLow (uncheckedShiftL# mantHigh 32#)))
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
andInteger !(SmallPos 0) _ = zeroInteger
andInteger _ !(SmallPos 0) = zeroInteger

andInteger (SmallPos a) (SmallPos b) = fromSmall Pos (a .&. b)
andInteger (SmallPos a) (SmallNeg b) = fromSmall Pos (a .&. complement (b - 1))
andInteger (SmallNeg a) (SmallPos b) = fromSmall Pos (complement (a - 1) .&. b)
andInteger (SmallNeg a) (SmallNeg b) = fromSmall Neg (1 + ((a - 1) .|. (b - 1)))

andInteger (SmallPos a) (Positive b) = fromSmall Pos (a .&. zerothWordOfNatural b)
andInteger (Positive a) (SmallPos b) = fromSmall Pos (zerothWordOfNatural a .&. b)


andInteger (Positive a) (Positive b) = fromNatural Pos (andNatural a b)
andInteger _ _ = error ("New3/GHC/Integer/Internals.hs: line " ++ show (__LINE__ :: Int))

{-# NOINLINE orInteger #-}
orInteger :: Integer -> Integer -> Integer
orInteger (SmallPos a) (SmallPos b) = SmallPos (a .|. b)
orInteger (SmallPos a) (SmallNeg b) = SmallNeg (1 + (complement a .&. (b - 1)))
orInteger (SmallNeg a) (SmallPos b) = SmallNeg (1 + ((a - 1) .&. complement b))
orInteger (SmallNeg a) (SmallNeg b) = SmallNeg (1 + ((a - 1) .&. (b - 1)))

orInteger (SmallPos a) (Positive b) = Positive (orNaturalW b a)
orInteger (Positive a) (SmallPos b) = Positive (orNaturalW a b)

orInteger (Positive a) (Positive b) = Positive (orNatural a b)

orInteger _ _ = error ("New3/GHC/Integer/Internals.hs: line " ++ show (__LINE__ :: Int))

{-# NOINLINE xorInteger #-}
xorInteger :: Integer -> Integer -> Integer
xorInteger (Positive (Natural n1 arr1)) (Positive (Natural n2 arr2)) = Positive (xorArray n1 arr1 n2 arr2)

xorInteger _ _ = error ("New3/GHC/Integer/Internals.hs: line " ++ show (__LINE__ :: Int))


{-# NOINLINE complementInteger #-}
complementInteger :: Integer -> Integer
complementInteger !(SmallPos !a) = fromSmall Neg (a + 1)
complementInteger !(SmallNeg !a) = fromSmall Pos (a - 1)
complementInteger !(Positive !a) = fromNatural Neg (plusNaturalW a 1)
complementInteger !(Negative !a) = fromNatural Pos (minusNaturalW a 1)


{-# NOINLINE shiftLInteger #-}
shiftLInteger :: Integer -> Int# -> Integer
shiftLInteger !a 0# = a
shiftLInteger !(SmallPos !a) b
    | a == 0 = zeroInteger
    | isTrue# (b >=# WORD_SIZE_IN_BITS#) = fromNatural Pos (shiftLNatural (mkSingletonNat a) (I# b))
    | otherwise =
        let !lo = unsafeShiftL a (I# b)
            hi = unsafeShiftR a (I# ( WORD_SIZE_IN_BITS# -# b))
        in if hi == 0
            then SmallPos lo
            else Positive (mkPair lo hi)

shiftLInteger !(SmallNeg !a) !b = fromNatural Neg (shiftLNatural (mkSingletonNat a) (I# b))
shiftLInteger !(Positive !a) !b = fromNatural Pos (shiftLNatural a (I# b))
shiftLInteger !(Negative !a) !b = fromNatural Neg (shiftLNatural a (I# b))

smallShiftLArray :: Int -> WordArray -> (# Int, Int #) -> Natural
smallShiftLArray !n !arr (# !si, !sj #) = runStrictPrim $ do
    marr <- newWordArray (succ n)
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
shiftRInteger !(SmallPos 0) _ = zeroInteger
shiftRInteger !a 0# = a
shiftRInteger !(SmallPos !a) !b
    | isTrue# (b >=# WORD_SIZE_IN_BITS#) = zeroInteger
    | otherwise = fromSmall Pos (shiftRWord a (I# b))
shiftRInteger !(SmallNeg !a) !b
    | isTrue# (b >=# WORD_SIZE_IN_BITS#) = SmallNeg 1
    | otherwise = fromSmall Neg ((shiftRWord (a - 1) (I# b)) + 1)

shiftRInteger !(Positive !a) !b = fromNatural Pos (shiftRNatural a (I# b))
shiftRInteger !(Negative !a) !b =
    let !nat@(Natural !nx _) = shiftRNatural (minusNaturalW a 1) (I# b)
    in if nx == 0
        then SmallNeg 1
        else fromNatural Neg (plusNaturalW nat 1)

{-# NOINLINE negateInteger #-}
negateInteger :: Integer -> Integer
negateInteger !(SmallPos 0) = SmallPos 0
negateInteger !(SmallPos !a) = SmallNeg a
negateInteger !(SmallNeg !a) = SmallPos a
negateInteger !(Positive !a) = Negative a
negateInteger !(Negative !a) = Positive a

{-# NOINLINE plusInteger #-}
plusInteger :: Integer -> Integer -> Integer
plusInteger !x !y = case (# x, y #) of
    (# !SmallPos !a, !SmallPos !b #) -> safePlusWord Pos a b
    (# !SmallPos !a, !SmallNeg !b #) -> safeMinusWord a b
    (# !SmallNeg !a, !SmallPos !b #) -> safeMinusWord b a
    (# !SmallNeg !a, !SmallNeg !b #) -> safePlusWord Neg a b

    (# SmallPos 0, b #) -> b
    (# a, SmallPos 0 #) -> a

    (# !SmallPos !a, !Positive !b #) -> Positive (plusNaturalW b a)
    (# !SmallPos !a, !Negative !b #) -> Negative (minusNaturalW b a)
    (# !SmallNeg !a, !Positive !b #) -> Positive (minusNaturalW b a)
    (# !SmallNeg !a, !Negative !b #) -> Negative (plusNaturalW b a)

    (# !Positive !a, !SmallPos !b #) -> Positive (plusNaturalW a b)
    (# !Positive !a, !SmallNeg !b #) -> Positive (minusNaturalW a b)
    (# !Positive !a, !Positive !b #) -> Positive (plusNatural a b)
    (# !Positive !a, !Negative !b #) -> plusMinusNatural a b

    (# !Negative !a, !SmallPos !b #) -> Negative (minusNaturalW a b)
    (# !Negative !a, !SmallNeg !b #) -> Negative (plusNaturalW a b)
    (# !Negative !a, !Positive !b #) -> plusMinusNatural b a
    (# !Negative !a, !Negative !b #) -> Negative (plusNatural a b)


{-# NOINLINE plusMinusNatural #-}
plusMinusNatural :: Natural -> Natural -> Integer
plusMinusNatural !a !b =
    case compareNatural a b of
        EQ -> zeroInteger
        GT -> fromNatural Pos (minusNatural a b)
        LT -> fromNatural Neg (minusNatural b a)

{-# INLINE safePlusWord #-}
safePlusWord :: Sign -> Word -> Word -> Integer
safePlusWord !sign !w1 !w2 =
    let (# !c, !s #) = plusWord2 w1 w2
    in case (# c == 0, sign #) of
        (# True, Pos #) -> SmallPos s
        (# True, Neg #) -> SmallNeg s
        (# False, Pos #) -> Positive (mkPair s c)
        (# False, Neg #) -> Negative (mkPair s c)

{-# INLINE safeMinusWord #-}
safeMinusWord :: Word -> Word -> Integer
safeMinusWord !a !b =
    case compare a b of
        EQ -> zeroInteger
        GT -> SmallPos (a - b)
        LT -> SmallNeg (b - a)

{-# INLINE minusInteger #-}
minusInteger :: Integer -> Integer -> Integer
minusInteger !x !y = case (# x, y #) of
    (# !SmallPos !a, !SmallPos !b #) -> safeMinusWord a b
    (# !SmallPos !a, !SmallNeg !b #) -> safePlusWord Pos a b
    (# !SmallNeg !a, !SmallPos !b #) -> safePlusWord Neg a b
    (# !SmallNeg !a, !SmallNeg !b #) -> safeMinusWord b a

    (# SmallPos 0, b #) -> negateInteger b
    (# a, SmallPos 0 #) -> a

    (# !SmallPos !a, !Positive !b #) -> Negative (minusNaturalW b a)
    (# !SmallPos !a, !Negative !b #) -> Positive (plusNaturalW b a)
    (# !SmallNeg !a, !Positive !b #) -> Negative (plusNaturalW b a)
    (# !SmallNeg !a, !Negative !b #) -> Positive (minusNaturalW b a)

    (# !Positive !a, !SmallPos !b #) -> Positive (minusNaturalW a b)
    (# !Positive !a, !SmallNeg !b #) -> Positive (plusNaturalW a b)
    (# !Positive !a, !Positive !b #) -> plusMinusNatural a b
    (# !Positive !a, !Negative !b #) -> Positive (plusNatural a b)

    (# !Negative !a, !SmallPos !b #) -> Negative (plusNaturalW a b)
    (# !Negative !a, !SmallNeg !b #) -> Negative (minusNaturalW a b)
    (# !Negative !a, !Positive !b #) -> Negative (plusNatural a b)
    (# !Negative !a, !Negative !b #) -> plusMinusNatural b a

{-# NOINLINE timesInteger #-}
timesInteger :: Integer -> Integer -> Integer
timesInteger !x !y = case (# x, y #) of
    (# SmallPos 0, _ #) -> zeroInteger
    (# _, SmallPos 0 #) -> zeroInteger

    (# SmallPos a, SmallPos b #) -> safeTimesWord Pos a b
    (# SmallPos a, SmallNeg b #) -> safeTimesWord Neg a b
    (# SmallNeg a, SmallPos b #) -> safeTimesWord Neg a b
    (# SmallNeg a, SmallNeg b #) -> safeTimesWord Pos a b

    (# SmallPos a, Positive b #) -> Positive (timesNaturalW b a)
    (# SmallPos a, Negative b #) -> Negative (timesNaturalW b a)
    (# SmallNeg a, Positive b #) -> Negative (timesNaturalW b a)
    (# SmallNeg a, Negative b #) -> Positive (timesNaturalW b a)

    (# Positive a, SmallPos b #) -> Positive (timesNaturalW a b)
    (# Positive a, SmallNeg b #) -> Negative (timesNaturalW a b)
    (# Positive a, Positive b #) -> Positive (timesNatural a b)
    (# Positive a, Negative b #) -> Negative (timesNatural a b)

    (# Negative a, SmallPos b #) -> Negative (timesNaturalW a b)
    (# Negative a, SmallNeg b #) -> Positive (timesNaturalW a b)
    (# Negative a, Positive b #) -> Negative (timesNatural a b)
    (# Negative a, Negative b #) -> Positive (timesNatural a b)


{-# INLINE safeTimesWord #-}
safeTimesWord :: Sign -> Word -> Word -> Integer
safeTimesWord !sign !w1 !w2 =
    let (# !ovf, !prod #) = timesWord2 w1 w2
    in case (# ovf == 0, sign #) of
        (# False, Pos #) -> Positive (mkPair prod ovf)
        (# False, Neg #) -> Negative (mkPair prod ovf)
        (# True, Pos #) -> SmallPos prod
        (# True, Neg #) -> SmallNeg prod


{- divModInteger should be implemented in terms of quotRemInteger -}
{-# NOINLINE divModInteger #-}
divModInteger :: Integer -> Integer -> (# Integer, Integer #)
divModInteger _ (SmallPos 0) = error ("New3/GHC/Integer/Internals.hs: line " ++ show (__LINE__ :: Int) ++ " divide by zero")
divModInteger (SmallPos 0) (SmallPos _) = (# zeroInteger, zeroInteger #)
divModInteger (SmallPos !a) (SmallPos !b) = let (!q, !r) = divMod a b in (# if q == 0 then zeroInteger else SmallPos q, if r == 0 then zeroInteger else SmallPos r #)
divModInteger (SmallNeg !a) (SmallNeg !b) = let (!q, !r) = divMod a b in (# if q == 0 then zeroInteger else SmallPos q, if r == 0 then zeroInteger else SmallNeg r #)
divModInteger (SmallPos !a) (SmallNeg !b) =
    let (!q, !r) = divMod a b
    in if r == 0
        then (# SmallNeg q, zeroInteger #)
        else (# SmallNeg (q + 1), SmallNeg (b - r) #)
divModInteger (SmallNeg !a) (SmallPos !b) =
    let (!q, !r) = divMod a b
    in case (q == 0, r == 0) of
        ( False, False )    -> (# SmallNeg (q + 1), SmallPos (b - r) #)
        ( True, False )     -> (# SmallNeg 1, SmallPos (b - r) #)
        ( False, True )     -> (# SmallNeg q, zeroInteger #)
        _                   -> (# SmallPos 3, SmallPos 3 #)

divModInteger _ _ = error ("New3/GHC/Integer/Internals.hs: line " ++ show (__LINE__ :: Int))


divInteger :: Integer -> Integer -> Integer
divInteger a b =
    case divModInteger a b of
        (# d, _ #) -> d


{-# NOINLINE quotRemInteger #-}
quotRemInteger :: Integer -> Integer -> (# Integer, Integer #)
quotRemInteger _ (SmallPos 0) = error ("New3/GHC/Integer/Internals.hs: line " ++ show (__LINE__ :: Int) ++ " divide by zero")
quotRemInteger (SmallPos 0) (SmallPos _) = (# zeroInteger, zeroInteger #)
quotRemInteger (SmallPos !a) (SmallPos !b) = let (# !q, !r #) = quotRemWord a b in (# if q == 0 then zeroInteger else SmallPos q, if r == 0 then zeroInteger else SmallPos r #)
quotRemInteger (SmallNeg !a) (SmallNeg !b) = let (# !q, !r #) = quotRemWord a b in (# if q == 0 then zeroInteger else SmallPos q, if r == 0 then zeroInteger else SmallNeg r #)
quotRemInteger (SmallPos !a) (SmallNeg !b) = let (# !q, !r #) = quotRemWord a b in (# if q == 0 then zeroInteger else SmallNeg q, if r == 0 then zeroInteger else SmallPos r #)

quotRemInteger (SmallNeg !a) (SmallPos !b) =
    let (# !q, !r #) = quotRemWord a b
    in case (q == 0, r == 0) of
        ( False, False )    -> (# SmallNeg q, SmallNeg r #)
        ( True, False )     -> (# zeroInteger, SmallNeg r #)
        ( False, True )     -> (# SmallNeg q, zeroInteger #)
        _                   -> (# SmallPos 3, SmallPos 3 #)

quotRemInteger (Positive !a) (SmallPos !b) = let (!q, !r) = quotRemNaturalW a b in (# fromNatural Pos q, if r == 0 then zeroInteger else SmallPos r #)
quotRemInteger (Negative !a) (SmallPos !b) = let (!q, !r) = quotRemNaturalW a b in (# fromNatural Neg q, if r == 0 then zeroInteger else SmallNeg r #)
quotRemInteger (Positive !a) (SmallNeg !b) = let (!q, !r) = quotRemNaturalW a b in (# fromNatural Neg q, if r == 0 then zeroInteger else SmallPos r #)
quotRemInteger (Negative !a) (SmallNeg !b) = let (!q, !r) = quotRemNaturalW a b in (# fromNatural Pos q, if r == 0 then zeroInteger else SmallNeg r #)

quotRemInteger (Positive !a) (Positive !b) = let (!q, !r) = quotRemNatural a b in (# fromNatural Pos q, fromNatural Pos r #)
quotRemInteger (Positive !a) (Negative !b) = let (!q, !r) = quotRemNatural a b in (# fromNatural Neg q, fromNatural Pos r #)
quotRemInteger (Negative !a) (Positive !b) = let (!q, !r) = quotRemNatural a b in (# fromNatural Neg q, fromNatural Neg r #)
quotRemInteger (Negative !a) (Negative !b) = let (!q, !r) = quotRemNatural a b in (# fromNatural Pos q, fromNatural Neg r #)

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
eqInteger !(SmallPos !a) !(SmallPos !b) = a == b
eqInteger !(SmallNeg !a) !(SmallNeg !b) = b == a
eqInteger !(Positive !a) !(Positive !b) = eqNatural a b
eqInteger !(Negative !a) !(Negative !b) = eqNatural a b

eqInteger !(SmallPos _) !(SmallNeg _) = False
eqInteger !(SmallPos _) !(Positive _) = False
eqInteger !(SmallPos _) !(Negative _) = False

eqInteger !(Positive _) !(SmallPos _) = False
eqInteger !(Positive _) !(SmallNeg _) = False
eqInteger !(Positive _) !(Negative _) = False

eqInteger !(SmallNeg _) _ = False
eqInteger !(Negative _) _ = False

{-# NOINLINE neqInteger #-}
neqInteger :: Integer -> Integer -> Bool
neqInteger !a !b = not (eqInteger a b)

instance  Eq Integer  where
    (==) = eqInteger
    (/=) = neqInteger

{-# NOINLINE ltInteger #-}
ltInteger :: Integer -> Integer -> Bool
ltInteger !(SmallPos !a) !(SmallPos !b) = a < b
ltInteger !(SmallNeg !a) !(SmallNeg !b) = b < a
ltInteger !(SmallPos _) !(SmallNeg _) = False
ltInteger !(SmallNeg _) !(SmallPos _) = True

ltInteger !(SmallPos _) !(Positive _) = True
ltInteger !(SmallPos _) !(Negative _) = False
ltInteger !(SmallNeg _) !(Positive _) = True
ltInteger !(SmallNeg _) !(Negative _) = False

ltInteger !(Positive _) !(SmallPos _) = False
ltInteger !(Positive _) !(SmallNeg _) = False
ltInteger !(Positive _) !(Negative _) = False

ltInteger !(Negative _) !(SmallPos _) = True
ltInteger !(Negative _) !(SmallNeg _) = True
ltInteger !(Negative _) !(Positive _) = True

ltInteger !(Positive !a) !(Positive !b) = ltNatural a b
ltInteger !(Negative !a) !(Negative !b) = ltNatural b a

{-# NOINLINE gtInteger #-}
gtInteger :: Integer -> Integer -> Bool
gtInteger !(SmallPos !a) !(SmallPos !b) = a > b
gtInteger !(SmallNeg !a) !(SmallNeg !b) = a < b
gtInteger !(SmallPos _) !(SmallNeg _) = True
gtInteger !(SmallNeg _) !(SmallPos _) = False

gtInteger !(SmallPos _) !(Positive _) = False
gtInteger !(SmallPos _) !(Negative _) = True
gtInteger !(SmallNeg _) !(Positive _) = False
gtInteger !(SmallNeg _) !(Negative _) = True

gtInteger !(Positive _) !(SmallPos _) = True
gtInteger !(Positive _) !(SmallNeg _) = True
gtInteger !(Positive _) !(Negative _) = True

gtInteger !(Negative _) !(SmallPos _) = False
gtInteger !(Negative _) !(SmallNeg _) = False
gtInteger !(Negative _) !(Positive _) = False

gtInteger !(Positive !a) !(Positive !b) = gtNatural a b
gtInteger !(Negative !a) !(Negative !b) = gtNatural b a

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
absInteger !a@(Positive _) = a
absInteger !(Negative !a) = Positive a

{-# NOINLINE signumInteger #-}
signumInteger :: Integer -> Integer
signumInteger = error ("New3/GHC/Integer/Internals.hs: line " ++ show (__LINE__ :: Int))

{-# NOINLINE hashInteger #-}
hashInteger :: Integer -> Int#
hashInteger = integerToInt

--------------------------------------------------------------------------------
-- Helpers (not part of the API).

{-# INLINE fromSmall #-}
fromSmall :: Sign -> Word -> Integer
fromSmall !s !w
    | w == 0 = zeroInteger
    | s == Pos = SmallPos w
    | otherwise = SmallNeg w

{-# INLINE fromNatural #-}
fromNatural :: Sign -> Natural -> Integer
fromNatural !s !nat@(Natural n arr)
    | n == 0 = zeroInteger
    | n == 1 && indexWordArray arr 0 == 0 = zeroInteger -- TODO: See if this can be removed.
    | s == Pos = Positive nat
    | otherwise = Negative nat

zeroInteger, oneInteger, minusOneInteger :: Integer
zeroInteger = SmallPos 0
oneInteger = SmallPos 1
minusOneInteger = SmallNeg 1


toList :: Integer -> [Word]
toList ii =
    case ii of
        SmallPos w -> [w]
        SmallNeg w -> [w]
        Positive nat -> natList nat
        Negative nat -> natList nat
  where
    natList (Natural n arr) = unpackArray 0
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


hexShowW :: Word -> String
hexShowW w = "0x" ++ showHex w ""

signShow :: Sign -> String
signShow Pos = "Pos"
signShow Neg = "Neg"

absInt :: Int -> Int
absInt x = if x < 0 then -x else x

debugPrint :: Int -> String -> StrictPrim s ()
debugPrint line s = trace (show line ++ " : " ++ s) $ return ()


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
        SmallNeg a -> a /= 0
        Positive a -> isMinimalNatural a
        Negative a -> isMinimalNatural a
  where
    isMinimalNatural (Natural 0 _) = False
    isMinimalNatural (Natural n arr) = indexWordArray arr (n - 1) /= 0

#endif
