{-# LANGUAGE CPP, MagicHash, ForeignFunctionInterface, NoImplicitPrelude,
             BangPatterns, UnboxedTuples, UnliftedFFITypes #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}


#include "MachDeps.h"

module New4.GHC.Integer.Internals
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

import New4.GHC.Integer.Natural
import New4.GHC.Integer.Sign
import New4.GHC.Integer.StrictPrim
import New4.GHC.Integer.Type
import New4.GHC.Integer.WordArray

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

{-# NOINLINE smallInteger #-}
smallInteger :: Int# -> Integer
smallInteger i
    | isTrue# (i ==# 0#) = zeroInteger
    | isTrue# (i <# 0#) = Negative (NatS (W# (int2Word# (negateInt# i))))
    | otherwise = Positive (NatS (W# (int2Word# i)))

{-# NOINLINE wordToInteger #-}
wordToInteger :: Word# -> Integer
wordToInteger w = Positive (NatS (W# w))

{-# NOINLINE integerToWord #-}
integerToWord :: Integer -> Word#
integerToWord (Positive n) = unboxWord (naturalToWord n)
integerToWord (Negative n) = unboxWord (naturalToWord n)

{-# NOINLINE integerToInt #-}
integerToInt :: Integer -> Int#
integerToInt (Positive (NatS (W# x))) = word2Int# x
integerToInt (Negative (NatS (W# x))) = negateInt# (word2Int# x)
integerToInt (Positive (NatB _ arr)) = firstWordAsInt Pos arr
integerToInt (Negative (NatB _ arr)) = firstWordAsInt Neg arr

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
integerToWord64 = error ("New4/GHC/Integer/Internals.hs: line " ++ show (__LINE__ :: Int))

{-# NOINLINE word64ToInteger #-}
word64ToInteger:: Word64# -> Integer
word64ToInteger = error ("New4/GHC/Integer/Internals.hs: line " ++ show (__LINE__ :: Int))

{-# NOINLINE integerToInt64 #-}
integerToInt64 :: Integer -> Int64#
integerToInt64 = error ("New4/GHC/Integer/Internals.hs: line " ++ show (__LINE__ :: Int))

{-# NOINLINE int64ToInteger #-}
int64ToInteger :: Int64# -> Integer
int64ToInteger = error ("New4/GHC/Integer/Internals.hs: line " ++ show (__LINE__ :: Int))
#else
#error WORD_SIZE_IN_BITS not supported
#endif

{-# NOINLINE encodeDoubleInteger #-}
encodeDoubleInteger :: Integer -> Int# -> Double#
encodeDoubleInteger (Positive n) s = encodeDoubleNatural n s
encodeDoubleInteger (Negative n) s = negateDouble# (encodeDoubleNatural n s)

{-# NOINLINE decodeDoubleInteger #-}
decodeDoubleInteger :: Double# -> (# Integer, Int# #)
decodeDoubleInteger d =
    case decodeDouble_2Int# d of
        (# mantSign, mantHigh, mantLow, expn #) ->
            let !signf = if isTrue# (mantSign ># 0#) then Positive else Negative
            in  (# signf (NatS (W# (plusWord# mantLow (uncheckedShiftL# mantHigh 32#)))), expn #)

{-# NOINLINE encodeFloatInteger #-}
encodeFloatInteger :: Integer -> Int# -> Float#
encodeFloatInteger = error ("New4/GHC/Integer/Internals.hs: line " ++ show (__LINE__ :: Int))

{-# NOINLINE decodeFloatInteger #-}
decodeFloatInteger :: Float# -> (# Integer, Int# #)
decodeFloatInteger = error ("New4/GHC/Integer/Internals.hs: line " ++ show (__LINE__ :: Int))

{-# NOINLINE doubleFromInteger #-}
doubleFromInteger :: Integer -> Double#
doubleFromInteger = error ("New4/GHC/Integer/Internals.hs: line " ++ show (__LINE__ :: Int))

{-# NOINLINE floatFromInteger #-}
floatFromInteger :: Integer -> Float#
floatFromInteger = error ("New4/GHC/Integer/Internals.hs: line " ++ show (__LINE__ :: Int))

{-# NOINLINE andInteger #-}
andInteger :: Integer -> Integer -> Integer
andInteger !(Positive (NatS 0)) _ = zeroInteger
andInteger _ !(Positive (NatS 0)) = zeroInteger
andInteger (Positive a) (Positive b) = fromNatural Pos (andNatural a b)
andInteger _ _ = error ("New4/GHC/Integer/Internals.hs: line " ++ show (__LINE__ :: Int))

{-# NOINLINE orInteger #-}
orInteger :: Integer -> Integer -> Integer
orInteger (Positive a) (Positive b) = Positive (orNatural a b)

orInteger _ _ = error ("New4/GHC/Integer/Internals.hs: line " ++ show (__LINE__ :: Int))

{-# NOINLINE xorInteger #-}
xorInteger :: Integer -> Integer -> Integer
xorInteger (Positive (NatB n1 arr1)) (Positive (NatB n2 arr2)) = Positive (xorArray n1 arr1 n2 arr2)

xorInteger _ _ = error ("New4/GHC/Integer/Internals.hs: line " ++ show (__LINE__ :: Int))


{-# NOINLINE complementInteger #-}
complementInteger :: Integer -> Integer
complementInteger !(Positive !a) = fromNatural Neg (plusNaturalW a 1)
complementInteger !(Negative !a) = fromNatural Pos (minusNaturalW a 1)


{-# NOINLINE shiftLInteger #-}
shiftLInteger :: Integer -> Int# -> Integer
shiftLInteger !a 0# = a
shiftLInteger !(Positive !a) !b = fromNatural Pos (shiftLNatural a (I# b))
shiftLInteger !(Negative !a) !b = fromNatural Neg (shiftLNatural a (I# b))

smallShiftLArray :: Int -> WordArray -> (# Int, Int #) -> Natural
smallShiftLArray !n !arr (# !si, !sj #) = runStrictPrim $ do
    marr <- newWordArray (succ n)
    nlen <- loop marr 0 0
    narr <- unsafeFreezeWordArray marr
    return $! NatB nlen narr
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
    return $! NatB (n + q) narr
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
    return $! NatB nlen narr
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
shiftRInteger !(Positive !(NatS !0)) _ = zeroInteger
shiftRInteger !(Positive !a) !b = fromNatural Pos (shiftRNatural a (I# b))
shiftRInteger !(Negative !a) !b = do
    let nat = shiftRNatural (minusNaturalW a 1) (I# b)
    case nat of
        NatS _ -> fromNatural Neg (plusNaturalW nat 1)
        NatB !nx _ ->
            if nx == 0
                then minusOneInteger
                else fromNatural Neg (plusNaturalW nat 1)

{-# NOINLINE negateInteger #-}
negateInteger :: Integer -> Integer
negateInteger !(Positive !a) = Negative a
negateInteger !(Negative !a) = Positive a

{-# NOINLINE plusInteger #-}
plusInteger :: Integer -> Integer -> Integer
plusInteger !x !y = case (# x, y #) of
    (# !Positive !a, !Positive !b #) -> Positive (plusNatural a b)
    (# !Positive !a, !Negative !b #) -> plusMinusNatural a b
    (# !Negative !a, !Positive !b #) -> plusMinusNatural b a
    (# !Negative !a, !Negative !b #) -> Negative (plusNatural a b)


{-# NOINLINE plusMinusNatural #-}
plusMinusNatural :: Natural -> Natural -> Integer
plusMinusNatural !a !b =
    case compareNatural a b of
        EQ -> zeroInteger
        GT -> fromNatural Pos (minusNatural a b)
        LT -> fromNatural Neg (minusNatural b a)


{-# INLINE safeMinusWord #-}
safeMinusWord :: Word -> Word -> Integer
safeMinusWord !a !b =
    case compare a b of
        EQ -> zeroInteger
        GT -> Positive (NatS (a - b))
        LT -> Negative (NatS (b - a))

{-# INLINE minusInteger #-}
minusInteger :: Integer -> Integer -> Integer
minusInteger !x !y = case (# x, y #) of
    (# !Positive !a, !Positive !b #) -> plusMinusNatural a b
    (# !Positive !a, !Negative !b #) -> Positive (plusNatural a b)
    (# !Negative !a, !Positive !b #) -> Negative (plusNatural a b)
    (# !Negative !a, !Negative !b #) -> plusMinusNatural b a




{-# NOINLINE timesInteger #-}
timesInteger :: Integer -> Integer -> Integer
timesInteger !x !y = case (# x, y #) of
    (# Positive a, Positive b #) -> Positive (timesNatural a b)
    (# Positive a, Negative b #) -> Negative (timesNatural a b)

    (# Negative a, Positive b #) -> Negative (timesNatural a b)
    (# Negative a, Negative b #) -> Positive (timesNatural a b)


{- divModInteger should be implemented in terms of quotRemInteger -}
{-# NOINLINE divModInteger #-}
divModInteger :: Integer -> Integer -> (# Integer, Integer #)
divModInteger (Positive !a) (Positive !b) =
    case quotRemNatural a b of
        (!q, !r) -> (# fromNatural Pos q, fromNatural Pos r #)
divModInteger (Negative !a) (Negative !b) =
    case quotRemNatural a b of
        (!q, NatS 0) -> (# fromNatural Pos q, zeroInteger #)
        (!q, !r) -> (# fromNatural Pos q, fromNatural Neg r #)
divModInteger (Positive !a) (Negative !b) =
    case quotRemNatural a b of
        (!q, NatS 0) -> (# fromNatural Neg q, zeroInteger #)
        (!q, !r) -> (# fromNatural Neg (plusNaturalW q 1), fromNatural Neg (minusNatural b r) #)
divModInteger (Negative !a) (Positive !b) =
    case quotRemNatural a b of
        (!q, NatS 0) ->  (# fromNatural Neg q, zeroInteger #)
        (!q, !r) -> (# fromNatural Neg (plusNaturalW q 1), fromNatural Pos (minusNatural b r) #)

divInteger :: Integer -> Integer -> Integer
divInteger a b =
    case divModInteger a b of
        (# d, _ #) -> d


{-# NOINLINE quotRemInteger #-}
quotRemInteger :: Integer -> Integer -> (# Integer, Integer #)
quotRemInteger (Positive !a) (Positive !b) =
    case quotRemNatural a b of
        (!q, !r) -> (# fromNatural Pos q, fromNatural Pos r #)
quotRemInteger (Positive !a) (Negative !b) =
    case quotRemNatural a b of
        (!q, !r) -> (# fromNatural Neg q, fromNatural Pos r #)
quotRemInteger (Negative !a) (Positive !b) =
    case quotRemNatural a b of
        (!q, NatS 0) -> (# fromNatural Neg q, zeroInteger #)
        (!q, !r) -> (# fromNatural Neg q, fromNatural Neg r #)
quotRemInteger (Negative !a) (Negative !b) =
    case quotRemNatural a b of
        (!q, NatS 0) -> (# fromNatural Pos q, zeroInteger #)
        (!q, !r) -> (# fromNatural Pos q, fromNatural Neg r #)

{-# NOINLINE quotInteger #-}
quotInteger :: Integer -> Integer -> Integer
quotInteger a b =
    case quotRemInteger a b of
        (# q, _ #) -> q


{-# NOINLINE remInteger #-}
remInteger :: Integer -> Integer -> Integer
remInteger = error ("New4/GHC/Integer/Internals.hs: line " ++ show (__LINE__ :: Int))

{-# NOINLINE compareInteger #-}
compareInteger :: Integer -> Integer -> Ordering
compareInteger = error ("New4/GHC/Integer/Internals.hs: line " ++ show (__LINE__ :: Int))

{-# NOINLINE eqInteger #-}
eqInteger :: Integer -> Integer -> Bool
eqInteger !(Positive !a) !(Positive !b) = eqNatural a b
eqInteger !(Negative !a) !(Negative !b) = eqNatural a b
eqInteger !(Positive _) !(Negative _) = False
eqInteger !(Negative _) !(Positive _) = False

{-# NOINLINE neqInteger #-}
neqInteger :: Integer -> Integer -> Bool
neqInteger !a !b = not (eqInteger a b)

instance  Eq Integer  where
    (==) = eqInteger
    (/=) = neqInteger

{-# NOINLINE ltInteger #-}
ltInteger :: Integer -> Integer -> Bool
ltInteger !(Positive _) !(Negative _) = False
ltInteger !(Negative _) !(Positive _) = True
ltInteger !(Positive !a) !(Positive !b) = ltNatural a b
ltInteger !(Negative !a) !(Negative !b) = ltNatural b a

{-# NOINLINE gtInteger #-}
gtInteger :: Integer -> Integer -> Bool
gtInteger !(Positive _) !(Negative _) = True
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
absInteger !a@(Positive _) = a
absInteger !(Negative !a) = Positive a

{-# NOINLINE signumInteger #-}
signumInteger :: Integer -> Integer
signumInteger = error ("New4/GHC/Integer/Internals.hs: line " ++ show (__LINE__ :: Int))

{-# NOINLINE hashInteger #-}
hashInteger :: Integer -> Int#
hashInteger = integerToInt

--------------------------------------------------------------------------------
-- Helpers (not part of the API).

{-# INLINE fromSmall #-}
fromSmall :: Sign -> Word -> Integer
fromSmall !s !w
    | w == 0 = zeroInteger
    | s == Pos = Positive (NatS w)
    | otherwise = Negative (NatS w)

{-# INLINE fromNatural #-}
fromNatural :: Sign -> Natural -> Integer
fromNatural _ !(NatS 0) = zeroInteger
fromNatural !s !nat@(NatS _)
    | s == Pos = Positive nat
    | otherwise = Negative nat

fromNatural !s !nat@(NatB n arr)
    | n == 0 = zeroInteger
    | n == 1 && indexWordArray arr 0 == 0 = zeroInteger -- TODO: See if this can be removed.
    | s == Pos = Positive nat
    | otherwise = Negative nat

zeroInteger, oneInteger, minusOneInteger :: Integer
zeroInteger = Positive (NatS 0)
oneInteger = Positive (NatS 1)
minusOneInteger = Negative (NatS 0)


toList :: Integer -> [Word]
toList ii =
    case ii of
        Positive nat -> natList nat
        Negative nat -> natList nat
  where
    natList (NatS x) = [x]
    natList (NatB n arr) = unpackArray 0
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
isSmall (Positive n) = isSmallNatural n
isSmall (Negative n) = isSmallNatural n

errorLine :: Int -> String -> a
errorLine linenum s = error $ "Line " ++ show linenum ++ ": " ++ s

isMinimal :: Integer -> Bool
isMinimal i =
    case i of
        Positive a -> isMinimalNatural a
        Negative a -> isMinimalNatural a

#endif