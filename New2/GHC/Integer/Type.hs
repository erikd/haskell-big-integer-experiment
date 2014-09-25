{-# LANGUAGE CPP, MagicHash, ForeignFunctionInterface, NoImplicitPrelude,
             BangPatterns, UnboxedTuples, UnliftedFFITypes #-}


#include "MachDeps.h"

module New2.GHC.Integer.Type
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


    , toList, mkLarge

    ) where
-}
    where

import Prelude hiding (Integer, abs, pi, succ) -- (all, error, otherwise, return, show, (++))

import Data.Bits
import Data.Primitive.ByteArray

import GHC.Prim
import GHC.Types
import GHC.Tuple ()
#if WORD_SIZE_IN_BITS < 64
import GHC.IntWord64
#endif

import Numeric (showHex) -- TODO: Remove when its working.

import Common.GHC.Integer.Prim
import Common.GHC.Integer.Loop
import Common.GHC.Integer.StrictPrim
import New2.GHC.Integer.Array
import New2.GHC.Integer.Sign

#if !defined(__HADDOCK__)

data Integer
    = Positive !Natural
    | Negative !Natural

data Natural
    = Small
        {-# UNPACK #-} !Word
    | Large
        {-# UNPACK #-} !Int
        {-# UNPACK #-} !ByteArray

--------------------------------------------------------------------------------

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
    f (I# x : xs) = smallInteger x `orInteger` shiftLInteger (f xs) 31#

{-# NOINLINE smallInteger #-}
smallInteger :: Int# -> Integer
smallInteger i
    | isTrue# (i ==# 0#) = Positive (Small 0)
    | isTrue# (i <# 0#) = Negative (Small (W# (int2Word# (negateInt# i))))
    | otherwise = Positive (Small (W# (int2Word# i)))

{-# NOINLINE wordToInteger #-}
wordToInteger :: Word# -> Integer
wordToInteger w = Positive (Small (W# w))

{-# NOINLINE integerToWord #-}
integerToWord :: Integer -> Word#
integerToWord (Positive (Small (W# w))) = w
integerToWord (Negative (Small (W# w))) = w
integerToWord (Positive (Large _ arr)) = unboxWord (indexWordArray arr 0)
integerToWord (Negative (Large _ arr)) = unboxWord (indexWordArray arr 0)

{-# NOINLINE integerToInt #-}
integerToInt :: Integer -> Int#
integerToInt (Positive (Small (W# w))) = word2Int# w
integerToInt (Negative (Small (W# w))) = negateInt# (word2Int# w)
integerToInt (Positive (Large _ arr)) = firstWordAsInt Pos arr
integerToInt (Negative (Large _ arr)) = firstWordAsInt Neg arr

firstWordAsInt :: Sign -> ByteArray -> Int#
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
integerToWord64 = error ("New2/GHC/Integer/Type.hs: line " ++ show (__LINE__ :: Int))

{-# NOINLINE word64ToInteger #-}
word64ToInteger:: Word64# -> Integer
word64ToInteger = error ("New2/GHC/Integer/Type.hs: line " ++ show (__LINE__ :: Int))

{-# NOINLINE integerToInt64 #-}
integerToInt64 :: Integer -> Int64#
integerToInt64 = error ("New2/GHC/Integer/Type.hs: line " ++ show (__LINE__ :: Int))

{-# NOINLINE int64ToInteger #-}
int64ToInteger :: Int64# -> Integer
int64ToInteger = error ("New2/GHC/Integer/Type.hs: line " ++ show (__LINE__ :: Int))
#else
int64ToInteger = error $ "New2/GHC/Integer/Type.hs: line " ++ show (__LINE__ :: Int) ++ ": WORD_SIZE_IN_BITS not supported"
#endif

{-# NOINLINE encodeDoubleInteger #-}
encodeDoubleInteger :: Integer -> Int# -> Double#
encodeDoubleInteger (Positive n) i = encodeNaturalDouble n i
encodeDoubleInteger (Negative n) i = negateDouble# (encodeNaturalDouble n i)

encodeNaturalDouble :: Natural -> Int# -> Double#
encodeNaturalDouble (Small (W# w)) i = encodeDouble# w i
encodeNaturalDouble (Large n arr) e0 =
    let (!res, _) = runStrictPrim $ intLoopState 0 (n - 1) (0.0, I# e0) $ \ i (D# d, e) -> do
                        (W# w) <- indexWordArrayM arr i
                        return (D# (d +## encodeDouble# w (unboxInt e)), e + WORD_SIZE_IN_BITS)
    in unboxDouble res



{-# NOINLINE encodeFloatInteger #-}
encodeFloatInteger :: Integer -> Int# -> Float#
encodeFloatInteger = error ("New2/GHC/Integer/Type.hs: line " ++ show (__LINE__ :: Int))

{-# NOINLINE decodeFloatInteger #-}
decodeFloatInteger :: Float# -> (# Integer, Int# #)
decodeFloatInteger = error ("New2/GHC/Integer/Type.hs: line " ++ show (__LINE__ :: Int))

{-# NOINLINE decodeDoubleInteger #-}
decodeDoubleInteger :: Double# -> (# Integer, Int# #)
decodeDoubleInteger d# =
    case decodeDouble_2Int# d# of
        (# isign, mantHigh, mantLow, expn #) ->
            let sign = if isTrue# (isign <# 0#) then Negative else Positive
            in (# sign (Small (W# (plusWord# mantLow (uncheckedShiftL# mantHigh 32#)))), expn #)

{-# NOINLINE doubleFromInteger #-}
doubleFromInteger :: Integer -> Double#
doubleFromInteger = error ("New2/GHC/Integer/Type.hs: line " ++ show (__LINE__ :: Int))

{-# NOINLINE floatFromInteger #-}
floatFromInteger :: Integer -> Float#
floatFromInteger = error ("New2/GHC/Integer/Type.hs: line " ++ show (__LINE__ :: Int))

{-# NOINLINE andInteger #-}
andInteger :: Integer -> Integer -> Integer
andInteger (Positive a) (Positive b) = Positive (andNatural a b)

andInteger (Positive (Small a)) (Negative (Small b)) = Positive (Small (a .&. complement (b - 1)))
andInteger (Negative (Small a)) (Positive (Small b)) = Positive (Small (complement (a - 1) .&. b))
andInteger (Negative (Small a)) (Negative (Small b)) = Negative (Small (1 + ((a - 1) .|. (b - 1))))

andInteger _ _ = error ("New2/GHC/Integer/Type.hs: line " ++ show (__LINE__ :: Int))

andNatural :: Natural -> Natural -> Natural
andNatural _ (Small 0) = Small 0
andNatural (Small 0) _ = Small 0
andNatural (Small a) (Small b) = Small (a .&. b)
andNatural (Small w) (Large _ arr) = Small (w .&. indexWordArray arr 0)
andNatural (Large _ arr) (Small w) = Small (w .&. indexWordArray arr 0)
andNatural (Large n1 arr1) (Large n2 arr2) = andArray (min n1 n2) arr1 arr2

andArray :: Int -> ByteArray -> ByteArray -> Natural
andArray n arr1 arr2 = runStrictPrim $ do
    !marr <- newWordArray n
    loop marr 0
    !narr <- unsafeFreezeWordArray marr
    finalizeLarge n narr
  where
    loop !marr !i
        | i < n = do
                !x <- indexWordArrayM arr1 i
                !y <- indexWordArrayM arr2 i
                writeWordArray marr i (x .&. y)
                loop marr (i + 1)
        | otherwise = return ()


{-# NOINLINE orInteger #-}
orInteger :: Integer -> Integer -> Integer
orInteger (Positive a) (Positive b) = Positive (orNatural a b)
orInteger (Positive (Small a)) (Negative (Small b)) = Negative (Small (1 + (complement a .&. (b - 1))))
orInteger (Negative (Small a)) (Positive (Small b)) = Negative (Small (1 + ((a - 1) .&. complement b)))
orInteger (Negative (Small a)) (Negative (Small b)) = Negative (Small (1 + ((a - 1) .&. (b - 1))))
orInteger _ _ = error ("New2/GHC/Integer/Type.hs: line " ++ show (__LINE__ :: Int))

orNatural :: Natural -> Natural -> Natural
orNatural (Small 0) b = b
orNatural a (Small 0) = a
orNatural (Small a) (Small b) = Small (a .|. b)
orNatural (Small w) (Large n arr) = orArrayW n arr w
orNatural (Large n arr) (Small w) = orArrayW n arr w
orNatural (Large n1 arr1) (Large n2 arr2) = orArray n1 arr1 n2 arr2

orArrayW :: Int -> ByteArray -> Word -> Natural
orArrayW n arr w = runStrictPrim $ do
    !marr <- newWordArray n
    copyWordArray marr 1 arr 1 (n - 1)
    !x <- indexWordArrayM arr 0
    writeWordArray marr 0 (w .|. x)
    !narr <- unsafeFreezeWordArray marr
    finalizeLarge n narr

orArray :: Int -> ByteArray -> Int -> ByteArray -> Natural
orArray !n1 !arr1 !n2 !arr2
    | n1 < n2 = orArray n2 arr2 n1 arr1
    | otherwise = runStrictPrim $ do
        !marr <- newWordArray n1
        !nlen <- loop1 marr 0
        !narr <- unsafeFreezeWordArray marr
        finalizeLarge nlen narr
  where
    loop1 !marr !i
        | i < n2 = do
                !x <- indexWordArrayM arr1 i
                !y <- indexWordArrayM arr2 i
                writeWordArray marr i (x .|. y)
                loop1 marr (i + 1)
        | otherwise = loop2 marr i
    loop2 !marr !i
        | i < n1 = do
                -- TODO : Use copyArray here?
                !x <- indexWordArrayM arr1 i
                writeWordArray marr i x
                loop2 marr (i + 1)
        | otherwise = return i

{-# NOINLINE xorInteger #-}
xorInteger :: Integer -> Integer -> Integer
xorInteger a (Positive (Small 0)) = a
xorInteger (Positive (Small 0)) b = b
xorInteger (Positive (Large n1 arr1)) (Positive (Large n2 arr2)) = Positive (xorArray n1 arr1 n2 arr2)

xorInteger _ _ = error ("New2/GHC/Integer/Type.hs: line " ++ show (__LINE__ :: Int))


xorArray :: Int -> ByteArray -> Int -> ByteArray -> Natural
xorArray !n1 !arr1 !n2 !arr2
    | n1 < n2 = xorArray n2 arr2 n1 arr1
    | otherwise = runStrictPrim $ do
        !marr <- newWordArray n1
        loop1 marr 0
        !narr <- unsafeFreezeWordArray marr
        finalizeLarge n1 narr
  where
    loop1 !marr !i
        | i < n2 = do
                !x <- indexWordArrayM arr1 i
                !y <- indexWordArrayM arr2 i
                writeWordArray marr i (xor x y)
                loop1 marr (i + 1)
        | otherwise = loop2 marr i
    loop2 !marr !i
        | i < n1 = do
                -- TODO : Use copyArray here?
                !x <- indexWordArrayM arr1 i
                writeWordArray marr i x
                loop2 marr (i + 1)
        | otherwise = return ()

{-# NOINLINE complementInteger #-}
complementInteger :: Integer -> Integer
complementInteger !(Positive !(Small !a)) = Negative (Small (a + 1))
complementInteger !(Negative !(Small !a)) = Positive (Small (a - 1))
complementInteger !(Positive !(Large !n !arr)) = Negative (plusArrayW n arr 1)
complementInteger !(Negative !(Large !n !arr)) = Positive (minusArrayW n arr 1)


{-# NOINLINE shiftLInteger #-}
shiftLInteger :: Integer -> Int# -> Integer
shiftLInteger a 0# = a
shiftLInteger (Positive a) b = Positive (shiftLNatural a b)
shiftLInteger (Negative a) b = Negative (shiftLNatural a b)

shiftLNatural :: Natural -> Int# -> Natural
shiftLNatural a@(Small 0) _ = a
shiftLNatural a@(Small _) b = shiftLNatural (mkLarge a) b
shiftLNatural (Large !n !arr) b = shiftLArray n arr (I# b)


{-# NOINLINE shiftRInteger #-}
shiftRInteger :: Integer -> Int# -> Integer
shiftRInteger a 0# = a
shiftRInteger (Positive (Small a)) b
    | isTrue# (b >=# WORD_SIZE_IN_BITS#) = Positive (Small 0)
    | otherwise = Positive (Small (shiftRWord a (I# b)))
shiftRInteger (Negative (Small a)) b
    | isTrue# (b >=# WORD_SIZE_IN_BITS#) = Negative (Small 1)
    | otherwise = Negative (Small ((shiftRWord (a - 1) (I# b)) + 1))
shiftRInteger (Positive (Large n arr)) b = Positive (shiftRArray n arr (I# b))
shiftRInteger (Negative (Large n arr)) b = Negative $
    case minusArrayW n arr 1 of
        Small _ -> Small 42 -- Impossible
        Large !n1 !arr1 ->
            case shiftRArray n1 arr1 (I# b) of
                Small a2 -> Small (a2 + 1)
                Large !n2 !arr2 -> plusArrayW n2 arr2 1

{-# NOINLINE negateInteger #-}
negateInteger :: Integer -> Integer
negateInteger (Positive a) = Negative a
negateInteger (Negative a) = Positive a

{-# NOINLINE plusInteger #-}
plusInteger :: Integer -> Integer -> Integer
plusInteger !(Positive a) !(Positive b) = Positive (plusNatural a b)
plusInteger !(Negative a) !(Negative b) = Negative (plusNatural a b)
plusInteger !(Positive a) !(Negative b) =
                if gtNatural a b
                    then Positive (minusNatural a b)
                    else Negative (minusNatural b a)
plusInteger !(Negative a) !(Positive b) =
                if gtNatural a b
                    then Negative (minusNatural a b)
                    else Positive (minusNatural b a)

plusNatural :: Natural -> Natural -> Natural
plusNatural !x !y =
    case (# x, y #) of
        (# Small 0, b #) -> b
        (# Small !w1, Small !w2 #) -> safePlusWord w1 w2
        (# Large !n !arr, Small !w #) -> plusArrayW n arr w
        (# Small !w, Large !n !arr #) -> plusArrayW n arr w
        (# Large !n1 !arr1, Large !n2 !arr2 #) -> plusArray n1 arr1 n2 arr2

{-# INLINE safePlusWord #-}
safePlusWord :: Word -> Word -> Natural
safePlusWord !w1 !w2 =
    let (# !c, !s #) = plusWord2 w1 w2
    in if c == 0
        then Small s
        else mkPair s c

plusArrayW :: Int -> ByteArray -> Word -> Natural
plusArrayW !n !arr !w = runStrictPrim $ do
    !marr <- newWordArray (n + 1)
    writeWordArray marr n 0
    !x <- indexWordArrayM arr 0
    let (# !cry, !sm #) = plusWord2 x w
    writeWordArray marr 0 sm
    !nlen <- loop1 marr 1 cry
    !narr <- unsafeFreezeWordArray marr
    finalizeLarge nlen narr
  where
    loop1 !marr !i !carry
        | carry == 0 = loop2 marr i
        | i < n =  do
            !x <- indexWordArrayM arr i
            let (# !cry, !sm #) = plusWord2 x carry
            writeWordArray marr i sm
            loop1 marr (i + 1) cry
        | otherwise = do
            writeWordArray marr i carry
            return $ n + 1
    loop2 !marr !i
        | i < n =  do
            !x <- indexWordArrayM arr i
            writeWordArray marr i x
            loop2 marr (i + 1)
        | otherwise = return n


plusArray :: Int -> ByteArray -> Int -> ByteArray -> Natural
plusArray !n1 !arr1 !n2 !arr2
    | n1 < n2 = plusArray n2 arr2 n1 arr1
    | otherwise = runStrictPrim $ do
        marr <- newWordArray (n1 + 1)
        nlen <- loop1 marr 0 0
        narr <- unsafeFreezeWordArray marr
        finalizeLarge nlen narr
  where
    loop1 !marr !i !carry
        | i < n2 = do
            x <- indexWordArrayM arr1 i
            y <- indexWordArrayM arr2 i
            let (# !cry, !sm #) = plusWord2C x y carry
            writeWordArray marr i sm
            loop1 marr (i + 1) cry
        | otherwise = loop2 marr i carry
    loop2 !marr !i !carry
        | carry == 0 = loop3 marr i
        | i < n1 = do
            x <- indexWordArrayM arr1 i
            let (# !cry, !sm #) = plusWord2 x carry
            writeWordArray marr i sm
            loop2 marr (i + 1) cry
        | otherwise = do
            writeWordArray marr i carry
            return (i + 1)
    loop3 !marr !i
        | i < n1 = do
            x <- indexWordArrayM arr1 i
            writeWordArray marr i x
            loop3 marr (i + 1)
        | otherwise = return i



{-# NOINLINE minusInteger #-}
minusInteger :: Integer -> Integer -> Integer
minusInteger !(Positive a) !(Negative b) = Positive (plusNatural a b)
minusInteger !(Negative a) !(Positive b) = Negative (plusNatural a b)
minusInteger !(Positive a) !(Positive b) =
                if gtNatural a b
                    then Positive (minusNatural a b)
                    else Negative (minusNatural b a)

minusInteger !(Negative a) !(Negative b) =
                if gtNatural a b
                    then Negative (minusNatural a b)
                    else Positive (minusNatural b a)

minusNatural :: Natural -> Natural -> Natural
minusNatural !(Small a) !(Small b) = Small (a - b)
minusNatural !(Large n arr) !(Small w) = minusArrayW n arr w
minusNatural !(Small w) !(Large n arr) = plusArrayW n arr w
minusNatural !(Large n1 arr1) !(Large n2 arr2) = minusArray n1 arr1 n2 arr2


minusArrayW :: Int -> ByteArray -> Word -> Natural
minusArrayW !n !arr !w = runStrictPrim $ do
    !marr <- newWordArray (n + 1)
    writeWordArray marr n 0
    !x <- indexWordArrayM arr 0
    let (# !c, !d #) = minusWord2 x w
    writeWordArray marr 0 d
    !nlen <- loop1 marr 1 c
    !narr <- unsafeFreezeWordArray marr
    finalizeLarge nlen narr
  where
    loop1 !marr !i !carry
        | carry == 0 = loop2 marr i
        | i < n =  do
            !x <- indexWordArrayM arr i
            let (# !c, !d #) = minusWord2 x carry
            writeWordArray marr i d
            loop1 marr (i + 1) c
        | otherwise = do
            writeWordArray marr i carry
            return $ n + 1
    loop2 !marr !i
        | i < n =  do
            !x <- indexWordArrayM arr i
            writeWordArray marr i x
            loop2 marr (i + 1)
        | otherwise = return n


minusArray :: Int -> ByteArray -> Int -> ByteArray -> Natural
minusArray !n1 !arr1 !n2 !arr2
    | n1 < n2 = plusArray n2 arr2 n1 arr1
    | otherwise = runStrictPrim $ do --
        !marr <- newWordArray (n1 + 1)
        loop1 marr 0 0
        !narr <- unsafeFreezeWordArray marr
        finalizeLarge (n1 + 1) narr
  where
    loop1 !marr !i !carry
        | i < n2 = do
            !x <- indexWordArrayM arr1 i
            !y <- indexWordArrayM arr2 i
            let (# !c, !d #) = minusWord2C x y carry
            writeWordArray marr i d
            loop1 marr (i + 1) c
        | otherwise = loop2 marr i carry
    loop2 !marr !i !carry
        | carry == 0 = loop3 marr i
        | i < n1 = do
            !x <- indexWordArrayM arr1 i
            let (# !c, !d #) = minusWord2 x carry
            writeWordArray marr i d
            loop2 marr (i + 1) c
        | otherwise = do
            writeWordArray marr i carry
            loop4 marr (i + 1)
    loop3 !marr !i
        | i < n1 = do
            !x <- indexWordArrayM arr1 i
            writeWordArray marr i x
            loop3 marr (i + 1)
        | otherwise = loop4 marr i
    loop4 !marr !i
        | i <= n1 = do
            writeWordArray marr i 0
            loop4 marr (i + 1)
        | otherwise = return ()

{-# NOINLINE timesInteger #-}
timesInteger :: Integer -> Integer -> Integer
timesInteger !x !y = case (# x, y #) of
    (# Positive a, Positive b #) -> Positive (timesNatural a b)
    (# Positive a, Negative b #) -> Negative (timesNatural a b)
    (# Negative a, Positive b #) -> Negative (timesNatural a b)
    (# Negative a, Negative b #) -> Positive (timesNatural a b)

timesNatural :: Natural -> Natural -> Natural
timesNatural !x !y = case (# x, y #) of
    (# _, Small 0 #) -> Small 0
    (# Small 0, _ #) -> Small 0
    (# !a, Small 1 #) -> a
    (# Small 1, !b #) -> b
    (# Small !w1, Small !w2 #) -> safeTimesWord w1 w2
    (# Small !w1, Large !n2 !arr2 #) -> timesArrayW n2 arr2 w1
    (# Large !n1 !arr1, Small !w2 #) -> timesArrayW n1 arr1 w2
    (# Large !n1 !arr1, Large !n2 !arr2 #) -> timesArray n1 arr1 n2 arr2

{-# INLINE safeTimesWord #-}
safeTimesWord :: Word -> Word -> Natural
safeTimesWord !w1 !w2 =
    let (# !ovf, !prod #) = timesWord2 w1 w2
    in if ovf == 0
        then Small prod
        else mkPair prod ovf

timesArrayW :: Int -> ByteArray -> Word -> Natural
timesArrayW !n !arr !w = runStrictPrim $ do
    !marr <- newWordArrayCleared (n + 1)
    writeWordArray marr (n - 1) 0
    loop marr 0 0
    !narr <- unsafeFreezeWordArray marr
    finalizeLarge (n + 1) narr
  where
    loop !marr !i !carry
        | i < n = do
            !x <- indexWordArrayM arr i
            let (# !c, !p #) = timesWord2C x w carry
            writeWordArray marr i p
            loop marr (i + 1) c
        | otherwise =
            writeWordArray marr i carry


timesArray :: Int -> ByteArray -> Int -> ByteArray -> Natural
timesArray !n1 !arr1 !n2 !arr2
    | n1 < n2 = timesArray n2 arr2 n1 arr1
    | otherwise = runStrictPrim $ do
        !psum <- newPlaceholderWordArray
        outerLoop 0 psum 0
  where
    outerLoop !psumLen !psum !s2
        | s2 < n2 = do
            !w <- indexWordArrayM arr2 s2
            if w == 0
                then outerLoop psumLen psum (s2 + 1)
                else do
                    let !newPsumLen = (max psumLen (n1 + s2 + 1)) + 1
                    !marr <- cloneWordArrayExtend psumLen psum newPsumLen
                    !possLen <- innerLoop marr psumLen psum 0 s2 w 0
                    !narr <- unsafeFreezeWordArray marr
                    outerLoop possLen narr (s2 + 1)
        | otherwise =
            finalizeLarge psumLen psum

    innerLoop !marr !pn !psum !s1 !s2 !hw !carry
        | s1 + s2 < pn && s1 < n1 = do
            !ps <- indexWordArrayM psum (s1 + s2)
            !x <- indexWordArrayM arr1 s1
            let (# !hc, !hp #) = timesWord2CC x hw carry ps
            writeWordArray marr (s1 + s2) hp
            innerLoop marr pn psum (s1 + 1) s2 hw hc
        | s1 < n1 = do
            !x <- indexWordArrayM arr1 s1
            let (# !hc, !hp #) = timesWord2C x hw carry
            writeWordArray marr (s1 + s2) hp
            innerLoop marr pn psum (s1 + 1) s2 hw hc
        | carry /= 0 = do
            writeWordArray marr (s1 + s2) carry
            return (s1 + s2 + 1)
        | otherwise = return (s1 + s2 + 1)

{-# NOINLINE divModInteger #-}
divModInteger :: Integer -> Integer -> (# Integer, Integer #)
divModInteger _ _ = error ("New2/GHC/Integer/Type.hs: line " ++ show (__LINE__ :: Int))

{-# NOINLINE quotRemInteger #-}
quotRemInteger :: Integer -> Integer -> (# Integer, Integer #)
quotRemInteger _ _ = error ("New2/GHC/Integer/Type.hs: line " ++ show (__LINE__ :: Int))

{-# NOINLINE quotInteger #-}
quotInteger :: Integer -> Integer -> Integer
quotInteger a b =
    let (# q, _ #) = quotRemInteger a b
    in q

{-# NOINLINE remInteger #-}
remInteger :: Integer -> Integer -> Integer
remInteger = error ("New2/GHC/Integer/Type.hs: line " ++ show (__LINE__ :: Int))

{-# NOINLINE compareInteger #-}
compareInteger :: Integer -> Integer -> Ordering
compareInteger = error ("New2/GHC/Integer/Type.hs: line " ++ show (__LINE__ :: Int))

{-# NOINLINE eqInteger #-}
eqInteger :: Integer -> Integer -> Bool
eqInteger (Positive _) (Negative _) = False
eqInteger (Negative _) (Positive _) = False
eqInteger (Positive a) (Positive b) = eqNatural a b
eqInteger (Negative a) (Negative b) = eqNatural a b

eqNatural :: Natural -> Natural -> Bool
eqNatural (Small a) (Small b) = a == b
eqNatural (Small _) (Large _ _) = False
eqNatural (Large _ _) (Small _) = False
eqNatural (Large n1 arr1) (Large n2 arr2)
    | n1 /= n2 = False
    | otherwise =
        let eqArray idx
                | idx < 0 = True
                | indexWordArray arr1 idx /= indexWordArray arr2 idx = False
                | otherwise = eqArray (idx - 1)
        in eqArray (n1 - 1)

{-# NOINLINE neqInteger #-}
neqInteger :: Integer -> Integer -> Bool
neqInteger a b = not (eqInteger a b)

instance  Eq Integer  where
    (==) = eqInteger
    (/=) = neqInteger

{-# NOINLINE ltInteger #-}
ltInteger :: Integer -> Integer -> Bool
ltInteger (Positive _) (Negative _) = False
ltInteger (Negative _) (Positive _) = True

ltInteger (Positive a) (Positive b) = ltNatural a b
ltInteger (Negative a) (Negative b) = ltNatural b a

ltNatural :: Natural -> Natural -> Bool
ltNatural (Small a) (Small b) = a < b
ltNatural (Small _) (Large _ _) = True
ltNatural (Large _ _)(Small _) = False
ltNatural (Large n1 arr1) (Large n2 arr2)
    | n1 < n2 = True
    | n1 > n2 = False
    | otherwise =
        let check 0 = indexWordArray arr1 0 < indexWordArray arr2 0
            check i =
                if indexWordArray arr1 i == indexWordArray arr2 i
                    then check (i - 1)
                    else indexWordArray arr1 i < indexWordArray arr2 i
        in check (n1 - 1)


{-# NOINLINE gtInteger #-}
gtInteger :: Integer -> Integer -> Bool
gtInteger (Positive _) (Negative _) = True
gtInteger (Negative _) (Positive _) = False
gtInteger (Positive a) (Positive b) = gtNatural a b
gtInteger (Negative a) (Negative b) = gtNatural b a

gtNatural :: Natural -> Natural -> Bool
gtNatural (Large _ _) (Small _) = True
gtNatural  (Small _)(Large _ _) = False
gtNatural (Small a) (Small b) = a > b
gtNatural (Large !n1 !arr1) (Large !n2 !arr2)
    | n1 > n2 = True
    | n1 < n2 = False
    | otherwise =
            let check 0 = indexWordArray arr1 0 > indexWordArray arr2 0
                check i =
                    if indexWordArray arr1 i == indexWordArray arr2 i
                        then check (i - 1)
                        else indexWordArray arr1 i > indexWordArray arr2 i
            in check (n1 - 1)

{-# NOINLINE leInteger #-}
leInteger :: Integer -> Integer -> Bool
leInteger a b = not (gtInteger a b)

{-# NOINLINE geInteger #-}
geInteger :: Integer -> Integer -> Bool
geInteger a b = not (ltInteger a b)

instance Ord Integer where
    (<=) = leInteger
    (>)  = gtInteger
    (<)  = ltInteger
    (>=) = geInteger
    compare = compareInteger

{-# NOINLINE absInteger #-}
absInteger :: Integer -> Integer
absInteger a@(Positive _) = a
absInteger (Negative a) = Positive a

{-# NOINLINE signumInteger #-}
signumInteger :: Integer -> Integer
signumInteger = error ("New2/GHC/Integer/Type.hs: line " ++ show (__LINE__ :: Int))

{-# NOINLINE hashInteger #-}
hashInteger :: Integer -> Int#
hashInteger = integerToInt

--------------------------------------------------------------------------------
-- Helpers (not part of the API).

mkLarge :: Natural -> Natural
mkLarge (Small w) = mkSingletonArray w
mkLarge a = a

mkPair :: Word -> Word -> Natural
mkPair !lo !hi = runStrictPrim mkLargePair
  where
    mkLargePair :: StrictPrim s Natural
    mkLargePair = do
        !marr <- newWordArray 2
        writeWordArray marr 0 lo
        writeWordArray marr 1 hi
        !narr <- unsafeFreezeWordArray marr
        return $ Large 2 narr

mkSingletonArray :: Word -> Natural
mkSingletonArray !x = runStrictPrim mkSingleton
  where
    mkSingleton :: StrictPrim s Natural
    mkSingleton = do
        !marr <- newWordArray 1
        writeWordArray marr 0 x
        !narr <- unsafeFreezeWordArray marr
        return $ Large 1 narr

shiftLArray :: Int -> ByteArray -> Int -> Natural
shiftLArray !n !arr !i
    | i < WORD_SIZE_IN_BITS =
            smallShiftLArray n arr (# i, WORD_SIZE_IN_BITS - i #)
    | otherwise = do
            let (!q, !r) = quotRem i WORD_SIZE_IN_BITS
            if r == 0
                then wordShiftLArray n arr q
                else largeShiftLArray n arr (# q, r, WORD_SIZE_IN_BITS - r #)

smallShiftLArray :: Int -> ByteArray -> (# Int, Int #) -> Natural
smallShiftLArray !n !arr (# !si, !sj #) = runStrictPrim $ do
    !marr <- newWordArray (n + 1)
    !nlen <- loop marr 0 0
    !narr <- unsafeFreezeWordArray marr
    finalizeLarge nlen narr
  where
    loop !marr !i !mem
        | i < n =  do
            !x <- indexWordArrayM arr i
            writeWordArray marr i ((unsafeShiftL x si) .|. mem)
            loop marr (i + 1) (shiftRWord x sj)
        | mem /= 0 = do
            writeWordArray marr i mem
            return $ i + 1
        | otherwise = return n

-- | TODO : Use copy here
wordShiftLArray :: Int -> ByteArray -> Int -> Natural
wordShiftLArray !n !arr !q = runStrictPrim $ do
    !marr <- newWordArray (n + q)
    loop1 marr 0
    !narr <- unsafeFreezeWordArray marr
    finalizeLarge (n + q) narr
  where
    loop1 !marr !i
        | i < q = do
            writeWordArray marr i 0
            loop1 marr (i + 1)
        | otherwise = loop2 marr 0
    loop2 !marr !i
        | i < n =  do
            !x <- indexWordArrayM arr i
            writeWordArray marr (q + i) x
            loop2 marr (i + 1)
        | otherwise = return ()


largeShiftLArray :: Int -> ByteArray-> (# Int, Int, Int #) -> Natural
largeShiftLArray !n !arr (# !q, !si, !sj #) = runStrictPrim $ do
    !marr <- newWordArray (n + q + 1)
    setWordArray marr 0 q 0
    loop2 marr 0 0
    !narr <- unsafeFreezeWordArray marr
    finalizeLarge (n + q + 1) narr
  where
    loop2 !marr !i !mem
        | i < n =  do
            !x <- indexWordArrayM arr i
            writeWordArray marr (q + i) ((unsafeShiftL x si) .|. mem)
            loop2 marr (i + 1) (shiftRWord x sj)
        | mem /= 0 = do
            writeWordArray marr (q + i) mem
        | otherwise =
            writeWordArray marr (q + i) 0


shiftRArray :: Int -> ByteArray -> Int -> Natural
shiftRArray !n !arr !i
    | i < WORD_SIZE_IN_BITS =
            smallShiftRArray n arr (# i, WORD_SIZE_IN_BITS - i #)
    | otherwise = do
            let (!q, !r) = quotRem i WORD_SIZE_IN_BITS
            if q >= n
                then Small 0
                else if r == 0
                    then wordShiftRArray n arr q
                    else largeShiftRArray n arr (# q, r, WORD_SIZE_IN_BITS - r #)


smallShiftRArray :: Int -> ByteArray -> (# Int, Int #) -> Natural
smallShiftRArray !n !arr (# !si, !sj #) = runStrictPrim $ do
    !marr <- newWordArray n
    loop marr (n - 1) 0
    !narr <- unsafeFreezeWordArray marr
    finalizeLarge n narr
  where
    loop !marr !i !mem
        | i >= 0 =  do
            !x <- indexWordArrayM arr i
            writeWordArray marr i ((shiftRWord x si) .|. mem)
            loop marr (i - 1) (unsafeShiftL x sj)
        | otherwise = return ()

wordShiftRArray :: Int -> ByteArray -> Int -> Natural
wordShiftRArray !n !arr !q = runStrictPrim $ do
    !marr <- newWordArray (n - q)
    copyWordArray marr 0 arr q (n - q)
    !narr <- unsafeFreezeWordArray marr
    finalizeLarge (n - q) narr


largeShiftRArray :: Int -> ByteArray-> (# Int, Int, Int #) -> Natural
largeShiftRArray !n !arr (# !q, !si, !sj #) = runStrictPrim $ do
    !marr <- newWordArray (n - q)
    loop marr (n - q - 1) 0
    !narr <- unsafeFreezeWordArray marr
    finalizeLarge (n - q) narr
  where
    loop !marr !i !mem
        | i >= 0 =  do
            !x <- indexWordArrayM arr (q + i)
            writeWordArray marr i ((shiftRWord x si) .|. mem)
            loop marr (i - 1) (unsafeShiftL x sj)
        | otherwise = return ()


finalizeLarge :: Int -> ByteArray -> StrictPrim s Natural
finalizeLarge !nin !arr = do
    let !len = nonZeroLen nin arr
    !x <- indexWordArrayM arr 0
    return $
        if len == 0 || (len == 1 && x == 0)
            then Small 0
            else Large len arr

nonZeroLen :: Int -> ByteArray -> Int
nonZeroLen !len !arr
    | len <= 1 = 1
    | otherwise =
        let trim n
                | n <= 0 = 0
                | indexWordArray arr n == 0 = trim (n - 1)
                | otherwise = n
        in trim (len - 1) + 1


oneInteger, minusOneInteger :: Integer
oneInteger = Positive (Small 1)
minusOneInteger = Negative (Small 1)

{-

twoToTheThirtytwoInteger :: Integer
twoToTheThirtytwoInteger = error ("New2/GHC/Integer/Type.hs: line " ++ show (__LINE__ :: Int))
-}


toList :: Integer -> [Word]
toList ii =
    case ii of
        Positive nat -> natList nat
        Negative nat -> natList nat
  where
    natList (Small w) = [w]
    natList (Large n arr) = unpackArray 0
        where
            unpackArray i
                | i < n = do
                    let xs = unpackArray (i + 1)
                        x = indexWordArray arr i
                    x : xs
                | otherwise = []

arrayShow :: Int -> ByteArray -> String
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

isMinimal :: Integer -> Bool
isMinimal i =
    case i of
        Positive a -> isMinimalNatural a
        Negative a -> isMinimalNatural a
  where
    isMinimalNatural (Small _) = True
    isMinimalNatural (Large n arr) = indexWordArray arr (n - 1) /= 0


#endif
