{-# LANGUAGE CPP, MagicHash, ForeignFunctionInterface, NoImplicitPrelude,
             BangPatterns, UnboxedTuples, UnliftedFFITypes #-}


#include "MachDeps.h"

module New.GHC.Integer.Type
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

import Prelude hiding (Integer, abs) -- (all, error, otherwise, return, show, succ, (++))

import Control.Monad.Primitive
import Data.Bits
import Data.Primitive.ByteArray

import GHC.Prim
import GHC.Types
import GHC.Tuple ()
#if WORD_SIZE_IN_BITS < 64
import GHC.IntWord64
#endif

import New.GHC.Integer.Array
import New.GHC.Integer.Prim
import New.GHC.Integer.Sign

#if !defined(__HADDOCK__)

data Integer
    = Small
        {-# UNPACK #-} !Int
    | Large !Sign
        {-# UNPACK #-} !Int
        {-# UNPACK #-} !ByteArray

-------------------------------------------------------------------
-- The hard work is done on positive numbers

mkInteger :: Bool   -- non-negative?
          -> [Int]  -- absolute value in 31 bit chunks, least significant first
                    -- ideally these would be Words rather than Ints, but
                    -- we don't have Word available at the moment.
          -> Integer
mkInteger _ [] = smallInteger 0#
mkInteger True [I# i] = smallInteger i
mkInteger False [I# i]
    | i ==# 0# = smallInteger 0#
    | i ># 0# = unsafeInlinePrim (mkSingletonArray Neg (W# (int2Word# i)))
    | otherwise = unsafeInlinePrim (mkSingletonArray Pos (W# (int2Word# (negateInt# i))))
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
wordToInteger w
    | (W# w) < maxPositiveInt = Small (I# (word2Int# w))
    | otherwise = unsafeInlinePrim (mkSingletonArray Pos (W# w))

{-# NOINLINE integerToWord #-}
integerToWord :: Integer -> Word#
integerToWord (Small (I# i)) = int2Word# i
integerToWord (Large _ _ arr) = unboxWord (indexWordArray arr 0)

{-# NOINLINE integerToInt #-}
integerToInt :: Integer -> Int#
integerToInt (Small (I# i)) = i
integerToInt (Large !s _ arr) =
    let i = word2Int# (unboxWord (indexWordArray arr 0))
    in case s of
        Pos -> i
        Neg -> negateInt# i

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
andInteger _ (Small 0) = smallInteger 0#
andInteger (Small 0) _ = smallInteger 0#
andInteger (Small a) (Small b) = Small (a .&. b)
andInteger (Small a) b = andInteger (mkLarge a) b
andInteger a (Small b) = andInteger a (mkLarge b)
andInteger (Large _ n1 arr1) (Large _ n2 arr2) =
    unsafeInlinePrim $ andArray Pos (min n1 n2) arr1 arr2


andArray :: Sign -> Int -> ByteArray -> ByteArray -> IO Integer
andArray s n arr1 arr2 = do
    !marr <- newWordArray n
    loop marr 0
    narr <- unsafeFreezeWordArray marr
    return $ Large s (trimLeadingZeroes n narr) narr
  where
    loop !marr !i
        | i < n = do
                x <- indexWordArrayM arr1 i
                y <- indexWordArrayM arr2 i
                writeWordArray marr i (x .&. y)
                loop marr (i + 1)
        | otherwise = return ()


{-# NOINLINE orInteger #-}
orInteger :: Integer -> Integer -> Integer
orInteger (Small 0) b = b
orInteger a (Small 0) = a
orInteger (Small a) (Small b)
    | a < 0 && b < 0 = Small (1 + ((a - 1) .&. (b - 1)))
    | a < 0 = Small (1 + ((a - 1) .&. (complement b)))
    | b < 0 = Small (1 + ((complement a) .&. (b - 1)))
    | otherwise = Small (a .|. b)
{-
orInteger (Large Neg _ _) _ = smallInteger (-1#)
orInteger _ (Large Neg _ _) = smallInteger (-1#)
-}
orInteger (Small a) b = orInteger (mkLarge a) b
orInteger a (Small b) = orInteger a (mkLarge b)
orInteger (Large s1 n1 arr1) (Large s2 n2 arr2) =
    let s = case (s1, s2) of
                (Neg, _) -> Neg
                (_, Neg) -> Neg
                (_, _) -> Pos
    in unsafeInlinePrim $ if n1 >= n2
                            then orArray s n1 arr1 n2 arr2
                            else orArray s n2 arr2 n1 arr1

{-
Positive x `orInteger` Negative y = let x' = flipBits x
                                        y' = y `minusPositive` onePositive
                                        z = x' `andDigitsOnes` y'
                                        z' = succPositive z
                                    in digitsToNegativeInteger z'

Negative x `orInteger` Positive y = Positive y `orInteger` Negative x

Negative x `orInteger` Negative y = let x' = x `minusPositive` onePositive
                                        y' = y `minusPositive` onePositive
                                        z = x' `andDigits` y'
                                        z' = succPositive z
                                    in digitsToNegativeInteger z'
-}

orArray :: Sign -> Int -> ByteArray -> Int -> ByteArray -> IO Integer
orArray !s !n1 !arr1 !n2 !arr2 = do
    !marr <- newWordArray n1
    nlen <- loop1 marr 0
    narr <- unsafeFreezeWordArray marr
    return $ Large s (trimLeadingZeroes nlen narr) narr
  where
    loop1 !marr !i
        | i < n2 = do
                x <- indexWordArrayM arr1 i
                y <- indexWordArrayM arr2 i
                writeWordArray marr i (x .|. y)
                loop1 marr (i + 1)
        | otherwise = loop2 marr i
    loop2 !marr !i
        | i < n1 = do
                -- TODO : Use copyArray here?
                x <- indexWordArrayM arr1 i
                writeWordArray marr i x
                loop2 marr (i + 1)
        | otherwise = return i

{-# NOINLINE xorInteger #-}
xorInteger :: Integer -> Integer -> Integer
xorInteger a (Small 0) = a
xorInteger (Small 0) b = b
xorInteger (Small a) (Small b) = Small (xor a b)
xorInteger (Small a) b = xorInteger (mkLarge a) b
xorInteger a (Small b) = xorInteger a (mkLarge b)
xorInteger (Large _ n1 arr1) (Large _ n2 arr2) =
    unsafeInlinePrim $ if n1 >= n2
                            then xorArray Pos n1 arr1 n2 arr2
                            else xorArray Pos n2 arr2 n1 arr1

xorArray :: Sign -> Int -> ByteArray -> Int -> ByteArray -> IO Integer
xorArray !s !n1 !arr1 !n2 !arr2 = do
    !marr <- newWordArray n1
    loop1 marr 0
    narr <- unsafeFreezeWordArray marr
    return $ Large s (trimLeadingZeroes n1 narr) narr
  where
    loop1 !marr !i
        | i < n2 = do
                x <- indexWordArrayM arr1 i
                y <- indexWordArrayM arr2 i
                writeWordArray marr i (xor x y)
                loop1 marr (i + 1)
        | otherwise = loop2 marr i
    loop2 !marr !i
        | i < n1 = do
                -- TODO : Use copyArray here?
                x <- indexWordArrayM arr1 i
                writeWordArray marr i x
                loop2 marr (i + 1)
        | otherwise = return ()

{-# NOINLINE complementInteger #-}
complementInteger :: Integer -> Integer
complementInteger = error ("New/GHC/Integer/Type.hs: line " ++ show (__LINE__ :: Int))

{-# NOINLINE shiftLInteger #-}
shiftLInteger :: Integer -> Int# -> Integer
shiftLInteger a 0# = a
shiftLInteger (Small 0) _ = (Small 0)
shiftLInteger (Small a) b = shiftLInteger (mkLarge a) b
shiftLInteger (Large !s !n !arr) b = unsafeInlinePrim (shiftLArray s n arr (I# b))

{-# NOINLINE shiftRInteger #-}
shiftRInteger :: Integer -> Int# -> Integer
shiftRInteger a 0# = a
shiftRInteger _ _ = error ("New/GHC/Integer/Type.hs: line " ++ show (__LINE__ :: Int))

{-# NOINLINE negateInteger #-}
negateInteger :: Integer -> Integer
negateInteger (Small a) = Small (-a)
negateInteger (Large !s !n !arr) = Large (negateSign s) n arr

-- Note [Avoid patError]
{-# NOINLINE plusInteger #-}
plusInteger :: Integer -> Integer -> Integer
plusInteger (Small a) (Small b) = Small (a + b)
plusInteger _ _ = error ("New/GHC/Integer/Type.hs: line " ++ show (__LINE__ :: Int))

{-# NOINLINE minusInteger #-}
minusInteger :: Integer -> Integer -> Integer
minusInteger (Small a) (Small b) = Small (a - b)
minusInteger _ _ = error ("New/GHC/Integer/Type.hs: line " ++ show (__LINE__ :: Int))

{-# NOINLINE timesInteger #-}
timesInteger :: Integer -> Integer -> Integer
timesInteger (Small !a) (Small !b)
    | a <= maxHalfInt && b < maxHalfInt = Small (a * b)
    | otherwise = timesInteger (mkLarge a) (mkLarge b)
timesInteger (Small !a) b@(Large{}) = timesInteger b (mkLarge a)
timesInteger a@(Large{}) (Small !b) = timesInteger a (mkLarge b)
timesInteger (Large !s1 !n1 !arr1) (Large !s2 !n2 !arr2) =
    unsafeInlinePrim (timesArray (timesSign s1 s2) n1 arr1 n2 arr2)



{-# NOINLINE divModInteger #-}
divModInteger :: Integer -> Integer -> (# Integer, Integer #)
divModInteger (Small a) (Small b) =
    let (q, r) = a `divMod` b
    in (# Small q, Small r #)
divModInteger _ _ = error ("New/GHC/Integer/Type.hs: line " ++ show (__LINE__ :: Int))

{-# NOINLINE quotRemInteger #-}
quotRemInteger :: Integer -> Integer -> (# Integer, Integer #)
quotRemInteger (Small a) (Small b) =
    let (q, r) = a `quotRem` b
    in (# Small q, Small r #)
quotRemInteger _ _ = error ("New/GHC/Integer/Type.hs: line " ++ show (__LINE__ :: Int))

{-# NOINLINE quotInteger #-}
quotInteger :: Integer -> Integer -> Integer
quotInteger (Small a) (Small b) = Small (a `quot` b)
quotInteger a b =
    let (# q, _ #) = quotRemInteger a b
    in q

{-# NOINLINE remInteger #-}
remInteger :: Integer -> Integer -> Integer
remInteger = error ("New/GHC/Integer/Type.hs: line " ++ show (__LINE__ :: Int))

{-# NOINLINE compareInteger #-}
compareInteger :: Integer -> Integer -> Ordering
compareInteger = error ("New/GHC/Integer/Type.hs: line " ++ show (__LINE__ :: Int))

{-# NOINLINE eqInteger #-}
eqInteger :: Integer -> Integer -> Bool
eqInteger (Small a) (Small b) = a == b
eqInteger (Small a) b = eqInteger (mkLarge a) b
eqInteger a (Small b) = eqInteger a (mkLarge b)
eqInteger (Large s1 n1 arr1) (Large s2 n2 arr2)
    | s1 /= s2 = False
    | otherwise = eqArray 0 0
  where
    eqArray i1 i2
        | i1 >= n1 && i2 >= n2 = True
        | i1 < n1 && i2 >= n2 = False
        | i1 >= n1 && i2 < n2 = False
        | indexWordArray arr1 i1 /= indexWordArray arr2 i1 = False
        | otherwise = eqArray (i1 + 1) (i2 + 1)

{-# NOINLINE neqInteger #-}
neqInteger :: Integer -> Integer -> Bool
neqInteger (Small a) (Small b) = a /= b
neqInteger a b = not (eqInteger a b)

instance  Eq Integer  where
    (==) = eqInteger
    (/=) = neqInteger

{-# NOINLINE ltInteger #-}
ltInteger :: Integer -> Integer -> Bool
ltInteger (Small a) (Small b) = a < b
ltInteger (Small a) b = geInteger b (mkLarge a)
ltInteger a (Small b) = ltInteger a (mkLarge b)
ltInteger (Large s1 n1 arr1) (Large s2 n2 arr2)
    | s1 /= s2 = s1 < s2
    | s1 == Pos = ltArray n1 arr1 n2 arr2
    | otherwise = geArray n1 arr1 n2 arr2

ltArray :: Int -> ByteArray -> Int -> ByteArray -> Bool
ltArray !n1 !arr1 !n2 !arr2
    | n1 == n2 = indexWordArray arr1 (n1 - 1) < indexWordArray arr2 (n2 - 1)
    | n1 > n2 && indexWordArray arr1 (n1 - 1) > 0 = True
    | n1 < n2 && indexWordArray arr2 (n2 - 1) > 0 = True
    | otherwise = False

geArray :: Int -> ByteArray -> Int -> ByteArray -> Bool
-- geArray n1 arr1 n2 arr2
geArray = error ("New/GHC/Integer/Type.hs: line " ++ show (__LINE__ :: Int))


{-# NOINLINE gtInteger #-}
gtInteger :: Integer -> Integer -> Bool
gtInteger (Small a) (Small b) = a > b
gtInteger (Small a) b = leInteger b (mkLarge a)
gtInteger a (Small b) = gtInteger a (mkLarge b)
gtInteger (Large s1 n1 arr1) (Large s2 n2 arr2)
    | s1 /= s2 = s1 > s2
    | s1 == Pos = gtArray n1 arr1 n2 arr2
    | otherwise = leArray n1 arr1 n2 arr2


gtArray :: Int -> ByteArray -> Int -> ByteArray -> Bool
gtArray !n1 !arr1 !n2 !arr2
    | n1 == n2 = indexWordArray arr1 (n1 - 1) > indexWordArray arr2 (n2 - 1)
    | n1 > n2 && indexWordArray arr1 (n1 - 1) < 0 = True
    | n1 < n2 && indexWordArray arr2 (n2 - 1) < 0 = True
    | otherwise = False

leArray :: Int -> ByteArray -> Int -> ByteArray -> Bool
-- geArray n1 arr1 n2 arr2
leArray = error ("New/GHC/Integer/Type.hs: line " ++ show (__LINE__ :: Int))

{-# NOINLINE leInteger #-}
leInteger :: Integer -> Integer -> Bool
leInteger = error ("New/GHC/Integer/Type.hs: line " ++ show (__LINE__ :: Int))

{-# NOINLINE geInteger #-}
geInteger :: Integer -> Integer -> Bool
geInteger (Small a) (Small b) = a >= b
geInteger (Small a) b = ltInteger b (mkLarge a)
geInteger a (Small b) = geInteger a (mkLarge b)
geInteger (Large s1 n1 arr1) (Large s2 n2 arr2)
    | s1 /= s2 = s1 >= s2
    | s1 == Pos = geArray n1 arr1 n2 arr2
    | otherwise = ltArray n1 arr1 n2 arr2


instance Ord Integer where
    (<=) = leInteger
    (>)  = gtInteger
    (<)  = ltInteger
    (>=) = geInteger
    compare = compareInteger

{-# NOINLINE absInteger #-}
absInteger :: Integer -> Integer
absInteger (Small a) = Small (absInt a)
absInteger (Large Neg n arr) = Large Pos n arr
absInteger a = a

{-# NOINLINE signumInteger #-}
signumInteger :: Integer -> Integer
signumInteger = error ("New/GHC/Integer/Type.hs: line " ++ show (__LINE__ :: Int))

{-# NOINLINE hashInteger #-}
hashInteger :: Integer -> Int#
hashInteger = integerToInt

--------------------------------------------------------------------------------
-- Helpers (not part of the API).

maxHalfInt :: Int
maxPositiveInt :: Word
#if WORD_SIZE_IN_BITS == 64
maxHalfInt = 0xffffffff
maxPositiveInt = 0x7fffffffffffffff
#elif WORD_SIZE_IN_BITS == 32
maxHalfInt = 0xffff
maxPositiveInt = 0x7fffffff
#endif


unboxWord :: Word -> Word#
unboxWord !(W# w) = w

mkLarge :: Int -> Integer
mkLarge !(I# !i) =
    let !sign = if i <# 0# then Neg else Pos
    in unsafeInlinePrim (mkSingletonArray sign (W# (int2Word# i)))

mkSingletonArray :: Sign -> Word -> IO Integer
mkSingletonArray !s !x = do
    !marr <- newWordArray 1
    writeWordArray marr 0 x
    narr <- unsafeFreezeWordArray marr
    return $ Large s 1 narr

shiftLArray :: Sign -> Int -> ByteArray -> Int -> IO Integer
shiftLArray !s !n !arr !i
    | i < WORD_SIZE_IN_BITS =
                smallShiftLArray s n arr (# i, WORD_SIZE_IN_BITS - i #)
    | True = error ("New/GHC/Integer/Type.hs: line " ++ show (__LINE__ :: Int))

smallShiftLArray :: Sign -> Int -> ByteArray -> (# Int, Int #) -> IO Integer
smallShiftLArray !s !n !arr (# !si, !sj #) = do
    !marr <- newWordArray (succ n)
    nlen <- loop marr 0 0
    narr <- unsafeFreezeWordArray marr
    return $ Large s nlen narr
  where
    loop !marr !i !mem
        | i < n =  do
            x <- indexWordArrayM arr i
            writeWordArray marr i ((unsafeShiftL x si) .|. mem)
            loop marr (i + 1) (unsafeShiftR x sj)
        | mem == 0 = return n
        | otherwise = do
            writeWordArray marr i mem
            return $ i + 1


timesArray :: Sign -> Int -> ByteArray -> Int -> ByteArray -> IO Integer
timesArray s n1 arr1 n2 arr2 = do
    -- putStrLn $ "timesArray marr " ++ show (n1, n2)
    !marr1 <- newHalfWordArrayCleared (2 * (n1 + n2))
    !marr2 <- newHalfWordArrayCleared (2 * (n1 + n2))
    compute (marr1, marr2) 0 0 0 0
    narr <- unsafeFreezeHalfWordArray marr1
    _ <- unsafeFreezeHalfWordArray marr2
    return $ Large s (trimLeadingZeroes (n1 + n2) narr) narr
  where
    compute (!marr, !res) !d !s1 !s2 !carry
        | s1 < 2 * n1 = do
            -- putStrLn $ "arrFill marr " ++ show (d, s1, n1, s2, n2)
            x1 <- indexHalfWordArrayM arr1 s1
            x2 <- indexHalfWordArrayM arr2 s2
            let (nc, y) = timesHalfWordC x1 x2 carry
            writeHalfWordArray marr d y
            compute (marr, res) (d + 1) (s1 + 1) s2 nc
        | otherwise = return ()



{-
oneInteger :: Integer
oneInteger = Small 1

negativeOneInteger :: Integer
negativeOneInteger = Small (-1)

twoToTheThirtytwoInteger :: Integer
twoToTheThirtytwoInteger = error ("New/GHC/Integer/Type.hs: line " ++ show (__LINE__ :: Int))
-}

trimLeadingZeroes :: Int -> ByteArray -> Int
trimLeadingZeroes 1 _ = 1
trimLeadingZeroes !len !arr =
    trim (len - 1) + 1
  where
    trim n
        | n <= 1 = 1
        | indexWordArray arr n == 0 = trim (n - 1)
        | otherwise = n

toList :: Integer -> [Word]
toList (Small (I# i)) = [W# (int2Word# i)]
toList (Large _ n arr) =
    unpackArray 0
  where
    unpackArray i
        | i < n = do
                let xs = unpackArray (i + 1)
                    x = indexWordArray arr i
                x : xs
        | otherwise = []

absInt :: Int -> Int
absInt x = if x < 0 then -x else x

#endif
