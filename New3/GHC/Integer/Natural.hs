{-# LANGUAGE CPP, MagicHash, ForeignFunctionInterface, NoImplicitPrelude,
             BangPatterns, Strict, UnboxedTuples, UnliftedFFITypes #-}

#include "MachDeps.h"

module New3.GHC.Integer.Natural where

import Prelude hiding (Integer, abs, pi, sum, rem, succ) -- (all, error, otherwise, return, show, (++))

import Data.Bits
import Control.Monad.Primitive
import GHC.Base

import Common.GHC.Integer.Debug
import Common.GHC.Integer.Loop
import Common.GHC.Integer.Prim
import Common.GHC.Integer.StrictPrim
import New3.GHC.Integer.Type
import Common.GHC.Integer.WordArray

#if !defined(__HADDOCK__)

--------------------------------------------------------------------------------

mkNatural :: [Int] -> Natural
mkNatural = f
  where
    f [] = zeroNatural
    f [I# x] = mkSingletonNat (W# (int2Word# x))
    f (I# x : xs) = (shiftLNatural (f xs) 31) `orNaturalW` (W# (int2Word# x))

encodeDoubleNatural :: Natural -> Int# -> Double#
encodeDoubleNatural !(Natural !n !arr) e0 =
    let (!res, _) = runStrictPrim $ intLoopState 0 (n - 1) (0.0, I# e0) $ \ i (D# d, e) -> do
                        (W# w) <- indexWordArrayM arr i
                        return (D# (d +## encodeDouble# w (unboxInt e)), e + WORD_SIZE_IN_BITS)
    in unboxDouble res

andNatural :: Natural -> Natural -> Natural
andNatural (Natural n1 arr1) (Natural n2 arr2) = andArray (min n1 n2) arr1 arr2

andArray :: Int -> WordArray -> WordArray -> Natural
andArray n arr1 arr2 = runStrictPrim $ do
    marr <- newWordArray n
    loop1 marr 0
    narr <- unsafeFreezeWordArray marr
    nlen <- loop2 narr (n - 1)
    return $! Natural nlen narr
  where
    loop1 !marr !i
        | i < n = do
                x <- indexWordArrayM arr1 i
                y <- indexWordArrayM arr2 i
                writeWordArray marr i (x .&. y)
                loop1 marr (i + 1)
        | otherwise = return ()
    loop2 !narr !i
        | i < 0 = return 0
        | indexWordArray narr i == 0 = loop2 narr (i - 1)
        | otherwise = return (i + 1)

orNatural :: Natural -> Natural -> Natural
orNatural (Natural n1 arr1) (Natural n2 arr2) = orArray n1 arr1 n2 arr2

orNaturalW :: Natural -> Word -> Natural
orNaturalW !(Natural !n !arr) !w = runStrictPrim $ do
    marr <- newWordArray n
    copyWordArray marr 1 arr 1 (n - 1)
    x <- indexWordArrayM arr 0
    writeWordArray marr 0 (w .|. x)
    narr <- unsafeFreezeWordArray marr
    return $! Natural n narr

orArray :: Int -> WordArray -> Int -> WordArray -> Natural
orArray !n1 !arr1 !n2 !arr2
    | n1 < n2 = orArray n2 arr2 n1 arr1
    | otherwise = runStrictPrim $ do
        marr <- newWordArray n1
        loop1 marr 0
        narr <- unsafeFreezeWordArray marr
        return $! Natural n1 narr
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
        | otherwise = return ()


xorArray :: Int -> WordArray -> Int -> WordArray -> Natural
xorArray !n1 !arr1 !n2 !arr2
    | n1 < n2 = xorArray n2 arr2 n1 arr1
    | otherwise = runStrictPrim $ do
        marr <- newWordArray n1
        loop1 marr 0
        narr <- unsafeFreezeWordArray marr
        -- TODO : Test this and then optimize.
        finalizeNatural n1 narr
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


shiftLNatural :: Natural -> Int -> Natural
shiftLNatural !nat@(Natural !n !arr) !i
    | i <= 0 = nat
    | i < WORD_SIZE_IN_BITS =
            smallShiftLArray n arr (# i, WORD_SIZE_IN_BITS - i #)
    | otherwise = do
            let (!q, !r) = quotRem i WORD_SIZE_IN_BITS
            if r == 0
                then wordShiftLArray n arr q
                else largeShiftLArray n arr (# q, r, WORD_SIZE_IN_BITS - r #)

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


shiftRNatural :: Natural -> Int -> Natural
shiftRNatural !(Natural !n !arr) !i
    | i < WORD_SIZE_IN_BITS =
            smallShiftRArray n arr (# i, WORD_SIZE_IN_BITS - i #)
    | otherwise = do
            let (!q, !r) = quotRem i WORD_SIZE_IN_BITS
            if q >= n
                then zeroNatural
                else if r == 0
                    then wordShiftRArray n arr q
                    else largeShiftRArray n arr (# q, r, WORD_SIZE_IN_BITS - r #)


smallShiftRArray :: Int -> WordArray -> (# Int, Int #) -> Natural
smallShiftRArray !n !arr (# !si, !sj #) = runStrictPrim $ do
    marr <- newWordArray n
    loop marr (n - 1) 0
    narr <- unsafeFreezeWordArray marr
    return $! Natural n narr
  where
    loop !marr !i !mem
        | i >= 0 =  do
            x <- indexWordArrayM arr i
            writeWordArray marr i ((unsafeShiftR x si) .|. mem)
            loop marr (i - 1) (unsafeShiftL x sj)
        | otherwise = return ()

wordShiftRArray :: Int -> WordArray -> Int -> Natural
wordShiftRArray !n !arr !q = runStrictPrim $ do
    marr <- newWordArray (n - q)
    copyWordArray marr 0 arr q (n - q)
    narr <- unsafeFreezeWordArray marr
    return $! Natural (n - q) narr

largeShiftRArray :: Int -> WordArray-> (# Int, Int, Int #) -> Natural
largeShiftRArray !n !arr (# !q, !si, !sj #) = runStrictPrim $ do
    marr <- newWordArray (n - q)
    loop marr (n - q - 1) 0
    narr <- unsafeFreezeWordArray marr
    return $! Natural (n - q) narr
  where
    loop !marr !i !mem
        | i >= 0 =  do
            x <- indexWordArrayM arr (q + i)
            writeWordArray marr i ((unsafeShiftR x si) .|. mem)
            loop marr (i - 1) (unsafeShiftL x sj)
        | otherwise = return ()

{-# NOINLINE plusNaturalW #-}
plusNaturalW :: Natural -> Word -> Natural
plusNaturalW !(Natural !n !arr) !w = runStrictPrim $ do
    marr <- newWordArray (n + 1)
    x <- indexWordArrayM arr 0
    let (# !cry, !sm #) = plusWord2 x w
    writeWordArray marr 0 sm
    nlen <- loop1 marr 1 cry
    narr <- unsafeFreezeWordArray marr
    return $! Natural nlen narr
  where
    loop1 !marr !i !carry
        | carry == 0 = loop2 marr i
        | i < n =  do
            x <- indexWordArrayM arr i
            let (# !cry, !sm #) = plusWord2 x carry
            writeWordArray marr i sm
            loop1 marr (i + 1) cry
        | otherwise = do
            writeWordArray marr i carry
            return $ n + 1
    loop2 !marr !i
        | i < n =  do
            x <- indexWordArrayM arr i
            writeWordArray marr i x
            loop2 marr (i + 1)
        | otherwise = return i

{-# NOINLINE plusNatural #-}
plusNatural :: Natural -> Natural -> Natural
plusNatural !a@(Natural !n1 !arr1) !b@(Natural !n2 !arr2)
    | n1 < n2 = plusNatural b a
    | otherwise = runStrictPrim $ do
        marr <- newWordArray (n1 + 1)
        nlen <- loop1 marr 0 0
        narr <- unsafeFreezeWordArray marr
        return $! Natural nlen narr
  where
    loop1 !marr !i !carry
        | i < n2 = do
            x <- indexWordArrayM arr1 i
            y <- indexWordArrayM arr2 i
            let (# !cry, !sm #) = plusWord3 x y carry
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


{-# NOINLINE minusNaturalW #-}
minusNaturalW :: Natural -> Word -> Natural
minusNaturalW !(Natural !n !arr) !w = runStrictPrim $ do
    marr <- newWordArray (n + 1)
    x <- indexWordArrayM arr 0
    let (# !c, !d #) = minusWord2 x w
    writeWordArray marr 0 d
    nlen <- loop1 marr 1 c
    narr <- unsafeFreezeWordArray marr
    finalizeNatural nlen narr
  where
    loop1 !marr !i !carry
        | carry == 0 = loop2 marr i
        | i < n =  do
            x <- indexWordArrayM arr i
            let (# !c, !d #) = minusWord2 x carry
            writeWordArray marr i d
            loop1 marr (i + 1) c
        | otherwise = do
            writeWordArray marr i carry
            return $ n + 1
    loop2 !marr !i
        | i < n =  do
            x <- indexWordArrayM arr i
            writeWordArray marr i x
            loop2 marr (i + 1)
        | otherwise = return n


{-# NOINLINE minusNatural #-}
minusNatural :: Natural -> Natural -> Natural
minusNatural !a@(Natural !n1 !arr1) !b@(Natural !n2 !arr2)
    | n1 < n2 = plusNatural b a
    | otherwise = runStrictPrim $ do
        marr <- newWordArray (n1 + 1)
        nlen <- loop1 marr 0 0
        narr <- unsafeFreezeWordArray marr
        finalizeNatural nlen narr
  where
    loop1 !marr !i !carry
        | i < n2 = do
            x <- indexWordArrayM arr1 i
            y <- indexWordArrayM arr2 i
            let (# !c, !d #) = minusWord2C x y carry
            writeWordArray marr i d
            loop1 marr (i + 1) c
        | otherwise = loop2 marr i carry
    loop2 !marr !i !carry
        | carry == 0 = loop3 marr i
        | i < n1 = do
            x <- indexWordArrayM arr1 i
            let (# !c, !d #) = minusWord2 x carry
            writeWordArray marr i d
            loop2 marr (i + 1) c
        | otherwise = do
            writeWordArray marr i carry
            return (i + 1)
    loop3 !marr !i
        | i < n1 = do
            x <- indexWordArrayM arr1 i
            writeWordArray marr i x
            loop3 marr (i + 1)
        | otherwise = return i


{-# NOINLINE timesNaturalW #-}
timesNaturalW :: Natural -> Word -> Natural
timesNaturalW !(Natural !n !arr) !w = runStrictPrim $ do
    marr <- newWordArray (n + 1)
    nlen <- loop marr 0 0
    narr <- unsafeFreezeWordArray marr
    return $! Natural nlen narr
  where
    loop !marr !i !carry
        | i < n = do
            x <- indexWordArrayM arr i
            let (# !c, !p #) = timesWord2C x w carry
            writeWordArray marr i p
            loop marr (i + 1) c
        | carry /= 0 = do
            writeWordArray marr i carry
            return (i + 1)
        | otherwise = return i

{-# NOINLINE timesNatural #-}
timesNatural :: Natural -> Natural -> Natural
timesNatural !a@(Natural !n1 _) !b@(Natural !n2 _)
    | n1 < n2 = timesNatural b a
    | otherwise = runStrictPrim $ withMultiplyContext a b timesNaturalP


-- This one is broken. It fails because we take the Address of a ByteArray
-- and the ByteArray my be moved by the GC while we're still trying too access
-- it via the pointed.
{-# INLINE withMultiplyContextBroken #-}
withMultiplyContextBroken :: PrimMonad m => Natural -> Natural -> (NaturalP -> NaturalP -> m Natural) -> m Natural
withMultiplyContextBroken (Natural n0 arr0) (Natural n1 arr1) times = do
    res <- times (NaturalP n0 $ wordArrayContents arr0) (NaturalP n1 $ wordArrayContents arr1)
    touch arr0
    touch arr1
    pure res

-- To get around the issue above, we copy the two operands into pinned memory
-- (which is *never* moved by the GC, but is aonly used for the duration of the
-- computation.
{-# INLINE withMultiplyContext #-}
withMultiplyContext :: PrimMonad m => Natural -> Natural -> (NaturalP -> NaturalP -> m Natural) -> m Natural
withMultiplyContext (Natural n0 arr0) (Natural n1 arr1) times = do
    marr <- newPinnedWordArray $ n0 + n1
    copyWordArray marr 0 arr0 0 n0
    copyWordArray marr n0 arr1 0 n1
    narr <- unsafeFreezeWordArray marr
    let addr = wordArrayContents narr
    res <- times (NaturalP n0 addr) (NaturalP n1 $ plusWordAddr n0 addr)
    touch narr
    pure res


{-# INLINE timesNaturalP #-}
timesNaturalP :: PrimMonad m => NaturalP -> NaturalP -> m Natural
timesNaturalP (NaturalP n1 arr1) (NaturalP n2 arr2) = do
    marr <- newWordArray (n1 + n2)
    len <- case 10 * n1 + n2 of
                22 -> timesNat2x2 arr1 arr2 marr
                32 -> timesNat3x2 arr1 arr2 marr
                33 -> timesNat3x3 arr1 arr2 marr
                42 -> timesNat4x2 arr1 arr2 marr
                43 -> timesNat4x3 arr1 arr2 marr
                44 -> timesNat4x4 arr1 arr2 marr
                _ -> timesNaturalWA n1 arr1 n2 arr2 marr
    narr <- unsafeFreezeWordArray marr
    return $! Natural len narr

{-# INLINE timesNat2x2 #-}
timesNat2x2 :: PrimMonad m => WordAddr -> WordAddr -> MutableWordArray m -> m Int
timesNat2x2 !xarr !yarr !marr = do
    x0 <- indexWordAddrM xarr 0
    y0 <- indexWordAddrM yarr 0
    let (# pc1a, prod0a #) = timesWord2 x0 y0
    writeWordArray marr 0 prod0a
    y1 <- indexWordAddrM yarr 1
    let (# pc2a, prod1a #) = timesWord2 x0 y1
    x1 <- indexWordAddrM xarr 1
    let (# pc2b, prod1b #) = timesWord2 x1 y0
    let (# sc2a, sum1a #) = plusWord2 prod1b prod1a
    let (# sc2b, sum1b #) = plusWord2 pc1a sum1a
    writeWordArray marr 1 sum1b
    let (# pc3a, prod2a #) = timesWord2 x1 y1
    let (# sc3a, sum2a #) = plusWord2 prod2a pc2b
    let sum2b = plusWord pc2a sc2a
    let (# sc3b, sum2c #) = plusWord2 sc2b sum2a
    let (# sc3c, sum2d #) = plusWord2 sum2b sum2c
    writeWordArray marr 2 sum2d
    let sum3a = plusWord pc3a sc3a
    let sum3b = plusWord sc3b sc3c
    let sum3c = plusWord sum3a sum3b
    writeWordArray marr 3 sum3c
    return $ 3 + boxInt# (neWord# (unboxWord sum3c) 0##)

{-# INLINE timesNat3x2 #-}
timesNat3x2 :: PrimMonad m => WordAddr -> WordAddr -> MutableWordArray m -> m Int
timesNat3x2 !xarr !yarr !marr = do
    x0 <- indexWordAddrM xarr 0
    y0 <- indexWordAddrM yarr 0
    let (# pc1a, prod0a #) = timesWord2 x0 y0
    writeWordArray marr 0 prod0a
    y1 <- indexWordAddrM yarr 1
    let (# pc2a, prod1a #) = timesWord2 x0 y1
    x1 <- indexWordAddrM xarr 1
    let (# pc2b, prod1b #) = timesWord2 x1 y0
    let (# sc2a, sum1a #) = plusWord2 prod1b prod1a
    let (# sc2b, sum1b #) = plusWord2 pc1a sum1a
    writeWordArray marr 1 sum1b
    let (# pc3a, prod2a #) = timesWord2 x1 y1
    x2 <- indexWordAddrM xarr 2
    let (# pc3b, prod2b #) = timesWord2 x2 y0
    let (# sc3a, sum2a #) = plusWord2 prod2b prod2a
    let (# sc3b, sum2b #) = plusWord2 pc2b pc2a
    let sum2c = plusWord sc2a sc2b
    let (# sc3c, sum2d #) = plusWord2 sum2a sum2b
    let (# sc3d, sum2e #) = plusWord2 sum2c sum2d
    writeWordArray marr 2 sum2e
    let (# pc4a, prod3a #) = timesWord2 x2 y1
    let (# sc4a, sum3a #) = plusWord2 prod3a pc3b
    let sum3b = plusWord pc3a sc3a
    let sum3c = plusWord sc3b sc3c
    let (# sc4b, sum3d #) = plusWord2 sc3d sum3a
    let (# sc4c, sum3e #) = plusWord2 sum3b sum3c
    let (# sc4d, sum3f #) = plusWord2 sum3d sum3e
    writeWordArray marr 3 sum3f
    let sum4a = plusWord pc4a sc4a
    let sum4b = plusWord sc4b sc4c
    let sum4c = plusWord sc4d sum4a
    let sum4d = plusWord sum4b sum4c
    writeWordArray marr 4 sum4d
    return $ 4 + boxInt# (neWord# (unboxWord sum4d) 0##)

{-# INLINE timesNat3x3 #-}
timesNat3x3 :: PrimMonad m => WordAddr -> WordAddr -> MutableWordArray m -> m Int
timesNat3x3 !xarr !yarr !marr = do
    x0 <- indexWordAddrM xarr 0
    y0 <- indexWordAddrM yarr 0
    let (# pc1a, prod0a #) = timesWord2 x0 y0
    writeWordArray marr 0 prod0a
    y1 <- indexWordAddrM yarr 1
    let (# pc2a, prod1a #) = timesWord2 x0 y1
    x1 <- indexWordAddrM xarr 1
    let (# pc2b, prod1b #) = timesWord2 x1 y0
    let (# sc2a, sum1a #) = plusWord2 prod1b prod1a
    let (# sc2b, sum1b #) = plusWord2 pc1a sum1a
    writeWordArray marr 1 sum1b
    let (# pc3a, prod2a #) = timesWord2 x1 y1
    y2 <- indexWordAddrM yarr 2
    let (# pc3b, prod2b #) = timesWord2 x0 y2
    x2 <- indexWordAddrM xarr 2
    let (# pc3c, prod2c #) = timesWord2 x2 y0
    let (# sc3a, sum2a #) = plusWord2 prod2c prod2b
    let (# sc3b, sum2b #) = plusWord2 prod2a pc2b
    let sum2c = plusWord pc2a sc2a
    let (# sc3c, sum2d #) = plusWord2 sc2b sum2a
    let (# sc3d, sum2e #) = plusWord2 sum2b sum2c
    let (# sc3e, sum2f #) = plusWord2 sum2d sum2e
    writeWordArray marr 2 sum2f
    let (# pc4a, prod3a #) = timesWord2 x1 y2
    let (# pc4b, prod3b #) = timesWord2 x2 y1
    let (# sc4a, sum3a #) = plusWord2 prod3b prod3a
    let (# sc4b, sum3b #) = plusWord2 pc3c pc3b
    let sum3c = plusWord pc3a sc3a
    let sum3d = plusWord sc3b sc3c
    let sum3e = plusWord sc3d sc3e
    let (# sc4c, sum3f #) = plusWord2 sum3a sum3b
    let (# sc4d, sum3g #) = plusWord2 sum3c sum3d
    let (# sc4e, sum3h #) = plusWord2 sum3e sum3f
    let (# sc4f, sum3i #) = plusWord2 sum3g sum3h
    writeWordArray marr 3 sum3i
    let (# pc5a, prod4a #) = timesWord2 x2 y2
    let (# sc5a, sum4a #) = plusWord2 prod4a pc4b
    let sum4b = plusWord pc4a sc4a
    let sum4c = plusWord sc4b sc4c
    let sum4d = plusWord sc4d sc4e
    let (# sc5b, sum4e #) = plusWord2 sc4f sum4a
    let (# sc5c, sum4f #) = plusWord2 sum4b sum4c
    let (# sc5d, sum4g #) = plusWord2 sum4d sum4e
    let (# sc5e, sum4h #) = plusWord2 sum4f sum4g
    writeWordArray marr 4 sum4h
    let sum5a = plusWord pc5a sc5a
    let sum5b = plusWord sc5b sc5c
    let sum5c = plusWord sc5d sc5e
    let sum5d = plusWord sum5a sum5b
    let sum5e = plusWord sum5c sum5d
    writeWordArray marr 5 sum5e
    return $ 5 + boxInt# (neWord# (unboxWord sum5e) 0##)


{-# INLINE timesNat4x2 #-}
timesNat4x2 :: PrimMonad m => WordAddr -> WordAddr -> MutableWordArray m -> m Int
timesNat4x2 !xarr !yarr !marr = do
    x0 <- indexWordAddrM xarr 0
    y0 <- indexWordAddrM yarr 0
    let (# pc1a, prod0a #) = timesWord2 x0 y0
    writeWordArray marr 0 prod0a
    y1 <- indexWordAddrM yarr 1
    let (# pc2a, prod1a #) = timesWord2 x0 y1
    x1 <- indexWordAddrM xarr 1
    let (# pc2b, prod1b #) = timesWord2 x1 y0
    let (# sc2a, sum1a #) = plusWord2 prod1b prod1a
    let (# sc2b, sum1b #) = plusWord2 pc1a sum1a
    writeWordArray marr 1 sum1b
    let (# pc3a, prod2a #) = timesWord2 x1 y1
    x2 <- indexWordAddrM xarr 2
    let (# pc3b, prod2b #) = timesWord2 x2 y0
    let (# sc3a, sum2a #) = plusWord2 prod2b prod2a
    let (# sc3b, sum2b #) = plusWord2 pc2b pc2a
    let sum2c = plusWord sc2a sc2b
    let (# sc3c, sum2d #) = plusWord2 sum2a sum2b
    let (# sc3d, sum2e #) = plusWord2 sum2c sum2d
    writeWordArray marr 2 sum2e
    let (# pc4a, prod3a #) = timesWord2 x2 y1
    x3 <- indexWordAddrM xarr 3
    let (# pc4b, prod3b #) = timesWord2 x3 y0
    let (# sc4a, sum3a #) = plusWord2 prod3b prod3a
    let (# sc4b, sum3b #) = plusWord2 pc3b pc3a
    let sum3c = plusWord sc3a sc3b
    let sum3d = plusWord sc3c sc3d
    let (# sc4c, sum3e #) = plusWord2 sum3a sum3b
    let (# sc4d, sum3f #) = plusWord2 sum3c sum3d
    let (# sc4e, sum3g #) = plusWord2 sum3e sum3f
    writeWordArray marr 3 sum3g
    let (# pc5a, prod4a #) = timesWord2 x3 y1
    let (# sc5a, sum4a #) = plusWord2 prod4a pc4b
    let sum4b = plusWord pc4a sc4a
    let sum4c = plusWord sc4b sc4c
    let sum4d = plusWord sc4d sc4e
    let (# sc5b, sum4e #) = plusWord2 sum4a sum4b
    let (# sc5c, sum4f #) = plusWord2 sum4c sum4d
    let (# sc5d, sum4g #) = plusWord2 sum4e sum4f
    writeWordArray marr 4 sum4g
    let sum5a = plusWord pc5a sc5a
    let sum5b = plusWord sc5b sc5c
    let sum5c = plusWord sc5d sum5a
    let sum5d = plusWord sum5b sum5c
    writeWordArray marr 5 sum5d
    return $ 5 + boxInt# (neWord# (unboxWord sum5d) 0##)

{-# INLINE timesNat4x3 #-}
timesNat4x3 :: PrimMonad m => WordAddr -> WordAddr -> MutableWordArray m -> m Int
timesNat4x3 !xarr !yarr !marr = do
    x0 <- indexWordAddrM xarr 0
    y0 <- indexWordAddrM yarr 0
    let (# pc1a, prod0a #) = timesWord2 x0 y0
    writeWordArray marr 0 prod0a
    y1 <- indexWordAddrM yarr 1
    let (# pc2a, prod1a #) = timesWord2 x0 y1
    x1 <- indexWordAddrM xarr 1
    let (# pc2b, prod1b #) = timesWord2 x1 y0
    let (# sc2a, sum1a #) = plusWord2 prod1b prod1a
    let (# sc2b, sum1b #) = plusWord2 pc1a sum1a
    writeWordArray marr 1 sum1b
    let (# pc3a, prod2a #) = timesWord2 x1 y1
    y2 <- indexWordAddrM yarr 2
    let (# pc3b, prod2b #) = timesWord2 x0 y2
    x2 <- indexWordAddrM xarr 2
    let (# pc3c, prod2c #) = timesWord2 x2 y0
    let (# sc3a, sum2a #) = plusWord2 prod2c prod2b
    let (# sc3b, sum2b #) = plusWord2 prod2a pc2b
    let sum2c = plusWord pc2a sc2a
    let (# sc3c, sum2d #) = plusWord2 sc2b sum2a
    let (# sc3d, sum2e #) = plusWord2 sum2b sum2c
    let (# sc3e, sum2f #) = plusWord2 sum2d sum2e
    writeWordArray marr 2 sum2f
    let (# pc4a, prod3a #) = timesWord2 x1 y2
    let (# pc4b, prod3b #) = timesWord2 x2 y1
    x3 <- indexWordAddrM xarr 3
    let (# pc4c, prod3c #) = timesWord2 x3 y0
    let (# sc4a, sum3a #) = plusWord2 prod3c prod3b
    let (# sc4b, sum3b #) = plusWord2 prod3a pc3c
    let (# sc4c, sum3c #) = plusWord2 pc3b pc3a
    let sum3d = plusWord sc3a sc3b
    let sum3e = plusWord sc3c sc3d
    let (# sc4d, sum3f #) = plusWord2 sc3e sum3a
    let (# sc4e, sum3g #) = plusWord2 sum3b sum3c
    let (# sc4f, sum3h #) = plusWord2 sum3d sum3e
    let (# sc4g, sum3i #) = plusWord2 sum3f sum3g
    let (# sc4h, sum3j #) = plusWord2 sum3h sum3i
    writeWordArray marr 3 sum3j
    let (# pc5a, prod4a #) = timesWord2 x2 y2
    let (# pc5b, prod4b #) = timesWord2 x3 y1
    let (# sc5a, sum4a #) = plusWord2 prod4b prod4a
    let (# sc5b, sum4b #) = plusWord2 pc4c pc4b
    let sum4c = plusWord pc4a sc4a
    let sum4d = plusWord sc4b sc4c
    let sum4e = plusWord sc4d sc4e
    let sum4f = plusWord sc4f sc4g
    let (# sc5c, sum4g #) = plusWord2 sc4h sum4a
    let (# sc5d, sum4h #) = plusWord2 sum4b sum4c
    let (# sc5e, sum4i #) = plusWord2 sum4d sum4e
    let (# sc5f, sum4j #) = plusWord2 sum4f sum4g
    let (# sc5g, sum4k #) = plusWord2 sum4h sum4i
    let (# sc5h, sum4l #) = plusWord2 sum4j sum4k
    writeWordArray marr 4 sum4l
    let (# pc6a, prod5a #) = timesWord2 x3 y2
    let (# sc6a, sum5a #) = plusWord2 prod5a pc5b
    let sum5b = plusWord pc5a sc5a
    let sum5c = plusWord sc5b sc5c
    let sum5d = plusWord sc5d sc5e
    let sum5e = plusWord sc5f sc5g
    let (# sc6b, sum5f #) = plusWord2 sc5h sum5a
    let (# sc6c, sum5g #) = plusWord2 sum5b sum5c
    let (# sc6d, sum5h #) = plusWord2 sum5d sum5e
    let (# sc6e, sum5i #) = plusWord2 sum5f sum5g
    let (# sc6f, sum5j #) = plusWord2 sum5h sum5i
    writeWordArray marr 5 sum5j
    let sum6a = plusWord pc6a sc6a
    let sum6b = plusWord sc6b sc6c
    let sum6c = plusWord sc6d sc6e
    let sum6d = plusWord sc6f sum6a
    let sum6e = plusWord sum6b sum6c
    let sum6f = plusWord sum6d sum6e
    writeWordArray marr 6 sum6f
    return $ 6 + boxInt# (neWord# (unboxWord sum6f) 0##)

{-# INLINE timesNat4x4 #-}
timesNat4x4 :: PrimMonad m => WordAddr -> WordAddr -> MutableWordArray m -> m Int
timesNat4x4 !xarr !yarr !marr = do
    x0 <- indexWordAddrM xarr 0
    y0 <- indexWordAddrM yarr 0
    let (# pc1a, prod0a #) = timesWord2 x0 y0
    writeWordArray marr 0 prod0a
    y1 <- indexWordAddrM yarr 1
    let (# pc2a, prod1a #) = timesWord2 x0 y1
    x1 <- indexWordAddrM xarr 1
    let (# pc2b, prod1b #) = timesWord2 x1 y0
    let (# sc2a, sum1a #) = plusWord2 prod1b prod1a
    let (# sc2b, sum1b #) = plusWord2 pc1a sum1a
    writeWordArray marr 1 sum1b
    let (# pc3a, prod2a #) = timesWord2 x1 y1
    y2 <- indexWordAddrM yarr 2
    let (# pc3b, prod2b #) = timesWord2 x0 y2
    x2 <- indexWordAddrM xarr 2
    let (# pc3c, prod2c #) = timesWord2 x2 y0
    let (# sc3a, sum2a #) = plusWord2 prod2c prod2b
    let (# sc3b, sum2b #) = plusWord2 prod2a pc2b
    let sum2c = plusWord pc2a sc2a
    let (# sc3c, sum2d #) = plusWord2 sc2b sum2a
    let (# sc3d, sum2e #) = plusWord2 sum2b sum2c
    let (# sc3e, sum2f #) = plusWord2 sum2d sum2e
    writeWordArray marr 2 sum2f
    let (# pc4a, prod3a #) = timesWord2 x1 y2
    y3 <- indexWordAddrM yarr 3
    let (# pc4b, prod3b #) = timesWord2 x0 y3
    let (# pc4c, prod3c #) = timesWord2 x2 y1
    x3 <- indexWordAddrM xarr 3
    let (# pc4d, prod3d #) = timesWord2 x3 y0
    let (# sc4a, sum3a #) = plusWord2 prod3d prod3c
    let (# sc4b, sum3b #) = plusWord2 prod3b prod3a
    let (# sc4c, sum3c #) = plusWord2 pc3c pc3b
    let sum3d = plusWord pc3a sc3a
    let sum3e = plusWord sc3b sc3c
    let sum3f = plusWord sc3d sc3e
    let (# sc4d, sum3g #) = plusWord2 sum3a sum3b
    let (# sc4e, sum3h #) = plusWord2 sum3c sum3d
    let (# sc4f, sum3i #) = plusWord2 sum3e sum3f
    let (# sc4g, sum3j #) = plusWord2 sum3g sum3h
    let (# sc4h, sum3k #) = plusWord2 sum3i sum3j
    writeWordArray marr 3 sum3k
    let (# pc5a, prod4a #) = timesWord2 x2 y2
    let (# pc5b, prod4b #) = timesWord2 x1 y3
    let (# pc5c, prod4c #) = timesWord2 x3 y1
    let (# sc5a, sum4a #) = plusWord2 prod4c prod4b
    let (# sc5b, sum4b #) = plusWord2 prod4a pc4d
    let (# sc5c, sum4c #) = plusWord2 pc4c pc4b
    let sum4d = plusWord pc4a sc4a
    let sum4e = plusWord sc4b sc4c
    let sum4f = plusWord sc4d sc4e
    let sum4g = plusWord sc4f sc4g
    let (# sc5d, sum4h #) = plusWord2 sc4h sum4a
    let (# sc5e, sum4i #) = plusWord2 sum4b sum4c
    let (# sc5f, sum4j #) = plusWord2 sum4d sum4e
    let (# sc5g, sum4k #) = plusWord2 sum4f sum4g
    let (# sc5h, sum4l #) = plusWord2 sum4h sum4i
    let (# sc5i, sum4m #) = plusWord2 sum4j sum4k
    let (# sc5j, sum4n #) = plusWord2 sum4l sum4m
    writeWordArray marr 4 sum4n
    let (# pc6a, prod5a #) = timesWord2 x2 y3
    let (# pc6b, prod5b #) = timesWord2 x3 y2
    let (# sc6a, sum5a #) = plusWord2 prod5b prod5a
    let (# sc6b, sum5b #) = plusWord2 pc5c pc5b
    let sum5c = plusWord pc5a sc5a
    let sum5d = plusWord sc5b sc5c
    let sum5e = plusWord sc5d sc5e
    let sum5f = plusWord sc5f sc5g
    let sum5g = plusWord sc5h sc5i
    let (# sc6c, sum5h #) = plusWord2 sc5j sum5a
    let (# sc6d, sum5i #) = plusWord2 sum5b sum5c
    let (# sc6e, sum5j #) = plusWord2 sum5d sum5e
    let (# sc6f, sum5k #) = plusWord2 sum5f sum5g
    let (# sc6g, sum5l #) = plusWord2 sum5h sum5i
    let (# sc6h, sum5m #) = plusWord2 sum5j sum5k
    let (# sc6i, sum5n #) = plusWord2 sum5l sum5m
    writeWordArray marr 5 sum5n
    let (# pc7a, prod6a #) = timesWord2 x3 y3
    let (# sc7a, sum6a #) = plusWord2 prod6a pc6b
    let sum6b = plusWord pc6a sc6a
    let sum6c = plusWord sc6b sc6c
    let sum6d = plusWord sc6d sc6e
    let sum6e = plusWord sc6f sc6g
    let sum6f = plusWord sc6h sc6i
    let (# sc7b, sum6g #) = plusWord2 sum6a sum6b
    let (# sc7c, sum6h #) = plusWord2 sum6c sum6d
    let (# sc7d, sum6i #) = plusWord2 sum6e sum6f
    let (# sc7e, sum6j #) = plusWord2 sum6g sum6h
    let (# sc7f, sum6k #) = plusWord2 sum6i sum6j
    writeWordArray marr 6 sum6k
    let sum7a = plusWord pc7a sc7a
    let sum7b = plusWord sc7b sc7c
    let sum7c = plusWord sc7d sc7e
    let sum7d = plusWord sc7f sum7a
    let sum7e = plusWord sum7b sum7c
    let sum7f = plusWord sum7d sum7e
    writeWordArray marr 7 sum7f
    return $ 7 + boxInt# (neWord# (unboxWord sum7f) 0##)

{-# INLINE timesNaturalWA #-}
timesNaturalWA :: PrimMonad m => Int -> WordAddr -> Int -> WordAddr -> MutableWordArray m -> m Int
timesNaturalWA !n1 !arr1 !n2 !arr2 =
    preLoop
  where
    preLoop marr = do
        x <- indexWordAddrM arr1 0
        y <- indexWordAddrM arr2 0
        let (# cry, prod #) = timesWord2 x y
        writeWordArray marr 0 prod
        outerLoop1 1 marr 0 cry

    outerLoop1 !nx !marr !carryhi !carrylo
        | nx < n2 = do
            (cryhi, crylo, sum) <- innerLoop1xi nx 0 0 carryhi carrylo
            writeWordArray marr nx sum
            outerLoop1 (nx + 1) marr cryhi crylo
        | otherwise = outerLoop1a nx marr carryhi carrylo

    outerLoop1a !nx !marr !carryhi !carrylo
        | nx < n1 - 1 = do
            (cryhi, crylo, sum) <- innerLoop1yi nx 0 0 carryhi carrylo
            writeWordArray marr nx sum
            outerLoop1a (nx + 1) marr cryhi crylo
        | otherwise = outerLoop2 nx marr carryhi carrylo

    innerLoop1xi !xi !yi !carryhi !carrylo !sum
        | xi >= 0 = do
            x <- indexWordAddrM arr1 xi
            y <- indexWordAddrM arr2 yi
            let (# !cry0, !prod #) = timesWord2 x y
                (# !cry1, !sum1 #) = plusWord2 prod sum
                (# !tcryhi, !crylo #) = plusWord3 carrylo cry0 cry1
                !cryhi = plusWord carryhi tcryhi
            innerLoop1xi (xi - 1) (yi + 1) cryhi crylo sum1
        | otherwise = return (carryhi, carrylo, sum)

    innerLoop1yi !xi !yi !carryhi !carrylo !sum
        | yi < n2 = do
            x <- indexWordAddrM arr1 xi
            y <- indexWordAddrM arr2 yi
            let (# !cry0, !prod #) = timesWord2 x y
                (# !cry1, !sum1 #) = plusWord2 prod sum
                (# !tcryhi, !crylo #) = plusWord3 carrylo cry0 cry1
                !cryhi = plusWord carryhi tcryhi
            innerLoop1yi (xi - 1) (yi + 1) cryhi crylo sum1
        | otherwise = return (carryhi, carrylo, sum)

    outerLoop2 !nx !marr !carryhi !carrylo
        | nx < n1 + n2 - 1 = do
            (cryhi, crylo, sum) <- innerLoop2 (n1 - 1) (nx - n1 + 1) 0 carryhi carrylo
            writeWordArray marr nx sum
            outerLoop2 (nx + 1) marr cryhi crylo
        | carrylo /= 0 = do
            writeWordArray marr nx carrylo
            return $! nx + 1
        | otherwise = return $! nx

    innerLoop2 !xi !yi !carryhi !carrylo !sum
        | yi < n2 = do
            x <- indexWordAddrM arr1 xi
            y <- indexWordAddrM arr2 yi
            let (# !cry0, !prod #) = timesWord2 x y
                (# !cry1, !sum1 #) = plusWord2 prod sum
                (# !tcryhi, !crylo #) = plusWord3 carrylo cry0 cry1
                !cryhi = plusWord carryhi tcryhi
            innerLoop2 (xi - 1) (yi + 1) cryhi crylo sum1
        | otherwise = return $ (carryhi, carrylo, sum)


quotRemNaturalW :: Natural -> Word -> (Natural, Word)
quotRemNaturalW !(Natural !n !arr) !w = runStrictPrim $ do
    qlen <- return $! if w >= indexWordArray arr (n - 1) then n - 1 else n
    qmarr <- newWordArray qlen
    rem <- loop (n - 1) qmarr 0
    qarr <- unsafeFreezeWordArray qmarr
    return $! (Natural qlen qarr, rem)
  where
    loop i qmarr rem
        | i >= 0 = do
            x <- indexWordArrayM arr i
            let (# q, r #) = quotRemWord2 rem x w
            writeWordArray qmarr i q
            loop (i - 1) qmarr r
        | otherwise = return rem

{-
https://www.khanacademy.org/math/arithmetic/multiplication-division/partial_quotient_division/v/partial-quotient-method-of-division-2
http://en.wikipedia.org/wiki/Division_algorithm
http://en.wikipedia.org/wiki/Barrett_reduction
http://en.wikipedia.org/wiki/Euclidean_division
-}


quotRemNatural :: Natural -> Natural -> (Natural, Natural)
quotRemNatural !numer@(Natural !nn !narr) !denom@(Natural !dn !darr)
    | dn > nn = (zeroNatural, numer)
    | dn == nn && indexWordArray narr (nn - 1) < indexWordArray darr (nn - 1) = (zeroNatural, numer)
    | dn == nn && indexWordArray narr (nn - 1) == indexWordArray darr (nn - 1) =
                    if ltNatural numer denom
                        then (zeroNatural, numer)
                        else (oneNatural, minusNatural numer denom)
#if 0
    | otherwise = error ("New3/GHC/Integer/Natural.hs: line " ++ show (__LINE__ :: Int))
#else
    | otherwise = divideOnceNatural numer denom

{-
      runStrictPrim $ do
        qlen <- return $! if indexWordArray darr (dn - 1) >= indexWordArray narr (nn - 1) then n - 1 else n
        qmarr <- newWordArray n
        rmarr <- newWordArray n
        rem <- loop (n - 1) qmarr 0
        qarr <- unsafeFreezeWordArray qmarr
        return $! (Natural qlen qarr, rem)

divideSimple :: Natural -> Natural -> (Natural, Natural)
divideSimple numer denom =
    let nd# = encodeDoubleNatural numer 0#
        dd# = encodeDoubleNatural denom 0#
        q = decodeDoubleNatural (nd# /## dd#)
    in (q, minusNatural numer (timesNatural q denom))

-}


#endif

divideOnceNatural :: Natural -> Natural -> (Natural, Natural)
divideOnceNatural numer denom =
    let !(q1, r1) = partialQuotient numer
    in case q1 of
        Natural 0 _ -> (q1, r1)
        _ ->
            let !(!q2, !r2) = divideOnceNatural r1 denom
            in (plusNatural q1 q2, r2)
  where
    {-# INLINE partialQuotient #-}
    partialQuotient :: Natural -> (Natural, Natural)
    partialQuotient num =
        let (m, s) = partialQuotientWord num denom
        in case m of
            0 -> (zeroNatural, num)
            _ ->
                let !qt = shiftLNatural (wordToNatural m) (64 * s)
                    !pr = timesNatural denom qt
                    !rm  = minusNatural num pr
                in (qt, rm)

    {-# INLINE partialQuotientWord #-}
    partialQuotientWord :: Natural -> Natural -> (Word, Int)
    partialQuotientWord !(Natural !nn !narr) !(Natural !dn !darr) =
        let !n0 = indexWordArray narr (nn - 1)
            !d0 = indexWordArray darr (dn - 1)
        in case (# compare dn nn, compare n0 d0 #) of
            (# GT, _ #) -> (0, 0)
            (# EQ, LT #) -> (0, 0)
            (# EQ, EQ #) -> (1, nn - dn)
            (# EQ, GT #) -> case quotRemWord n0 d0 of (# !q, _ #) -> (q, nn - dn)
            (# LT, LT #) -> case quotRemWord2 n0 (indexWordArray narr $ nn - 2) d0 of (# !q, _ #) -> (q, nn - dn - 1)
            (# LT, _ #) -> case quotRemWord n0 d0 of (# !q, _ #) -> (q, nn - dn)


eqNatural :: Natural -> Natural -> Bool
eqNatural !(Natural !n1 !arr1) !(Natural !n2 !arr2)
    | n1 /= n2 = False
    | otherwise =
        let eqArray !idx
                | idx < 0 = True
                | indexWordArray arr1 idx /= indexWordArray arr2 idx = False
                | otherwise = eqArray (idx - 1)
        in eqArray (n1 - 1)

compareNatural :: Natural -> Natural -> Ordering
compareNatural !(Natural !n1 !arr1) !(Natural !n2 !arr2)
    | n1 < n2 = LT
    | n1 > n2 = GT
    | otherwise =
        let cmpArray !idx
                | idx < 0 = EQ
                | otherwise =
                    case compare (indexWordArray arr1 idx) (indexWordArray arr2 idx) of
                        EQ -> cmpArray (idx - 1)
                        cmp -> cmp
        in cmpArray (n1 - 1)


ltNatural :: Natural -> Natural -> Bool
ltNatural !(Natural !n1 !arr1) !(Natural !n2 !arr2)
    | n1 < n2 = True
    | n1 > n2 = False
    | otherwise =
        let check 0 = indexWordArray arr1 0 < indexWordArray arr2 0
            check i =
                if indexWordArray arr1 i == indexWordArray arr2 i
                    then check (i - 1)
                    else indexWordArray arr1 i < indexWordArray arr2 i
        in check (n1 - 1)


gtNatural :: Natural -> Natural -> Bool
gtNatural !(Natural !n1 !arr1) !(Natural !n2 !arr2)
    | n1 > n2 = True
    | n1 < n2 = False
    | otherwise =
            let check 0 = indexWordArray arr1 0 > indexWordArray arr2 0
                check i =
                    if indexWordArray arr1 i == indexWordArray arr2 i
                        then check (i - 1)
                        else indexWordArray arr1 i > indexWordArray arr2 i
            in check (n1 - 1)


--------------------------------------------------------------------------------
-- Helpers (not part of the API).

{-# INLINE zerothWordOfNatural #-}
zerothWordOfNatural :: Natural -> Word
zerothWordOfNatural !(Natural _ arr) = indexWordArray arr 0

mkSingletonNat :: Word -> Natural
mkSingletonNat !x =
    runStrictPrim $ do
        marr <- newWordArray 1
        writeWordArray marr 0 x
        narr <- unsafeFreezeWordArray marr
        return $ Natural 1 narr


finalizeNatural :: Int -> WordArray -> StrictPrim s Natural
finalizeNatural 0 _ = return zeroNatural
finalizeNatural !nin !arr = do
    let !len = nonZeroLen nin arr
    x <- indexWordArrayM arr 0
    return $
        if len < 0 || (len == 1 && x == 0)
            then zeroNatural
            else Natural len arr

nonZeroLen :: Int -> WordArray -> Int
nonZeroLen !len !arr
    | len < 1 = 0
    | otherwise =
        let trim i
                | i < 0 = 0
                | indexWordArray arr i == 0 = trim (i - 1)
                | otherwise = i + 1
        in trim (len - 1)


zeroNatural, oneNatural :: Natural
zeroNatural = runStrictPrim $ do
        marr <- newWordArray 1
        writeWordArray marr 0 0
        narr <- unsafeFreezeWordArray marr
        return $! Natural 0 narr
oneNatural = wordToNatural 1

wordToNatural :: Word -> Natural
wordToNatural w = runStrictPrim $ do
        marr <- newWordArray 1
        writeWordArray marr 0 w
        narr <- unsafeFreezeWordArray marr
        return $! Natural 1 narr


arrayShow :: Int -> WordArray -> String
arrayShow !len !arr =
    let hexify w =
            let x = showHexW w
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

traceNatural :: Int -> Natural -> Natural
traceNatural linenum nat@(Natural n arr) =
    if n <= 0
        then error $ "Bad natural (" ++ show linenum ++ ") " ++ show n ++ " " ++ arrayShow n arr
        else nat

errorLine :: Int -> String -> a
errorLine linenum s = error $ "Line " ++ show linenum ++ ": " ++ s

#endif
