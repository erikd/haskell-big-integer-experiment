{-# LANGUAGE BangPatterns, Strict #-}
module Common.GHC.Integer.WordArray where

import Control.Monad
import Control.Monad.Primitive
import Data.Primitive

import Common.GHC.Integer.StrictPrim

-- The `primitive` package supports two array types, `ByteArray` (and array of
-- bytes) and `Array a` (an array of boxed values). Therefore use the former to
-- build a `WordArray` an array of unboxed `Word` values. Benchmarking showed
-- this `WordArray` implementation to be about 4 times faster than `Array Word`
-- on trivial operations like `copyWordArray`.


newtype WordArray = WA ByteArray

newtype MutableWordArray = MWA (MutableByteArray RealWorld)

{-# INLINE newWordArray #-}
newWordArray :: Int -> StrictPrim MutableWordArray
newWordArray !len = do
    !marr <- newByteArray (len * sizeOf (0 :: Word))
    pure $ MWA marr

{-# INLINE newPinnedWordArray #-}
newPinnedWordArray :: Int -> StrictPrim MutableWordArray
newPinnedWordArray !len = do
    !marr <- newPinnedByteArray (len * sizeOf (0 :: Word))
    pure $ MWA marr

-- | allocaWords : Create a temporary array of Word to used for the duration
-- of the provided computation. Idea stolen from Foriegn.Marshal.Alloc.
{-# INLINE allocaWords #-}
allocaWords :: Int -> (WordArray -> StrictPrim b) -> StrictPrim b
allocaWords len computation = do
    marr <- newPinnedByteArray (len * sizeOf (0 :: Word))
    narr <- unsafeFreezeByteArray marr
    res <- computation $ WA narr
    touch narr
    pure res

-- | newPlaceholderWordArray : Create a place holder ByteArray for timesInteger
-- where a zero length ByteArray is needed. Memory is actually allocated, but
-- nothing is written to it os it will actually contain junk data.
newPlaceholderWordArray :: StrictPrim WordArray
newPlaceholderWordArray = do
    !marr <- newByteArray (sizeOf (0 :: Word))
    unsafeFreezeWordArray (MWA marr)

cloneWordArrayExtend :: Int -> WordArray -> Int -> StrictPrim MutableWordArray
cloneWordArrayExtend !oldLen (WA !arr) !newLen = do
    !marr <- newByteArray (newLen * sizeOf (0 :: Word))
    when (oldLen > 0) $
        copyByteArray marr 0 arr 0 (min oldLen newLen * sizeOf (0 :: Word))
    setByteArray marr oldLen (max 0 (newLen - oldLen)) (0 :: Word)
    pure $ MWA marr

{-# INLINE readWordArray #-}
readWordArray :: MutableWordArray -> Int -> StrictPrim Word
readWordArray (MWA !marr) = readByteArray marr

{-# INLINE unsafeFreezeWordArray #-}
unsafeFreezeWordArray :: MutableWordArray -> StrictPrim WordArray
unsafeFreezeWordArray (MWA !marr) = do
    !arr <- unsafeFreezeByteArray marr
    pure (WA arr)

{-# INLINE indexWordArray #-}
indexWordArray :: WordArray -> Int -> Word
indexWordArray (WA !arr) = indexByteArray arr

{-# INLINE indexWordArrayM #-}
indexWordArrayM :: WordArray -> Int -> StrictPrim Word
indexWordArrayM (WA !arr) !i = case indexByteArray arr i of x -> pure x

{-# INLINE writeWordArray #-}
writeWordArray :: MutableWordArray -> Int -> Word -> StrictPrim ()
writeWordArray (MWA !marr) = writeByteArray marr

{-# INLINE setWordArray #-}
setWordArray :: MutableWordArray -> Int -> Int -> Word -> StrictPrim ()
setWordArray !marr !offset !count !word =
    loop offset (offset + count)
  where
    loop !off !end
        | off < end = do
            writeWordArray marr off word
            loop (off + 1) end
        | otherwise = pure ()

{-# INLINE copyWordArray #-}
copyWordArray :: MutableWordArray -> Int -> WordArray -> Int -> Int -> StrictPrim ()
copyWordArray (MWA !marr) !doff (WA !arr) !soff !wrds =
    let !wordsize = sizeOf (0 :: Word)
    in copyByteArray marr (doff * wordsize) arr (soff * wordsize) (wrds * wordsize)



newtype WordAddr = WordAddr Addr

class ReadWord a where
    readWord :: a -> Int -> Word
    readWordM :: a -> Int -> StrictPrim Word

instance ReadWord WordArray where
    {-# INLINE readWord #-}
    readWord = indexWordArray
    {-# INLINE readWordM #-}
    readWordM = indexWordArrayM

instance ReadWord WordAddr where
    {-# INLINE readWord #-}
    readWord (WordAddr addr) = indexOffAddr addr
    {-# INLINE readWordM #-}
    readWordM (WordAddr addr) = readOffAddr addr

{-# INLINE plusWordAddr #-}
plusWordAddr :: Int -> WordAddr -> WordAddr
plusWordAddr count (WordAddr addr) =
    WordAddr . plusAddr addr $ count * sizeOf (0 :: Word)

{-# INLINE wordArrayContents #-}
wordArrayContents :: WordArray -> WordAddr
wordArrayContents (WA arr) = WordAddr $ byteArrayContents arr

{-# INLINE indexWordAddr #-}
indexWordAddr :: WordAddr -> Int -> Word
indexWordAddr (WordAddr !arr) = indexOffAddr arr

{-# INLINE indexWordAddrM #-}
indexWordAddrM :: WordAddr -> Int -> StrictPrim Word
indexWordAddrM (WordAddr arr) = readOffAddr arr
