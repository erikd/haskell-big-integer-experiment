{-# LANGUAGE BangPatterns, Strict #-}
module Common.GHC.Integer.WordArray where

import Control.Monad
import Control.Monad.Primitive
import Data.Primitive


-- The `primitive` package supports two array types, `ByteArray` (and array of
-- bytes) and `Array a` (an array of boxed values). Therefore use the former to
-- build a `WordArray` an array of unboxed `Word` values. Benchmarking showed
-- this `WordArray` implementation to be about 4 times faster than `Array Word`
-- on trivial operations like `copyWordArray`.


newtype WordArray = WA ByteArray

newtype MutableWordArray m = MWA (MutableByteArray (PrimState m))

{-# INLINE newWordArray #-}
newWordArray :: PrimMonad m => Int -> m (MutableWordArray m)
newWordArray !len = do
    !marr <- newByteArray (len * sizeOf (0 :: Word))
    return $ MWA marr

-- | allocaWords : Create a temporary array of Word to used for the duration
-- of the provided computation. Idea stolen from Foriegn.Marshal.Alloc.
{-# INLINE allocaWords #-}
allocaWords :: PrimMonad m => Int -> (Addr -> m b) -> m b
allocaWords len computation = do
    marr <- newPinnedByteArray (len * sizeOf (0 :: Word))
    narr <- unsafeFreezeByteArray marr
    res <- computation $ byteArrayContents narr
    touch narr
    pure res

-- | newPlaceholderWordArray : Create a place holder ByteArray for timesInteger
-- where a zero length ByteArray is needed. Memory is actually allocated, but
-- nothing is written to it os it will actually contain junk data.
newPlaceholderWordArray :: PrimMonad m => m WordArray
newPlaceholderWordArray = do
    !marr <- newByteArray (sizeOf (0 :: Word))
    unsafeFreezeWordArray (MWA marr)

cloneWordArrayExtend :: PrimMonad m => Int -> WordArray -> Int -> m (MutableWordArray m)
cloneWordArrayExtend !oldLen (WA !arr) !newLen = do
    !marr <- newByteArray (newLen * sizeOf (0 :: Word))
    when (oldLen > 0) $
        copyByteArray marr 0 arr 0 (min oldLen newLen * sizeOf (0 :: Word))
    setByteArray marr oldLen (max 0 (newLen - oldLen)) (0 :: Word)
    return $ MWA marr

{-# INLINE readWordArray #-}
readWordArray :: PrimMonad m => MutableWordArray m -> Int -> m Word
readWordArray (MWA !marr) = readByteArray marr

{-# INLINE unsafeFreezeWordArray #-}
unsafeFreezeWordArray :: PrimMonad m => MutableWordArray m -> m WordArray
unsafeFreezeWordArray (MWA !marr) = do
    !arr <- unsafeFreezeByteArray marr
    return (WA arr)

{-# INLINE indexWordArray #-}
indexWordArray :: WordArray -> Int -> Word
indexWordArray (WA !arr) = indexByteArray arr

{-# INLINE indexWordArrayM #-}
indexWordArrayM :: Monad m => WordArray -> Int -> m Word
indexWordArrayM (WA !arr) !i = case indexByteArray arr i of x -> return x

{-# INLINE writeWordArray #-}
writeWordArray :: PrimMonad m => MutableWordArray m -> Int -> Word -> m ()
writeWordArray (MWA !marr) = writeByteArray marr

{-# INLINE setWordArray #-}
setWordArray :: PrimMonad m => MutableWordArray m -> Int -> Int -> Word -> m ()
setWordArray !marr !offset !count !word =
    loop offset (offset + count)
  where
    loop !off !end
        | off < end = do
            writeWordArray marr off word
            loop (off + 1) end
        | otherwise = return ()

{-# INLINE copyWordArray #-}
copyWordArray :: PrimMonad m => MutableWordArray m -> Int -> WordArray -> Int -> Int -> m ()
copyWordArray (MWA !marr) !doff (WA !arr) !soff !wrds =
    let !wordsize = sizeOf (0 :: Word)
    in copyByteArray marr (doff * wordsize) arr (soff * wordsize) (wrds * wordsize)
