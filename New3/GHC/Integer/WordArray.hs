{-# LANGUAGE BangPatterns #-}
module New3.GHC.Integer.WordArray where

import Control.Monad.Primitive
import Data.Primitive


newtype WordArray = WA ByteArray

newtype MutableWordArray m = MWA (MutableByteArray (PrimState m))

{-# INLINE newWordArray #-}
newWordArray :: (Monad m, PrimMonad m) => Int -> m (MutableWordArray m)
newWordArray !len = do
    !marr <- newByteArray (len * sizeOf (0 :: Word))
    return $ MWA marr

newWordArrayCleared :: (Monad m, PrimMonad m) => Int -> m (MutableWordArray m)
newWordArrayCleared !len = do
    !marr <- newByteArray (len * sizeOf (0 :: Word))
    let !wmarr = MWA marr
    setWordArray wmarr 0 len 0
    return wmarr

-- | newPlaceholderWordArray : Create a place holder ByteArray for timesInteger
-- where a zero length ByteArray is needed. Memory is actually allocated, but
-- nothing is written to it os it will actually contain junk data.
newPlaceholderWordArray :: (Monad m, PrimMonad m) => m WordArray
newPlaceholderWordArray = do
    !marr <- newByteArray (sizeOf (0 :: Word))
    unsafeFreezeWordArray (MWA marr)

cloneWordArrayExtend :: (Monad m, PrimMonad m) => Int -> WordArray -> Int -> m (MutableWordArray m)
cloneWordArrayExtend !oldLen !(WA !arr) !newLen = do
    !marr <- newByteArray (newLen * sizeOf (0 :: Word))
    if oldLen > 0
        then copyByteArray marr 0 arr 0 ((min oldLen newLen) * sizeOf (0 :: Word))
        else return ()
    setByteArray marr oldLen (max 0 (newLen - oldLen)) (0 :: Word)
    return $ MWA marr

{-# INLINE readWordArray #-}
readWordArray :: (Monad m, PrimMonad m) => MutableWordArray m -> Int -> m Word
readWordArray !(MWA !marr) i = readByteArray marr i

{-# INLINE unsafeFreezeWordArray #-}
unsafeFreezeWordArray :: (Monad m, PrimMonad m) => MutableWordArray m -> m WordArray
unsafeFreezeWordArray !(MWA !marr) = do
    !arr <- unsafeFreezeByteArray marr
    return (WA arr)

{-# INLINE indexWordArray #-}
indexWordArray :: WordArray -> Int -> Word
indexWordArray !(WA !arr) = indexByteArray arr

{-# INLINE indexWordArrayM #-}
indexWordArrayM :: Monad m => WordArray -> Int -> m Word
indexWordArrayM !(WA !arr) !i = case indexByteArray arr i of x -> return x

{-# INLINE writeWordArray #-}
writeWordArray :: (Monad m, PrimMonad m) => MutableWordArray m -> Int -> Word -> m ()
writeWordArray !(MWA !marr) = writeByteArray marr

{-# INLINE setWordArray #-}
setWordArray :: (Monad m, PrimMonad m) => MutableWordArray m -> Int -> Int -> Word -> m ()
setWordArray !marr !offset !count !word =
    loop offset (offset + count)
  where
    loop !off !end
        | off < end = do
            writeWordArray marr off word
            loop (off + 1) end
        | otherwise = return ()

{-# INLINE copyWordArray #-}
copyWordArray :: (Monad m, PrimMonad m) => MutableWordArray m -> Int -> WordArray -> Int -> Int -> m ()
copyWordArray !(MWA !marr) !doff !(WA !arr) !soff !wrds =
    let !wordsize = sizeOf (0 :: Word)
    in copyByteArray marr (doff * wordsize) arr (soff * wordsize) (wrds * wordsize)
