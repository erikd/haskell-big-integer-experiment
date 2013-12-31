{-# LANGUAGE BangPatterns #-}
module New.GHC.Integer.Array where

import Control.Monad.Primitive
import Data.Primitive

import New.GHC.Integer.Prim


newtype MutableWordArray m = MWA (MutableByteArray (PrimState m))
newtype MutableHalfWordArray m = MHWA (MutableByteArray (PrimState m))


newWordArray :: PrimMonad m => Int -> m (MutableWordArray m)
newWordArray !len = do
    !marr <- newByteArray (len * sizeOf (0 :: FullWord))
    return $ MWA marr

newHalfWordArray :: PrimMonad m => Int -> m (MutableHalfWordArray m)
newHalfWordArray !len = do
    !marr <- newByteArray (len * sizeOf (0 :: HalfWord))
    return $ MHWA marr

newWordArrayCleared :: PrimMonad m => Int -> m (MutableWordArray m)
newWordArrayCleared !len = do
    !marr <- newByteArray (len * sizeOf (0 :: FullWord))
    setByteArray marr 0 len (0 :: FullWord)
    return $ MWA marr

newHalfWordArrayCleared :: PrimMonad m => Int -> m (MutableHalfWordArray m)
newHalfWordArrayCleared !len = do
    !marr <- newByteArray (len * sizeOf (0 :: HalfWord))
    setByteArray marr 0 len (0 :: HalfWord)
    return $ MHWA marr

-- | newPlaceholderWordArray : Create a place holder ByteArray for timesInteger
-- where a zero length ByteArray is needed. Memory is actually allocated, but
-- nothing is written to it os it will actually contain junk data.
newPlaceholderWordArray :: PrimMonad m => m ByteArray
newPlaceholderWordArray = do
    !marr <- newByteArray (sizeOf (0 :: FullWord))
    unsafeFreezeByteArray marr

cloneWordArrayExtend :: PrimMonad m=> Int -> ByteArray -> Int -> m (MutableWordArray m)
cloneWordArrayExtend !oldLen !arr !newLen = do
    !marr <- newByteArray (newLen * sizeOf (0 :: FullWord))
    if oldLen > 0
        then copyByteArray marr 0 arr 0 ((min oldLen newLen) * sizeOf (0 :: FullWord))
        else return ()
    setByteArray marr oldLen (max 0 (newLen - oldLen)) (0 :: FullWord)
    return $ MWA marr

cloneHalfWordArrayExtend :: PrimMonad m=> Int -> ByteArray -> Int -> m (MutableHalfWordArray m)
cloneHalfWordArrayExtend !oldLen !arr !newLen = do
    !marr <- newByteArray (newLen * sizeOf (0 :: HalfWord))
    if oldLen > 0
        then copyByteArray marr 0 arr 0 ((min oldLen newLen) * sizeOf (0 :: HalfWord))
        else return ()
    setByteArray marr oldLen (max 0 (newLen - oldLen)) (0 :: HalfWord)
    return $ MHWA marr

unsafeFreezeWordArray :: PrimMonad m => MutableWordArray m -> m ByteArray
unsafeFreezeWordArray !(MWA marr) = unsafeFreezeByteArray marr

unsafeFreezeHalfWordArray :: PrimMonad m => MutableHalfWordArray m -> m ByteArray
unsafeFreezeHalfWordArray !(MHWA marr) = unsafeFreezeByteArray marr

indexWordArray :: ByteArray -> Int -> FullWord
indexWordArray = indexByteArray

indexHalfWordArray :: ByteArray -> Int -> HalfWord
indexHalfWordArray = indexByteArray

indexWordArrayM :: Monad m => ByteArray -> Int -> m FullWord
indexWordArrayM !arr !i = return $ indexByteArray arr i

indexHalfWordArrayM :: Monad m => ByteArray -> Int -> m HalfWord
indexHalfWordArrayM !arr !i = return $ indexByteArray arr i

writeWordArray :: PrimMonad m => MutableWordArray m -> Int -> FullWord -> m ()
writeWordArray !(MWA marr) = writeByteArray marr

writeHalfWordArray :: PrimMonad m => MutableHalfWordArray m -> Int -> HalfWord -> m ()
writeHalfWordArray !(MHWA marr) = writeByteArray marr

setWordArray :: PrimMonad m => MutableWordArray m -> Int -> Int -> FullWord -> m ()
setWordArray !(MWA marr) !off !count !word = setByteArray marr off count word

copyWordArray :: PrimMonad m => MutableWordArray m -> Int -> ByteArray -> Int -> Int -> m ()
copyWordArray !(MWA marr) !doff !arr !soff !wrds =
    let !wordsize = sizeOf (0 :: FullWord)
    in copyByteArray marr (doff * wordsize) arr (soff * wordsize) (wrds * wordsize)
