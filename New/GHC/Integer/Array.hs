{-# LANGUAGE BangPatterns #-}
module New.GHC.Integer.Array where

import Control.Monad.Primitive
import Data.Primitive

import New.GHC.Integer.Prim


newtype MutableWordArray m = MWA (MutableByteArray (PrimState m))
newtype MutableHalfWordArray m = MHWA (MutableByteArray (PrimState m))


newWordArray :: PrimMonad m => Int -> m (MutableWordArray m)
newWordArray !len = do
    marr <- newByteArray (len * sizeOf (0 :: FullWord))
    return $ MWA marr

newHalfWordArray :: PrimMonad m => Int -> m (MutableHalfWordArray m)
newHalfWordArray !len = do
    marr <- newByteArray (len * sizeOf (0 :: HalfWord))
    return $ MHWA marr

newHalfWordArrayCleared :: PrimMonad m => Int -> m (MutableHalfWordArray m)
newHalfWordArrayCleared !len = do
    marr <- newByteArray (len * sizeOf (0 :: HalfWord))
    setByteArray marr 0 len (0 :: HalfWord)
    return $ MHWA marr


cloneHalfWordArrayExtend :: PrimMonad m=> Int -> ByteArray -> Int -> m (MutableHalfWordArray m)
cloneHalfWordArrayExtend !oldLen !arr !newLen = do
    marr <- newByteArray (newLen * sizeOf (0 :: HalfWord))
    copyByteArray marr 0 arr 0 ((min oldLen newLen) * sizeOf (0 :: HalfWord))
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
