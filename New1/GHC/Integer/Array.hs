{-# LANGUAGE BangPatterns #-}
module New1.GHC.Integer.Array where

import Control.Monad.Primitive
import Data.Primitive
import GHC.Word (Word)


newtype MutableWordArray m = MWA (MutableByteArray (PrimState m))


newWordArray :: PrimMonad m => Int -> m (MutableWordArray m)
newWordArray !len = do
    !marr <- newByteArray (len * sizeOf (0 :: Word))
    return $ MWA marr

newWordArrayCleared :: PrimMonad m => Int -> m (MutableWordArray m)
newWordArrayCleared !len = do
    !marr <- newByteArray (len * sizeOf (0 :: Word))
    setByteArray marr 0 len (0 :: Word)
    return $ MWA marr

-- | newPlaceholderWordArray : Create a place holder ByteArray for timesInteger
-- where a zero length ByteArray is needed. Memory is actually allocated, but
-- nothing is written to it os it will actually contain junk data.
newPlaceholderWordArray :: PrimMonad m => m ByteArray
newPlaceholderWordArray = do
    !marr <- newByteArray (sizeOf (0 :: Word))
    unsafeFreezeByteArray marr

cloneWordArrayExtend :: PrimMonad m=> Int -> ByteArray -> Int -> m (MutableWordArray m)
cloneWordArrayExtend !oldLen !arr !newLen = do
    !marr <- newByteArray (newLen * sizeOf (0 :: Word))
    if oldLen > 0
        then copyByteArray marr 0 arr 0 ((min oldLen newLen) * sizeOf (0 :: Word))
        else return ()
    setByteArray marr oldLen (max 0 (newLen - oldLen)) (0 :: Word)
    return $ MWA marr

unsafeFreezeWordArray :: PrimMonad m => MutableWordArray m -> m ByteArray
unsafeFreezeWordArray !(MWA marr) = unsafeFreezeByteArray marr

indexWordArray :: ByteArray -> Int -> Word
indexWordArray = indexByteArray

indexWordArrayM :: Monad m => ByteArray -> Int -> m Word
indexWordArrayM !arr !i = return $ indexByteArray arr i

writeWordArray :: PrimMonad m => MutableWordArray m -> Int -> Word -> m ()
writeWordArray !(MWA marr) = writeByteArray marr

setWordArray :: PrimMonad m => MutableWordArray m -> Int -> Int -> Word -> m ()
setWordArray !(MWA marr) !off !count !word = setByteArray marr off count word

copyWordArray :: PrimMonad m => MutableWordArray m -> Int -> ByteArray -> Int -> Int -> m ()
copyWordArray !(MWA marr) !doff !arr !soff !wrds =
    let !wordsize = sizeOf (0 :: Word)
    in copyByteArray marr (doff * wordsize) arr (soff * wordsize) (wrds * wordsize)
