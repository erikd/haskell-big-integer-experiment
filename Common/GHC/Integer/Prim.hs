{-# LANGUAGE BangPatterns, CPP, ForeignFunctionInterface, MagicHash,
        NoImplicitPrelude, UnboxedTuples, UnliftedFFITypes #-}

#include "MachDeps.h"

module Common.GHC.Integer.Prim
    ( boxInt#, boxWord#, unboxInt, unboxWord
    , boxDouble#, unboxDouble
    , plusWord, plusWord2, plusWord2C, plusWord3C
    , minusWord2, minusWord2C
    , timesWord2, timesWord2C, timesWord2CC
    , quotRemWord, quotRemWord2
    , shiftLWord2#, shiftRWord
    , wordSizeInBits, wordSizeInBytes, highestSetBit

    , encodeDouble#
    ) where

import GHC.Base (Int (..), Word (..))
import GHC.Float (Double (..))
import GHC.Types (isTrue#)
import GHC.Prim

-- Many of these primitives are defined in compiler/prelude/primops.txt.pp
-- from the GHC source tree (both the Git tree and the distribution tarball.

{-# INLINE boxInt# #-}
boxInt# :: Int# -> Int
boxInt# = I#

{-# INLINE boxWord# #-}
boxWord# :: Word# -> Word
boxWord# = W#

{-# INLINE boxDouble# #-}
boxDouble# :: Double# -> Double
boxDouble# = D#


{-# INLINE unboxInt #-}
unboxInt :: Int -> Int#
unboxInt (I# !i#) = i#

{-# INLINE unboxWord #-}
unboxWord :: Word -> Word#
unboxWord (W# !w#) = w#

{-# INLINE unboxDouble #-}
unboxDouble :: Double -> Double#
unboxDouble (D# !d#) = d#


{-# INLINE plusWord #-}
plusWord :: Word -> Word -> Word
plusWord (W# a) (W# b) =
    let !s = plusWord# a b
    in W# s

{-# INLINE plusWord2 #-}
plusWord2 :: Word -> Word -> (# Word, Word #)
plusWord2 (W# a) (W# b) =
    let (# !c, !s #) = plusWord2# a b
    in (# W# c, W# s #)

{-# INLINE plusWord2C #-}
plusWord2C :: Word -> Word -> Word -> (# Word, Word #)
plusWord2C (W# a) (W# b) (W# c) =
    let (# !c1, !s1 #) = plusWord2# a b
        (# !c2, !s2 #) = plusWord2# s1 c
        !carry = plusWord# c1 c2
    in (# W# carry, W# s2 #)

{-# INLINE plusWord3C #-}
plusWord3C :: Word -> Word -> Word -> Word -> (# Word, Word #)
plusWord3C (W# a) (W# b) (W# c) (W# d) =
    let (# !c1, !s1 #) = plusWord2# a b
        (# !c2, !s2 #) = plusWord2# c d
        (# !c3, !s3 #) = plusWord2# s1 s2
        !c4 = plusWord# c1 c2
        !carry = plusWord# c3 c4
    in (# W# carry, W# s3 #)

{-# INLINE minusWord2 #-}
minusWord2 :: Word -> Word -> (# Word, Word #)
minusWord2 (W# a) (W# b) =
    let !diff = minusWord# a b
        -- TODO : Really need a minusWord2# PrimOp.
        !borrow = if isTrue# (ltWord# a b) then 1## else 0##
    in (# W# borrow, W# diff #)

{-# INLINE minusWord2C #-}
minusWord2C :: Word -> Word -> Word -> (# Word, Word #)
minusWord2C (W# a) (W# b) (W# c) =
    let (# W# c1, W# sum #) = plusWord2 (W# b) (W# c)
        !diff = minusWord# a sum
        !carry = if isTrue# (ltWord# a sum) then plusWord# c1 1## else c1
    in (# W# carry, W# diff #)

{-# INLINE timesWord2 #-}
timesWord2 :: Word -> Word -> (# Word, Word #)
timesWord2 (W# a) (W# b) =
    let (# !ovf, !prod #) = timesWord2# a b
    in (# W# ovf, W# prod #)

{-# INLINE timesWord2C #-}
timesWord2C :: Word -> Word -> Word -> (# Word, Word #)
timesWord2C (W# a) (W# b) (W# c) =
    let (# !ovf, !prod #) = timesWord2# a b
        (# !cry, !prodc #) = plusWord2# prod c
        !carry = plusWord# ovf cry
    in (# W# carry, W# prodc #)

{-# INLINE timesWord2CC #-}
timesWord2CC :: Word -> Word -> Word -> Word -> (# Word, Word #)
timesWord2CC (W# a) (W# b) (W# c) (W# d) =
    let (# !ovf, !prod #) = timesWord2# a b
        (# !c1, !sm #) = plusWord2# c d
        (# !cry, !prodc #) = plusWord2# prod sm
        !carry = plusWord# (plusWord# ovf cry) c1
    in (# W# carry, W# prodc #)

{-# INLINE shiftLWord2# #-}
-- Assumes shift amount is >= 0 && < WORD_SIZE_IN_BITS.
shiftLWord2# :: Word# -> Word# -> (# Word#, Word# #)
shiftLWord2# w s =
    let i# = word2Int# s
    in (# uncheckedShiftRL# w (WORD_SIZE_IN_BITS# -# i#), uncheckedShiftL# w i# #)

{-# INLINE shiftRWord #-}
shiftRWord :: Word -> Int -> Word
shiftRWord (W# w) (I# i) = W# (uncheckedShiftRL# w i)

{-# INLINE quotRemWord #-}
quotRemWord :: Word -> Word -> (# Word, Word #)
quotRemWord (W# x) (W# y) =
    let (# q, r #) = quotRemWord# x y
    in (# W# q, W# r #)

{-# INLINE quotRemWord2 #-}
quotRemWord2 :: Word -> Word -> Word -> (# Word, Word #)
quotRemWord2 (W# xhi) (W# xlo) (W# y) =
    let (# q, r #) = quotRemWord2# xhi xlo y
    in (# W# q, W# r #)


{-# INLINE wordSizeInBits #-}
wordSizeInBits :: Int
wordSizeInBits = I# WORD_SIZE_IN_BITS#

{-# INLINE wordSizeInBytes #-}
wordSizeInBytes :: Int
wordSizeInBytes = I# (word2Int# (uncheckedShiftRL# WORD_SIZE_IN_BITS## 3#))

{-# INLINE highestSetBit #-}
highestSetBit :: Word -> Int
highestSetBit (W# w) = I# (word2Int# (minusWord# WORD_SIZE_IN_BITS## (clz# w)))


foreign import ccall unsafe "__word_encodeDouble"
    encodeDouble# :: Word# -> Int# -> Double#
