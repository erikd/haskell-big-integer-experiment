{-# LANGUAGE BangPatterns, CPP, MagicHash, NoImplicitPrelude, UnboxedTuples #-}

#include "MachDeps.h"

module New4.GHC.Integer.Prim
    ( plusWord, plusWord2, plusWord2C, plusWord3C
    , minusWord2, minusWord2C
    , timesWord2, timesWord2C, timesWord2CC
    , quotRemWord, quotRemWord2
    , shiftRWord
    , log2Word
    ) where

import GHC.Base (Word (..), Int (..))
import GHC.Types (isTrue#)
import GHC.Prim


{-# INLINE plusWord #-}
plusWord :: Word -> Word -> Word
plusWord !(W# a) !(W# b) =
    let !s = plusWord# a b
    in W# s

{-# INLINE plusWord2 #-}
plusWord2 :: Word -> Word -> (# Word, Word #)
plusWord2 !(W# a) !(W# b) =
    let (# !c, !s #) = plusWord2# a b
    in (# W# c, W# s #)

{-# INLINE plusWord2C #-}
plusWord2C :: Word -> Word -> Word -> (# Word, Word #)
plusWord2C !(W# a) !(W# b) !(W# c) =
    let (# !c1, !s1 #) = plusWord2# a b
        (# !c2, !s2 #) = plusWord2# s1 c
        !carry = plusWord# c1 c2
    in (# W# carry, W# s2 #)

{-# INLINE plusWord3C #-}
plusWord3C :: Word -> Word -> Word -> Word -> (# Word, Word #)
plusWord3C !(W# a) !(W# b) !(W# c) !(W# d) =
    let (# !c1, !s1 #) = plusWord2# a b
        (# !c2, !s2 #) = plusWord2# c d
        (# !c3, !s3 #) = plusWord2# s1 s2
        !c4 = plusWord# c1 c2
        !carry = plusWord# c3 c4
    in (# W# carry, W# s3 #)

{-# INLINE minusWord2 #-}
minusWord2 :: Word -> Word -> (# Word, Word #)
minusWord2 !(W# a) !(W# b) =
    let !diff = minusWord# a b
        -- TODO : Really need a minusWord2# PrimOp.
        !carry = if isTrue# (ltWord# a b) then 1## else 0##
    in (# W# carry, W# diff #)

{-# INLINE minusWord2C #-}
minusWord2C :: Word -> Word -> Word -> (# Word, Word #)
minusWord2C !(W# a) !(W# b) !(W# c) =
    let !diff = minusWord# a (plusWord# b c)
        !carry = if isTrue# (ltWord# a b) then 1## else 0##
    in (# W# carry, W# diff #)

{-# INLINE timesWord2 #-}
timesWord2 :: Word -> Word -> (# Word, Word #)
timesWord2 !(W# a) !(W# b) =
    let (# !ovf, !prod #) = timesWord2# a b
    in (# W# ovf, W# prod #)

{-# INLINE timesWord2C #-}
timesWord2C :: Word -> Word -> Word -> (# Word, Word #)
timesWord2C !(W# a) !(W# b) !(W# c) =
    let (# !ovf, !prod #) = timesWord2# a b
        (# !cry, !prodc #) = plusWord2# prod c
        !carry = plusWord# ovf cry
    in (# W# carry, W# prodc #)

{-# INLINE timesWord2CC #-}
timesWord2CC :: Word -> Word -> Word -> Word -> (# Word, Word #)
timesWord2CC !(W# a) !(W# b) !(W# c) !(W# d) =
    let (# !ovf, !prod #) = timesWord2# a b
        (# !c1, !sm #) = plusWord2# c d
        (# !cry, !prodc #) = plusWord2# prod sm
        !carry = plusWord# (plusWord# ovf cry) c1
    in (# W# carry, W# prodc #)

{-# INLINE shiftRWord #-}
shiftRWord :: Word -> Int -> Word
shiftRWord !(W# w) !(I# i) = W# (uncheckedShiftRL# w i)

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

{-# NOINLINE log2Word #-}
log2Word :: Word# -> Int#
log2Word w# =
    case or# w# (uncheckedShiftL# w# 1#) of
        w1# -> case or# w1# (uncheckedShiftL# w1# 2#) of
            w2# -> case or# w2# (uncheckedShiftL# w2# 4#) of
                w3# -> case or# w3# (uncheckedShiftL# w3# 8#) of
                    w4# -> case or# w4# (uncheckedShiftL# w4# 16#) of
                        w5# -> case or# w5# (uncheckedShiftL# w5# 32#) of
                            x# -> case word2Int# (and# x# (not# (uncheckedShiftL# x# 1#))) of
                                res# -> res#
