{-# LANGUAGE BangPatterns, CPP, MagicHash, NoImplicitPrelude #-}

#include "MachDeps.h"

module New2.GHC.Integer.Prim
    ( plusWord2, plusWord2C
    , minusWord2, minusWord2C
    , timesWord2, timesWord2C, timesWord2CC
    , shiftRWord
    ) where

import GHC.Prim
import GHC.Base (Word (..), Int (..))


{-# INLINE plusWord2 #-}
plusWord2 :: Word -> Word -> (# Word, Word #)
plusWord2 !(W# a) !(W# b) =
    let (# c, s #) = plusWord2# a b
    in (# W# c, W# s #)

{-# INLINE plusWord2C #-}
plusWord2C :: Word -> Word -> Word -> (# Word, Word #)
plusWord2C !(W# a) !(W# b) !(W# c) =
    let (# c1, s1 #) = plusWord2# a b
        (# c2, s2 #) = plusWord2# s1 c
        !carry = plusWord# c1 c2
    in (# W# carry, W# s2 #)

{-# INLINE minusWord2 #-}
minusWord2 :: Word -> Word -> (Word, Word)
minusWord2 !(W# a) !(W# b) =
    let !diff = minusWord# a b
        -- TODO : Really need a minusWord2# PrimOp.
        !carry = if ltWord# a b then 1## else 0##
    in (W# carry, W# diff)

{-# INLINE minusWord2C #-}
minusWord2C :: Word -> Word -> Word -> (Word, Word)
minusWord2C !(W# a) !(W# b) !(W# c) =
    let !diff = minusWord# a (plusWord# b c)
        !carry = if ltWord# a b then 1## else 0##
    in (W# carry, W# diff)

{-# INLINE timesWord2 #-}
timesWord2 :: Word -> Word -> (Word, Word)
timesWord2 !(W# a) !(W# b) =
    let (# ovf, prod #) = timesWord2# a b
    in (W# ovf, W# prod)

{-# INLINE timesWord2C #-}
timesWord2C :: Word -> Word -> Word -> (Word, Word)
timesWord2C !(W# a) !(W# b) !(W# c) =
    let (# ovf, prod #) = timesWord2# a b
        (# cry, prodc #) = plusWord2# prod c
        carry = plusWord# ovf cry
    in (W# carry, W# prodc)

{-# INLINE timesWord2CC #-}
timesWord2CC :: Word -> Word -> Word -> Word -> (Word, Word)
timesWord2CC !(W# a) !(W# b) !(W# c) !(W# d) =
    let (# ovf, prod #) = timesWord2# a b
        (# c1, sm #) = plusWord2# c d
        (# cry, prodc #) = plusWord2# prod sm
        carry = plusWord# (plusWord# ovf cry) c1
    in (W# carry, W# prodc)

{-# INLINE shiftRWord #-}
shiftRWord :: Word -> Int -> Word
shiftRWord !(W# w) !(I# i) = W# (uncheckedShiftRL# w i)
