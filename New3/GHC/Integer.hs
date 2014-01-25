{-# LANGUAGE CPP,  NoImplicitPrelude #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  New3.GHC.Integer
-- Copyright   :  (c) Erik de Castro Lopo
-- License     :  BSD3
--
-- Maintainer  :  <erikd@mega-nerd.com>
-- Stability   :  internal
-- Portability :  non-portable (GHC Extensions)
--
-- An simple definition of the 'Integer' type.
--
-----------------------------------------------------------------------------

#include "MachDeps.h"

module New3.GHC.Integer (
    Integer, mkInteger,
    smallInteger, wordToInteger, integerToWord, integerToInt,
#if WORD_SIZE_IN_BITS < 64
    integerToWord64, word64ToInteger,
    integerToInt64, int64ToInteger,
#endif
    plusInteger, minusInteger, timesInteger, negateInteger,
    eqInteger, neqInteger, absInteger, signumInteger,
    leInteger, gtInteger, ltInteger, geInteger, compareInteger,
    divModInteger, quotRemInteger, quotInteger, remInteger,
    encodeFloatInteger, decodeFloatInteger, floatFromInteger,
    encodeDoubleInteger, decodeDoubleInteger, doubleFromInteger,
    -- gcdInteger, lcmInteger, -- XXX
    andInteger, orInteger, xorInteger, complementInteger,
    shiftLInteger, shiftRInteger,
    hashInteger,

#if TESTING
    isMinimal
#endif
    ) where

import New3.GHC.Integer.Internals
import New3.GHC.Integer.Type
