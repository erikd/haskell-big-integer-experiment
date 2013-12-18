{-# LANGUAGE BangPatterns, CPP, MagicHash, NoImplicitPrelude #-}

#include "MachDeps.h"

module New.GHC.Integer.Prim
    ( FullWord, HalfWord
    , plusHalfWord, minusHalfWord, timesHalfWord, timesHalfWordC
    , promoteHalfWord
    , splitFullWord, makeFullWord
    ) where

import GHC.Prim
import GHC.Word (
#if WORD_SIZE_IN_BITS == 64
	Word32 (..), Word (..)
#endif
#if WORD_SIZE_IN_BITS == 32
	Word16 (..), Word (..)
#endif
	)


#if WORD_SIZE_IN_BITS == 64

#define FW Word
#define HW Word32
#define FC W
#define HC W32
#define halfShift 32#
#define halfMask 0xffffffff#

#endif

#if WORD_SIZE_IN_BITS == 32

#define FW Word
#define HW Word16
#define FC W
#define HC W16
#define halfShift 16#
#define halfMask 0xffff#

#endif

type FullWord = FW  -- Word64 on 64 bit systems, otherwise Word32.
type HalfWord = HW  -- Word32 on 64 bit systems, otherwise Word16.


{-# INLINE plusHalfWord #-}
plusHalfWord :: HalfWord -> HalfWord -> (HalfWord, HalfWord)
plusHalfWord !a !b =
    let !(FC# fa) = promoteHalfWord a
        !(FC# fb) = promoteHalfWord b
        !prod = plusWord# fa fb
    in splitFullWord (FC# prod)

minusHalfWord :: HalfWord -> HalfWord -> (HalfWord, HalfWord)
minusHalfWord !a !b =
    let !(FC# fa) = promoteHalfWord a
        !(FC# fb) = promoteHalfWord b
        !prod = minusWord# fa fb
    in splitFullWord (FC# prod)

{-# INLINE timesHalfWord #-}
timesHalfWord :: HalfWord -> HalfWord -> (HalfWord, HalfWord)
timesHalfWord !a !b =
    let !(FC# fa) = promoteHalfWord a
        !(FC# fb) = promoteHalfWord b
        !prod = timesWord# fa fb
    in splitFullWord (FC# prod)


{-# INLINE timesHalfWordC #-}
timesHalfWordC :: HalfWord -> HalfWord -> HalfWord -> (HalfWord, HalfWord)
timesHalfWordC !a !b !c =
    let !(FC# fa) = promoteHalfWord a
        !(FC# fb) = promoteHalfWord b
        !(FC# fc) = promoteHalfWord c
        !prod = plusWord# (timesWord# fa fb) fc
    in splitFullWord (FC# prod)


{-# INLINE promoteHalfWord #-}
promoteHalfWord :: HalfWord -> FullWord
promoteHalfWord !(HC# x) = FC# (and# x halfMask#)


{-# INLINE splitFullWord #-}
splitFullWord :: FullWord -> (HalfWord, HalfWord)
splitFullWord !(FC# x) =
	(HC# (unsafeCoerce# (uncheckedShiftRL# x halfShift)), HC# (unsafeCoerce# x))


{-# INLINE makeFullWord #-}
makeFullWord :: (HalfWord, HalfWord) -> FullWord
makeFullWord (!(HC# a), !(HC# b)) =
	FC# (plusWord# (uncheckedShiftL# a halfShift) (and# b halfMask#))
