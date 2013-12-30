{-# LANGUAGE BangPatterns, CPP, MagicHash, NoImplicitPrelude #-}

#include "MachDeps.h"

module New.GHC.Integer.Prim
    ( FullWord, HalfWord
    , plusHalfWord, plusHalfWordC
    , minusHalfWord, minusHalfWordC
    , timesHalfWord, timesHalfWordC, timesHalfWordCC
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
#define halfMask 0xffffffff##

#elif WORD_SIZE_IN_BITS == 32

#define FW Word
#define HW Word16
#define FC W
#define HC W16
#define halfShift 16#
#define halfMask 0xffff##

#else

#error "Expecting WORD_SIZE_IN_BITS to be 32 or 64."

#endif

type FullWord = FW  -- Word64 on 64 bit systems.
type HalfWord = HW  -- Word32 on 32 bit systems.


{-# INLINE plusHalfWord #-}
plusHalfWord :: HalfWord -> HalfWord -> (HalfWord, HalfWord)
plusHalfWord !a !b =
    let !(FC# fa) = promoteHalfWord a
        !(FC# fb) = promoteHalfWord b
        !sum = plusWord# fa fb
    in splitFullWord (FC# sum)

{-# INLINE plusHalfWordC #-}
plusHalfWordC :: HalfWord -> HalfWord -> HalfWord -> (HalfWord, HalfWord)
plusHalfWordC !a !b !c =
    let !(FC# fa) = promoteHalfWord a
        !(FC# fb) = promoteHalfWord b
        !(FC# fc) = promoteHalfWord c
        !sum = plusWord# (plusWord# fa fc) fb
    in splitFullWord (FC# sum)

{-# INLINE minusHalfWord #-}
minusHalfWord :: HalfWord -> HalfWord -> (HalfWord, HalfWord)
minusHalfWord !a !b =
    let !(FC# fa) = promoteHalfWord a
        !(FC# fb) = promoteHalfWord b
        !diff = minusWord# fa fb
        !((HC# hc), hd) = splitFullWord (FC# diff)
    in (HC# (and# hc (unsafeCoerce# 1#)), hd)

{-# INLINE minusHalfWordC #-}
minusHalfWordC :: HalfWord -> HalfWord -> HalfWord -> (HalfWord, HalfWord)
minusHalfWordC !a !b !c =
    let !(FC# fa) = promoteHalfWord a
        !(FC# fb) = promoteHalfWord b
        !(FC# fc) = promoteHalfWord c
        !diff = minusWord# fa (plusWord# fb fc)
        !((HC# hc), hd) = splitFullWord (FC# diff)
    in (HC# (and# hc (unsafeCoerce# 1#)), hd)

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

{-# INLINE timesHalfWordCC #-}
timesHalfWordCC :: HalfWord -> HalfWord -> HalfWord -> HalfWord -> (HalfWord, HalfWord)
timesHalfWordCC !a !b !c !d =
    let !(FC# fa) = promoteHalfWord a
        !(FC# fb) = promoteHalfWord b
        !(FC# fc) = promoteHalfWord c
        !(FC# fd) = promoteHalfWord d
        !prod = plusWord# (plusWord# (timesWord# fa fb) fc) fd
    in splitFullWord (FC# prod)

{-# INLINE promoteHalfWord #-}
promoteHalfWord :: HalfWord -> FullWord
promoteHalfWord !(HC# x) = FC# (and# x halfMask)


{-# INLINE splitFullWord #-}
splitFullWord :: FullWord -> (HalfWord, HalfWord)
splitFullWord !(FC# x) =
    (HC# (unsafeCoerce# (uncheckedShiftRL# x halfShift)), HC# (unsafeCoerce# (and# x halfMask)))


{-# INLINE makeFullWord #-}
makeFullWord :: (HalfWord, HalfWord) -> FullWord
makeFullWord (!(HC# a), !(HC# b)) =
    FC# (plusWord# (uncheckedShiftL# a halfShift) (and# b halfMask))
