{-# LANGUAGE CPP, MagicHash, ForeignFunctionInterface, NoImplicitPrelude,
             BangPatterns, UnboxedTuples, UnliftedFFITypes #-}


#include "MachDeps.h"

module New3.GHC.Integer.Type where

import GHC.Types
#if WORD_SIZE_IN_BITS < 64
import GHC.IntWord64
#endif


import New3.GHC.Integer.WordArray

data Integer
    = SmallPos {-# UNPACK #-} !Word
    | SmallNeg {-# UNPACK #-} !Word
    | Positive !Natural
    | Negative !Natural

data Natural
    = Natural
        {-# UNPACK #-} !Int
        {-# UNPACK #-} !WordArray
