{-# LANGUAGE CPP, MagicHash, ForeignFunctionInterface, NoImplicitPrelude,
             BangPatterns, UnboxedTuples, UnliftedFFITypes #-}


#include "MachDeps.h"

module New3.GHC.Integer.Type where

import GHC.Prim
import GHC.Types


import Common.GHC.Integer.WordArray

data Integer
    = SmallPos Word#
    | SmallNeg Word#
    | Positive
            {-# UNPACK #-} !Int
            {-# UNPACK #-} !WordArray
    | Negative
            {-# UNPACK #-} !Int
            {-# UNPACK #-} !WordArray

data Natural
    = Natural
            {-# UNPACK #-} !Int
            {-# UNPACK #-} !WordArray
