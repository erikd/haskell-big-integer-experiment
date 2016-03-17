{-# LANGUAGE NoImplicitPrelude #-}

module New4.GHC.Integer.Type where

import GHC.Types


import New4.GHC.Integer.WordArray

data Integer
    = Positive !Natural
    | Negative !Natural

data Natural
    = NatS {-# UNPACK #-} !Word
    | NatB
        {-# UNPACK #-} !Int
        {-# UNPACK #-} !WordArray
