{-# LANGUAGE NoImplicitPrelude, MagicHash #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module New3.Integer
    ( module New3.GHC.Integer
    , hexShow
    , hexShowNatural
    ) where

import Prelude hiding (Integer)
import Numeric

import Common.GHC.Integer.Prim
import New3.GHC.Integer
import New3.GHC.Integer.Internals
import New3.GHC.Integer.Type


instance Num Integer where
    (+) = plusInteger
    (-) = minusInteger
    (*) = timesInteger
    abs = absInteger
    signum = signumInteger
    fromInteger = error "New3.Integer: fromInteger"


instance Show Integer where
    show = hexShow

instance Show Natural where
    show = hexShowNatural


hexShow :: Integer -> String
hexShow (SmallPos 0##) = "0x0"
hexShow (SmallPos a) = "+0x" ++ showHex (boxWord# a) ""
hexShow (SmallNeg a) = "-0x" ++ showHex (boxWord# a) ""
hexShow (Positive n arr) = '+' : hexShowNatural (Natural n arr)
hexShow (Negative n arr) = '-' : hexShowNatural (Natural n arr)

hexShowNatural :: Natural -> String
hexShowNatural (Natural n arr) = arrayShow n arr

