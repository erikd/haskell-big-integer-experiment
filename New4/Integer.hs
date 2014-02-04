{-# LANGUAGE NoImplicitPrelude, MagicHash #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module New4.Integer
    ( module New4.GHC.Integer
    , hexShow
    , hexShowNatural
    ) where

import Prelude hiding (Integer)
import Numeric

import New4.GHC.Integer
import New4.GHC.Integer.Internals
import New4.GHC.Integer.Type


instance Num Integer where
    (+) = plusInteger
    (-) = minusInteger
    (*) = minusInteger
    abs = absInteger
    signum = signumInteger
    fromInteger = error "New4.Integer: fromInteger"


instance Show Integer where
    show = hexShow

instance Show Natural where
    show = hexShowNatural


hexShow :: Integer -> String
hexShow (SmallPos 0) = "0x0"
hexShow (SmallPos a) = "+0x" ++ showHex a ""
hexShow (SmallNeg a) = "-0x" ++ showHex a ""
hexShow (Positive n) = '+' : hexShowNatural n
hexShow (Negative n) = '-' : hexShowNatural n

hexShowNatural :: Natural -> String
hexShowNatural (Natural n arr) = arrayShow n arr

