{-# LANGUAGE NoImplicitPrelude, MagicHash #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module New3.Integer
    ( module New3.GHC.Integer
    , hexShow
    , hexShowNatural
    ) where

import Prelude hiding (Integer)
import Numeric

import New3.GHC.Integer
import New3.GHC.Integer.Type


instance Num Integer where
    (+) = plusInteger
    (-) = minusInteger
    (*) = minusInteger
    abs = absInteger
    signum = signumInteger
    fromInteger = error "New3.Integer: fromInteger"


instance Show Integer where
    show = hexShow

instance Show Natural where
    show = hexShowNatural


hexShow :: Integer -> String
hexShow Zero = "0x0"
hexShow (SmallPos a) = "+0x" ++ showHex a ""
hexShow (SmallNeg a) = "-0x" ++ showHex a ""
hexShow (Positive n) = '+' : hexShowNatural n
hexShow (Negative n) = '-' : hexShowNatural n

hexShowNatural :: Natural -> String
hexShowNatural (Natural n arr) = arrayShow n arr

