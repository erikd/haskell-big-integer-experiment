{-# LANGUAGE NoImplicitPrelude, MagicHash #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- *All* of this code was ripped from the GHC sources.
-- We need this because the Integer we are compiling and testing is *not* the
-- same as the Integer GHC already knows about.

module New3.Integer
    ( module New3.GHC.Integer
    , hexShow
    ) where

import Prelude hiding (Integer)
import Numeric

import New3.GHC.Integer
import New3.GHC.Integer.Array
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


hexShow :: Integer -> String
hexShow Zero = "0x0"
hexShow (SmallPos a) = "+0x" ++ showHex a ""
hexShow (SmallNeg a) = "-0x" ++ showHex a ""
hexShow (Positive n) = hexShowNatural '+' n
hexShow (Negative n) = hexShowNatural '-' n

hexShowNatural :: Char -> Natural -> String
hexShowNatural sign (Natural n arr) =
    if n == 1 && indexWordArray arr 0 == 0
        then "0x0"
        else sign : arrayShow n arr

