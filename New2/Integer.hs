{-# LANGUAGE NoImplicitPrelude, MagicHash #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module New2.Integer
    ( module New2.GHC.Integer
    , hexShow
    ) where

import Prelude hiding (Integer)
import Numeric

import New2.GHC.Integer
import New2.GHC.Integer.Array
import New2.GHC.Integer.Type


instance Num Integer where
    (+) = plusInteger
    (-) = minusInteger
    (*) = timesInteger
    abs = absInteger
    signum = signumInteger
    fromInteger = error "New2.Integer: fromInteger"


instance Show Integer where
    show = hexShow


hexShow :: Integer -> String
hexShow (Positive n) = hexShowNatural '+' n
hexShow (Negative n) = hexShowNatural '-' n

hexShowNatural :: Char -> Natural -> String
hexShowNatural _ (Small 0) = "0x0"
hexShowNatural sign (Small a) = sign : "0x" ++ showHex a ""
hexShowNatural sign (Large n arr) =
    if n == 1 && indexWordArray arr 0 == 0
        then "0x0"
        else sign : arrayShow n arr

