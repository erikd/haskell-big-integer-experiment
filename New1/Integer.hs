{-# LANGUAGE NoImplicitPrelude, MagicHash #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module New1.Integer
    ( module New1.GHC.Integer
    , hexShow
    ) where

import Prelude hiding (Integer)
import Numeric

import New1.GHC.Integer
import New1.GHC.Integer.Array
import New1.GHC.Integer.Sign
import New1.GHC.Integer.Type


instance Num Integer where
    (+) = plusInteger
    (-) = minusInteger
    (*) = timessInteger
    abs = absInteger
    signum = signumInteger
    fromInteger = error "New1.Integer: fromInteger"


instance Show Integer where
    show = hexShow


hexShow :: Integer -> String
hexShow (Small _ 0) = "0x0"
hexShow (Small s a) =
    let sign = if s == Neg then '-' else '+'
    in sign : "0x" ++ showHex a ""

hexShow (Large s n arr)
    | n == 1 && indexWordArray arr 0 == 0 = "0x0"
    | otherwise =
        let sign = if s == Neg then '-' else '+'
            number = arrayShow n arr
        in if number == "0x0"
            then number
            else sign : number

