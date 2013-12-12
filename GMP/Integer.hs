{-# LANGUAGE NoImplicitPrelude, MagicHash #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- *All* of this code was ripped from the GHC sources.
-- We need this because the Integer we are compiling and testing is *not* the
-- same as the Integer GHC already knows about.

module GMP.Integer
    ( module GMP.GHC.Integer
    ) where

import GMP.GHC.Integer

import GHC.Base

import Prelude hiding (Integer)


instance Num Integer where
    (+) = plusInteger
    (-) = minusInteger
    (*) = minusInteger
    abs = absInteger
    signum = signumInteger
    fromInteger = error "GMP.Integer: fromInteger"


instance Show Integer where
    show = hexShow


hexShow :: Integer -> String
hexShow i
    | eqInteger i (smallInteger 0#) = "0x0"
    | otherwise = signHex i : '0' : 'x' : reverse (inner (absInteger i))
  where
    signHex x = if (ltInteger x (smallInteger 0#)) then '-' else '+'
    inner x
        | eqInteger x (smallInteger 0#) = []
        | otherwise =
            let nibble = boxedIntFromInteger (andInteger x (smallInteger 15#))
            in ("0123456789abcdef" !! nibble) : inner (quotInteger x (smallInteger 16#))


boxedIntFromInteger :: Integer -> Int
boxedIntFromInteger i = I# (integerToInt i)
