{-# LANGUAGE BangPatterns, NoImplicitPrelude, MagicHash #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module New3.Integer
    ( module New3.GHC.Integer
    , hexShow
    , hexShowNatural
    , readInteger
    ) where

import Prelude hiding (Integer)
import Data.Char (ord)
import Data.List (foldl')
import GHC.Types
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
    fromInteger = readInteger . show


instance Show Integer where
    show = hexShow

instance Show Natural where
    show (Natural n arr) = arrayShow n arr

hexShow :: Integer -> String
hexShow (SmallPos 0##) = "0x0"
hexShow (SmallPos a) = "+0x" ++ showHex (boxWord# a) ""
hexShow (SmallNeg a) = "-0x" ++ showHex (boxWord# a) ""
hexShow (Positive n arr) = '+' : hexShowNatural (Natural n arr)
hexShow (Negative n arr) = '-' : hexShowNatural (Natural n arr)

hexShowNatural :: Natural -> String
hexShowNatural = show

readInteger :: String -> Integer
readInteger [] = 0
readInteger ('-':xs) = -1 * readInteger xs
readInteger ('+':xs) = readInteger xs
readInteger ('0':'x':xs) = readIntegerHex xs
readInteger s =
    foldl' (\ !acc c -> acc * ten + readChar c) (smallInteger 0#) s
  where
    ten = smallInteger 10#
    readChar :: Char -> Integer
    readChar c =
        let !(I# i) = ord c - 48
        in smallInteger i

readIntegerHex :: String -> Integer
readIntegerHex =
    foldl' (\ !acc c -> acc * smallInteger 16# + readChar (ord c)) (smallInteger 0#)
  where
    readChar :: Int -> Integer
    readChar c
        | c >= 0x30 && c <= 0x39 = smallInteger (unboxInt (c - 0x30))
        | c >= 0x41 && c <= 0x46 = smallInteger (unboxInt (10 + c - 0x41))
        | otherwise = smallInteger (unboxInt (10 + c - 0x61))
