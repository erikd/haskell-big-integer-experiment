{-# LANGUAGE BangPatterns, NoImplicitPrelude, MagicHash #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module New4.Integer
    ( module New4.GHC.Integer
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
import New4.GHC.Integer
import New4.GHC.Integer.Internals
import New4.GHC.Integer.Type


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
    show = hexShowNatural


hexShow :: Integer -> String
hexShow (Positive (NatS 0)) = "0x0"
hexShow (Positive n) = '+' : hexShowNatural n
hexShow (Negative n) = '-' : hexShowNatural n

hexShowNatural :: Natural -> String
hexShowNatural (NatB n arr) = arrayShow n arr
hexShowNatural (NatS w) = "0x" ++ showHex w ""

readInteger :: String -> Integer
readInteger [] = 0
readInteger ('-':xs) = -1 * readInteger xs
readInteger ('+':xs) = readInteger xs
readInteger ('0':'x':xs) = readIntegerHex xs
readInteger s =
    foldl' (\acc c -> acc * (smallInteger 10#) + readChar c) (smallInteger 0#) s
  where
    readChar :: Char -> Integer
    readChar c =
        let !(I# i) = ord c - 48
        in smallInteger i

readIntegerHex :: String -> Integer
readIntegerHex s =
    foldl' (\acc c -> acc * (smallInteger 16#) + readChar (ord c)) (smallInteger 0#) s
  where
    readChar :: Int -> Integer
    readChar c
        | c >= 0x30 && c <= 0x39 = smallInteger (unboxInt (c - 0x30))
        | c >= 0x41 && c <= 0x46 = smallInteger (unboxInt (10 + c - 0x41))
        | otherwise = smallInteger (unboxInt (10 + c - 0x61))
