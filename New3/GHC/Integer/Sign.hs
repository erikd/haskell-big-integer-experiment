{-# LANGUAGE BangPatterns, NoImplicitPrelude #-}

module New3.GHC.Integer.Sign where

import GHC.Classes
import GHC.Types

data Sign
    = Pos | Neg
    deriving Eq

instance Ord Sign where
    compare Pos Neg = GT
    compare Neg Pos = LT
    compare _   _   = EQ
    (<) Pos Neg = False
    (<) Neg Pos = True
    (<) _   _   = False

    (>) Pos Neg = True
    (>) Neg Pos = False
    (>) _   _   = False

    (<=) Pos Neg = False
    (<=) Neg Pos = True
    (<=) _   _   = True

    (>=) Pos Neg = True
    (>=) Neg Pos = False
    (>=) _   _   = True

    max a b = if a >= b then a else b
    min a b = if a <= b then a else b

negateSign :: Sign -> Sign
negateSign !Pos = Neg
negateSign !Neg = Pos

timesSign :: Sign -> Sign -> Sign
timesSign !a !b = if a == b then Pos else Neg

signOfInt :: Int -> Sign
signOfInt !x
    | x < 0 = Neg
    | True = Pos
