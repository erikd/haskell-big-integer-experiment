{-# LANGUAGE CPP, NoImplicitPrelude, MagicHash, UnboxedTuples #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- *All* of this code ripped from GHC sources.
-- We need this because the Integer we are compiling and testing is *not* the
-- same as the Integer GHC already knows about.

module Support where

-- import GHC.Base (Char (..), Int (..), ord, quotRemInt, unsafeChr)
import GHC.Base
import GHC.Integer

import qualified Prelude as P

-- Use the 32 bit versions here regardless.
-- #define DIGITS       9
-- #define BASE         (smallInteger 1000000000#)

#include "MachDeps.h"
#if SIZEOF_HSWORD == 4
#define DIGITS       9
#define BASE         (smallInteger 1000000000#)
#elif SIZEOF_HSWORD == 8
#define DIGITS       18
#define BASE         (smallInteger 1000000000000000000#)
#else
#error Please define DIGITS and BASE
-- DIGITS should be the largest integer such that
--     10^DIGITS < 2^(SIZEOF_HSWORD * 8 - 1)
-- BASE should be 10^DIGITS. Note that ^ is not available yet.
#endif


instance P.Show Integer where
	show i = integerToString i ""


integerToString :: Integer -> String -> String
integerToString n0 cs0
    | n0 < (smallInteger 0#)    = '-' : integerToString' (negateInteger n0) cs0
    | otherwise = integerToString' n0 cs0
    where
    integerToString' :: Integer -> String -> String
    integerToString' n cs
        | n < BASE  = jhead (boxedIntFromInteger n) cs
        | otherwise = jprinth (jsplitf (BASE `timesInteger` BASE) n) cs

    -- Split n into digits in base p. We first split n into digits
    -- in base p*p and then split each of these digits into two.
    -- Note that the first 'digit' modulo p*p may have a leading zero
    -- in base p that we need to drop - this is what jsplith takes care of.
    -- jsplitb the handles the remaining digits.
    jsplitf :: Integer -> Integer -> [Integer]
    jsplitf p n
        | p > n     = [n]
        | otherwise = jsplith p (jsplitf (p `timesInteger` p) n)

    jsplith :: Integer -> [Integer] -> [Integer]
    jsplith p (n:ns) =
        case n `quotRemInteger` p of
        (# q, r #) ->
            if q > (smallInteger 0#) then q : r : jsplitb p ns
                     else     r : jsplitb p ns
    jsplith _ [] = error "jsplith: []"

    jsplitb :: Integer -> [Integer] -> [Integer]
    jsplitb _ []     = []
    jsplitb p (n:ns) = case n `quotRemInteger` p of
                       (# q, r #) ->
                           q : r : jsplitb p ns

    -- Convert a number that has been split into digits in base BASE^2
    -- this includes a last splitting step and then conversion of digits
    -- that all fit into a machine word.
    jprinth :: [Integer] -> String -> String
    jprinth (n:ns) cs =
        case n `quotRemInteger` BASE of
        (# q', r' #) ->
            let q = boxedIntFromInteger q'
                r = boxedIntFromInteger r'
            in if q > 0 then jhead q $ jblock r $ jprintb ns cs
                        else jhead r $ jprintb ns cs
    jprinth [] _ = error "jprinth []"

    jprintb :: [Integer] -> String -> String
    jprintb []     cs = cs
    jprintb (n:ns) cs = case n `quotRemInteger` BASE of
                        (# q', r' #) ->
                            let q = boxedIntFromInteger q'
                                r = boxedIntFromInteger r'
                            in jblock q $ jblock r $ jprintb ns cs

    -- Convert an integer that fits into a machine word. Again, we have two
    -- functions, one that drops leading zeros (jhead) and one that doesn't
    -- (jblock)
    jhead :: Int -> String -> String
    jhead n cs
        | n < 10    = case unsafeChr (ord '0' P.+ n) of
            c@(C# _) -> c : cs
        | otherwise = case unsafeChr (ord '0' P.+ r) of
            c@(C# _) -> jhead q (c : cs)
        where
        (q, r) = n `quotRemInt` 10

    jblock = jblock' {- ' -} DIGITS

    jblock' :: Int -> Int -> String -> String
    jblock' d n cs
        | d == 1    = case unsafeChr (ord '0' P.+ n) of
             c@(C# _) -> c : cs
        | otherwise = case unsafeChr (ord '0' P.+ r) of
             c@(C# _) -> jblock' (d P.- 1) q (c : cs)
        where
        (q, r) = n `quotRemInt` 10


boxedIntFromInteger :: Integer -> Int
boxedIntFromInteger i = I# (integerToInt i)
