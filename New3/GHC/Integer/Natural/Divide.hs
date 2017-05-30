{-# LANGUAGE CPP, MagicHash, ForeignFunctionInterface, NoImplicitPrelude,
             BangPatterns, Strict, UnboxedTuples, UnliftedFFITypes #-}

#include "MachDeps.h"

module New3.GHC.Integer.Natural.Divide
    ( quotRemNatural
    , quotRemNaturalW
    ) where

import Prelude hiding (Integer, abs, pi, sum, rem, succ) -- (all, error, otherwise, return, show, (++))

import Common.GHC.Integer.Prim
import Common.GHC.Integer.StrictPrim
import New3.GHC.Integer.Type
import New3.GHC.Integer.Natural
import New3.GHC.Integer.Natural.Multiply
import Common.GHC.Integer.WordArray

#if !defined(__HADDOCK__)

quotRemNaturalW :: Natural -> Word -> (Natural, Word)
quotRemNaturalW !(Natural !n !arr) !w = runStrictPrim $ do
    qlen <- pure $! if w >= indexWordArray arr (n - 1) then n - 1 else n
    qmarr <- newWordArray qlen
    rem <- loop (n - 1) qmarr 0
    qarr <- unsafeFreezeWordArray qmarr
    pure $! (Natural qlen qarr, rem)
  where
    loop i qmarr rem
        | i >= 0 = do
            x <- indexWordArrayM arr i
            let (# q, r #) = quotRemWord2 rem x w
            writeWordArray qmarr i q
            loop (i - 1) qmarr r
        | otherwise = pure rem

{-
https://www.khanacademy.org/math/arithmetic/multiplication-division/partial_quotient_division/v/partial-quotient-method-of-division-2
http://en.wikipedia.org/wiki/Division_algorithm
http://en.wikipedia.org/wiki/Barrett_reduction
http://en.wikipedia.org/wiki/Euclidean_division
-}


quotRemNatural :: Natural -> Natural -> (Natural, Natural)
quotRemNatural !numer@(Natural !nn !narr) !denom@(Natural !dn !darr)
    | dn > nn = (zeroNatural, numer)
    | dn == nn && indexWordArray narr (nn - 1) < indexWordArray darr (nn - 1) = (zeroNatural, numer)
    | dn == nn && indexWordArray narr (nn - 1) == indexWordArray darr (nn - 1) =
                    if ltNatural numer denom
                        then (zeroNatural, numer)
                        else (oneNatural, minusNatural numer denom)
#if 0
    | otherwise = error ("New3/GHC/Integer/Natural.hs: line " ++ show (__LINE__ :: Int))
#else
    | otherwise = divideOnceNatural numer denom

{-
      runStrictPrim $ do
        qlen <- pure $! if indexWordArray darr (dn - 1) >= indexWordArray narr (nn - 1) then n - 1 else n
        qmarr <- newWordArray n
        rmarr <- newWordArray n
        rem <- loop (n - 1) qmarr 0
        qarr <- unsafeFreezeWordArray qmarr
        pure $! (Natural qlen qarr, rem)

divideSimple :: Natural -> Natural -> (Natural, Natural)
divideSimple numer denom =
    let nd# = encodeDoubleNatural numer 0#
        dd# = encodeDoubleNatural denom 0#
        q = decodeDoubleNatural (nd# /## dd#)
    in (q, minusNatural numer (timesNatural q denom))

-}


#endif

divideOnceNatural :: Natural -> Natural -> (Natural, Natural)
divideOnceNatural numer denom =
    let !(q1, r1) = partialQuotient numer
    in case q1 of
        Natural 0 _ -> (q1, r1)
        _ ->
            let !(!q2, !r2) = divideOnceNatural r1 denom
            in (plusNatural q1 q2, r2)
  where
    {-# INLINE partialQuotient #-}
    partialQuotient :: Natural -> (Natural, Natural)
    partialQuotient num =
        let (m, s) = partialQuotientWord num denom
        in case m of
            0 -> (zeroNatural, num)
            _ ->
                let !qt = shiftLNatural (wordToNatural m) (64 * s)
                    !pr = timesNatural denom qt
                    !rm  = minusNatural num pr
                in (qt, rm)

    {-# INLINE partialQuotientWord #-}
    partialQuotientWord :: Natural -> Natural -> (Word, Int)
    partialQuotientWord !(Natural !nn !narr) !(Natural !dn !darr) =
        let !n0 = indexWordArray narr (nn - 1)
            !d0 = indexWordArray darr (dn - 1)
        in case (# compare dn nn, compare n0 d0 #) of
            (# GT, _ #) -> (0, 0)
            (# EQ, LT #) -> (0, 0)
            (# EQ, EQ #) -> (1, nn - dn)
            (# EQ, GT #) -> case quotRemWord n0 d0 of (# !q, _ #) -> (q, nn - dn)
            (# LT, LT #) -> case quotRemWord2 n0 (indexWordArray narr $ nn - 2) d0 of (# !q, _ #) -> (q, nn - dn - 1)
            (# LT, _ #) -> case quotRemWord n0 d0 of (# !q, _ #) -> (q, nn - dn)

#endif
