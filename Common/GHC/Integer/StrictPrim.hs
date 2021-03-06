{-
A Strict State Monad

We want a monad we can run our Data.Primitive operations inside. Data.Primitive
provides IO and ST instances, but we want strict evaluation.

We take Carter Schonwald's StrictIdentity monad idea [0] and cross it with the
State monad from Base to get StrictPrim.

[0] http://hackage.haskell.org/package/strict-identity
-}

{-# LANGUAGE BangPatterns, MagicHash, NoImplicitPrelude, RankNTypes,
    TypeFamilies, UnboxedTuples, UnliftedFFITypes #-}

module Common.GHC.Integer.StrictPrim
    ( StrictPrim (..)
    , runStrictPrim
    ) where

import GHC.Base hiding (($!)) -- Want to use the local definition of ($!) regardless.

import Control.Monad.Primitive

newtype StrictPrim a
    = StrictPrim (State# RealWorld -> (# State# RealWorld, a #))

instance Applicative StrictPrim where
    {-# INLINE pure #-}
    pure !x = StrictPrim ( \ !s -> (# s, x #))

    {-# INLINE (<*>) #-}
    (<*>) !a !b = do !f <- a ; !v <- b ; pure $! (f $! v)

instance Functor StrictPrim where
    {-# INLINE fmap #-}
    fmap !f (StrictPrim !m) = StrictPrim $ \ !s ->
        case m s of
            (# !new_s,!r #) -> (# new_s, f $! r #)


instance Monad StrictPrim where
    {-# INLINE (>>) #-}
    (!m) >> (!k) = do { _ <- m ;  k }

    {-# INLINE (>>=) #-}
    (StrictPrim !m) >>= (!k) =
        StrictPrim ( \ !s ->
            case m s of
                (# !new_s, !r #) -> case k r of
                    StrictPrim !k2 -> k2 new_s
            )

instance PrimMonad StrictPrim where
    type PrimState StrictPrim = RealWorld
    {-# INLINE primitive #-}
    primitive = StrictPrim


{-# INLINE runStrictPrim #-}
runStrictPrim :: StrictPrim a -> a
runStrictPrim !st =
    case st of
        StrictPrim !st_rep ->
            case st_rep realWorld# of
                (# _, !r #) -> r

{-# INLINE ($!) #-}
($!) :: (a -> b) -> a -> b
($!) !f !x =
    let !ey = f x
    in ey
