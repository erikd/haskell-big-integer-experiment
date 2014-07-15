{-
A Strict State Monad

We want a monad we can run our Data.Primitive operations inside. Data.Primitive
provides IO and ST instances, but we want strict evaluation.

We take Carter Schonwald's StrictIdentity monad idea [0] and cross it with the
State monad from Base to get StrictPrim.

[0] http://hackage.haskell.org/package/strict-identity
-}

{-# LANGUAGE BangPatterns, CPP, MagicHash, NoImplicitPrelude, RankNTypes,
    TypeFamilies, UnboxedTuples #-}

module Common.GHC.Integer.StrictPrim
    ( StrictPrim
    , runStrictPrim
    ) where

import GHC.Base
import Control.Applicative
import Control.Monad.Primitive


newtype StrictPrim s a
    = StrictPrim (State# s -> (# State# s, a #))

instance Applicative (StrictPrim s) where
    {-# INLINE pure #-}
    pure = return

    {-# INLINE (<*>) #-}
    (<*>) a b = do f <- a ; v <- b ; return $! (f $! v)

instance Functor (StrictPrim s) where
    {-# INLINE fmap #-}
    fmap !f !(StrictPrim !m) = StrictPrim $ \ !s ->
        case m s of
            (# !new_s,!r #) -> (# new_s, f $! r #)


instance Monad (StrictPrim s) where
    {-# INLINE return #-}
    return !x = StrictPrim $! \ !s -> (# s, x #)

    {-# INLINE (>>) #-}
    (!m) >> (!k) = m >>= \ _ -> k

    {-# INLINE (>>=) #-}
    (!(StrictPrim !m)) >>= (!k) =
        StrictPrim $! \ !s ->
            case m s of
                (# new_s, r #) -> case k r of
                    StrictPrim k2 -> (k2 new_s)

instance PrimMonad (StrictPrim s) where
    type PrimState (StrictPrim s) = s
    {-# INLINE primitive #-}
    primitive = StrictPrim
    {-# INLINE internal #-}
    internal (!(StrictPrim !p)) = p


{-# INLINE runStrictPrim #-}
runStrictPrim :: (forall s. StrictPrim s a) -> a
runStrictPrim !st =
    case st of
        StrictPrim st_rep ->
            case st_rep realWorld# of
                (# _, !r #) -> r


-- Grab this from Prelude (part of Base) because Base depends on this code.
($!) :: (a -> b) -> a -> b
f $! x  = let !vx = x in f vx
