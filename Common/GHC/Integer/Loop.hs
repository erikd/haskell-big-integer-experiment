{-# LANGUAGE BangPatterns, CPP, MagicHash #-}

{-

The ideas for these loop functions were stolen from Niklas Hambüchen's loop
package:

    http://hackage.haskell.org/package/loop

The function intLoop is pretty much just like the upstream while intLoopState
adds a state value that is threaded through the computation. A more general
version of this function was contributed back upstream:

    https://github.com/nh2/loop/pull/2

-}

module Common.GHC.Integer.Loop
    ( intLoop
    , intLoopState
    ) where

import Common.GHC.Integer.StrictPrim

-- | @intLoop start end f@: Loops over a contiguous numerical range, including
-- @end@.
--
-- It uses @(+ 1)@ so for most integer types it has no bounds (overflow) check.
{-# INLINE intLoop #-}
intLoop :: Int -> Int -> (Int -> StrictPrim s ()) -> StrictPrim s ()
intLoop start end f =
    go start
  where
    go !x
        | x == end  = f x
        | otherwise = do
                f x
                go (x + 1)


{-# INLINE intLoopState #-}
intLoopState :: Int -> Int -> b -> (Int -> b -> StrictPrim s b) -> StrictPrim s b
intLoopState start end initState f =
    go start initState
  where
    go !x !s
        | x == end = return s
        | otherwise = do
                !ns <- f x s
                go (x + 1) ns
