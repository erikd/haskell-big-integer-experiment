{-# LANGUAGE BangPatterns, Strict #-}

import Control.Monad.Primitive
import Data.Primitive

import qualified Criterion.Main as C

import Common.GHC.Integer.WordArray

main :: IO ()
main = do
    let len = 1000000
    src <- unsafeFreezeWordArray =<< newWordArray len
    dest1 <- newWordArray len
    dest2 <- newWordArray len
    C.defaultMain
        [ C.bgroup "copyWordArray"
            [ C.bench "Library"         $ C.whnfIO $ benchLibrary len src dest1
            , C.bench "Explicit"        $ C.whnfIO $ benchExplicit len src dest2
            , C.bench "ExplicitAddr"    $ C.whnfIO $ benchExplicitAddr len src dest2
            ]
        ]

{-# NOINLINE benchLibrary #-}
benchLibrary :: Int -> WordArray -> MutableWordArray IO -> IO WordArray
benchLibrary len src dest = do
    copyWordArrayLibrary dest 0 src 0 len
    unsafeFreezeWordArray dest

{-# NOINLINE benchExplicit #-}
benchExplicit :: Int -> WordArray -> MutableWordArray IO -> IO WordArray
benchExplicit len src dest = do
    copyWordArrayExplicit dest 0 src 0 len
    unsafeFreezeWordArray dest

{-# NOINLINE benchExplicitAddr #-}
benchExplicitAddr :: Int -> WordArray -> MutableWordArray IO -> IO WordArray
benchExplicitAddr len src dest = do
    copyWordArrayExplicitAddr dest 0 src 0 len
    unsafeFreezeWordArray dest

{-# INLINE copyWordArrayLibrary #-}
copyWordArrayLibrary :: PrimMonad m => MutableWordArray m -> Int -> WordArray -> Int -> Int -> m ()
copyWordArrayLibrary (MWA !marr) !doff (WA !arr) !soff !wrds =
    let !wordsize = sizeOf (0 :: Word)
    in copyByteArray marr (doff * wordsize) arr (soff * wordsize) (wrds * wordsize)

{-# INLINE copyWordArrayExplicit #-}
copyWordArrayExplicit :: PrimMonad m => MutableWordArray m -> Int -> WordArray -> Int -> Int -> m ()
copyWordArrayExplicit !marr !doff !arr !soff !wrds =
    loop 0
  where
    loop !i
        | i < wrds =  do
            !x <- indexWordArrayM arr (soff + i)
            writeWordArray marr (doff + i) x
            loop (i + 1)
        | otherwise = return ()

{-# INLINE copyWordArrayExplicitAddr #-}
copyWordArrayExplicitAddr :: PrimMonad m => MutableWordArray m -> Int -> WordArray -> Int -> Int -> m ()
copyWordArrayExplicitAddr !marr !doff !arr !soff !wrds =
    loop 0
  where
    addr = wordArrayContents arr
    loop !i
        | i < wrds =  do
            !x <- indexWordAddrM addr (soff + i)
            writeWordArray marr (doff + i) x
            loop (i + 1)
        | otherwise = return ()
