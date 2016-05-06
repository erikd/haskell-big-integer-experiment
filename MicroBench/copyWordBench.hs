{-# LANGUAGE BangPatterns #-}

import Control.Monad.Primitive
import Data.Primitive

import qualified Criterion.Main as C

import Common.GHC.Integer.WordArray

main :: IO ()
main = do
    let len = 1000000
    src <- unsafeFreezeWordArray =<< newWordArrayCleared len
    dest1 <- newWordArray len
    dest2 <- newWordArray len
    C.defaultMain
        [ C.bgroup "copyWordArray"
            [ C.bench "Library"     $ C.whnfIO $ benchLibrary len src dest1
            , C.bench "Explicit"    $ C.whnfIO $ benchExplicit len src dest2
            ]
        ]

benchLibrary :: Int -> WordArray -> MutableWordArray IO -> IO WordArray
benchLibrary len src dest = do
    copyWordArrayLibrary dest 0 src 0 len
    unsafeFreezeWordArray dest


benchExplicit :: Int -> WordArray -> MutableWordArray IO -> IO WordArray
benchExplicit len src dest = do
    copyWordArrayExplicit dest 0 src 0 len
    unsafeFreezeWordArray dest


{-# INLINE copyWordArrayLibrary #-}
copyWordArrayLibrary :: PrimMonad m => MutableWordArray m -> Int -> WordArray -> Int -> Int -> m ()
copyWordArrayLibrary (MWA !marr) !doff (WA !arr) !soff !wrds =
    let !wordsize = sizeOf (0 :: Word)
    in copyByteArray marr (doff * wordsize) arr (soff * wordsize) (wrds * wordsize)

{-# INLINE copyWordArrayExplicit #-}
copyWordArrayExplicit :: PrimMonad m => MutableWordArray m -> Int -> WordArray -> Int -> Int -> m ()
copyWordArrayExplicit !marr !doff !arr !soff !wrds =
    let loop !i
            | i < wrds =  do
                !x <- indexWordArrayM arr (soff + i)
                writeWordArray marr (doff + i) x
                loop (i + 1)
            | otherwise = return ()
    in loop 0

