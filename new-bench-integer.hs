{-# LANGUAGE CPP, MagicHash #-}

import Prelude hiding (Integer)

import qualified Criterion.Main as C

import qualified GMP.Integer as G
import qualified New.Integer as N
import qualified Simple.Integer as S

import TestHelpers

main :: IO ()
main = do
    -- Generate all the data needed for testing.
    gmpSmallList <- mkSmallIntegerList 1000 (\x -> G.smallInteger (unboxInt x))
    smpSmallList <- mkSmallIntegerList 1000 (\x -> S.smallInteger (unboxInt x))
    newSmallList <- mkSmallIntegerList 1000 (\x -> N.smallInteger (unboxInt x))

    largeList <- mkLargeIntegerList 200 (50, 60)
    let gmpLargeList = map (\(b, l) -> G.mkInteger b l) largeList
        smpLargeList = map (\(b, l) -> S.mkInteger b l) largeList
        newLargeList = map (\(b, l) -> N.mkInteger b l) largeList

    hugeList <- mkLargeIntegerList 10 (200, 250)
    let gmpHugeList = map (\(b, l) -> G.mkInteger b l) hugeList
        -- smpHugeList = map (\(b, l) -> S.mkInteger b l) hugeList
        newHugeList = map (\(b, l) -> N.mkInteger b l) hugeList

    let (gmpSmallPosList, smpSmallPosList, newSmallPosList) =
                ( map G.absInteger gmpSmallList
                , map S.absInteger smpSmallList
                , map N.absInteger newSmallList
                )

    let (gmpLargePosList, smpLargePosList, newLargePosList) =
                ( map G.absInteger gmpLargeList
                , map S.absInteger smpLargeList
                , map N.absInteger newLargeList
                )
    let (gmpFirstTen, smpFirstTen, newFirstTen) =
                ( map (\x -> G.smallInteger (unboxInt x)) [1..10]
                , map (\x -> S.smallInteger (unboxInt x)) [1..10]
                , map (\x -> N.smallInteger (unboxInt x)) [1..10]
                )
    let (gmpFirstHundred, smpFirstHundred, newFirstHundred) =
                ( map (\x -> G.smallInteger (unboxInt x)) [1..100]
                , map (\x -> S.smallInteger (unboxInt x)) [1..100]
                , map (\x -> N.smallInteger (unboxInt x)) [1..100]
                )

    -- Run the benchmarks.
    C.defaultMain
        [ C.bgroup
                ( "Sum of " ++ show (length gmpSmallList)
                    ++ " small Integers (positive and negative, so sum likely to stay small)"
                )
            [ C.bench "GMP"     $ C.whnf (foldl1 G.plusInteger) gmpSmallList
            , C.bench "Simple"  $ C.whnf (foldl1 S.plusInteger) smpSmallList
            , C.bench "New"     $ C.whnf (foldl1 N.plusInteger) newSmallList
            ]
        , C.bgroup
                ( "Sum of " ++ show (length gmpSmallPosList)
                    ++ " small positive Integers (only positive, so sum likely to grow to large Integer)"
                )
            [ C.bench "GMP"     $ C.whnf (foldl1 G.plusInteger) gmpSmallPosList
            , C.bench "Simple"  $ C.whnf (foldl1 S.plusInteger) smpSmallPosList
            , C.bench "New"     $ C.whnf (foldl1 N.plusInteger) newSmallPosList
            ]
        , C.bgroup
                ( "Sum of " ++ show (length gmpLargePosList) ++ " large (~600 decimal digit) positive Integers"
                )
            [ C.bench "GMP"     $ C.whnf (foldl1 G.plusInteger) gmpLargePosList
            , C.bench "Simple"  $ C.whnf (foldl1 S.plusInteger) smpLargePosList
            , C.bench "New"     $ C.whnf (foldl1 N.plusInteger) newLargePosList
            ]
        , C.bgroup "Product of [1..10] (final result fits in a 64 bit Word)"
            [ C.bench "GMP"     $ C.whnf (foldl1 G.timesInteger) gmpFirstTen
            , C.bench "Simple"  $ C.whnf (foldl1 S.timesInteger) smpFirstTen
            , C.bench "New"     $ C.whnf (foldl1 N.timesInteger) newFirstTen
            ]
        , C.bgroup "Product of [1..100] (final result is >> a 64 bit Word)"
            [ C.bench "GMP"     $ C.whnf (foldl1 G.timesInteger) gmpFirstHundred
            , C.bench "Simple"  $ C.whnf (foldl1 S.timesInteger) smpFirstHundred
            , C.bench "New"     $ C.whnf (foldl1 N.timesInteger) newFirstHundred
            ]
        , C.bgroup
                ( "Product of " ++ show (length gmpHugeList)
                    ++ " huge (~2500 decimal digit) Integers"
                )
            [ C.bench "GMP"     $ C.whnf (foldl1 G.timesInteger) gmpHugeList
            -- , C.bench "Simple"  $ C.whnf (foldl1 S.timesInteger) smpHugeList
            , C.bench "New"     $ C.whnf (foldl1 N.timesInteger) newHugeList
            ]
        ]

