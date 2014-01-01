{-# LANGUAGE CPP, MagicHash #-}

import Prelude hiding (Integer)

import qualified Criterion.Main as C

import qualified GMP.Integer as G
import qualified New1.Integer as N1
import qualified New2.Integer as N2
import qualified New3.Integer as N3
import qualified Simple.Integer as S

import Check.Helpers

main :: IO ()
main = do
    -- Generate all the data needed for testing.
    gmpSmallList <- mkSmallIntegerList 1000 (\x -> G.smallInteger (unboxInt x))
    smpSmallList <- mkSmallIntegerList 1000 (\x -> S.smallInteger (unboxInt x))
    new1SmallList <- mkSmallIntegerList 1000 (\x -> N1.smallInteger (unboxInt x))
    new2SmallList <- mkSmallIntegerList 1000 (\x -> N2.smallInteger (unboxInt x))
    new3SmallList <- mkSmallIntegerList 1000 (\x -> N3.smallInteger (unboxInt x))

    largeList <- mkLargeIntegerList 200 (50, 60)
    let gmpLargeList = map (\(b, l) -> G.mkInteger b l) largeList
        smpLargeList = map (\(b, l) -> S.mkInteger b l) largeList
        new1LargeList = map (\(b, l) -> N1.mkInteger b l) largeList
        new2LargeList = map (\(b, l) -> N2.mkInteger b l) largeList
        new3LargeList = map (\(b, l) -> N3.mkInteger b l) largeList

    hugeList <- mkLargeIntegerList 10 (200, 250)
    let gmpHugeList = map (\(b, l) -> G.mkInteger b l) hugeList
        -- smpHugeList = map (\(b, l) -> S.mkInteger b l) hugeList
        new1HugeList = map (\(b, l) -> N1.mkInteger b l) hugeList
        new2HugeList = map (\(b, l) -> N2.mkInteger b l) hugeList
        new3HugeList = map (\(b, l) -> N3.mkInteger b l) hugeList

    let (gmpSmallPosList, smpSmallPosList, new1SmallPosList, new2SmallPosList, new3SmallPosList) =
                ( map G.absInteger gmpSmallList
                , map S.absInteger smpSmallList
                , map N1.absInteger new1SmallList
                , map N2.absInteger new2SmallList
                , map N3.absInteger new3SmallList
                )

    let (gmpLargePosList, smpLargePosList, new1LargePosList, new2LargePosList, new3LargePosList) =
                ( map G.absInteger gmpLargeList
                , map S.absInteger smpLargeList
                , map N1.absInteger new1LargeList
                , map N2.absInteger new2LargeList
                , map N3.absInteger new3LargeList
                )
    let (gmpFirstTen, smpFirstTen, new1FirstTen, new2FirstTen, new3FirstTen) =
                ( map (\x -> G.smallInteger (unboxInt x)) [1..10]
                , map (\x -> S.smallInteger (unboxInt x)) [1..10]
                , map (\x -> N1.smallInteger (unboxInt x)) [1..10]
                , map (\x -> N2.smallInteger (unboxInt x)) [1..10]
                , map (\x -> N3.smallInteger (unboxInt x)) [1..10]
                )
    let (gmpFirstHundred, smpFirstHundred, new1FirstHundred, new2FirstHundred, new3FirstHundred) =
                ( map (\x -> G.smallInteger (unboxInt x)) [1..100]
                , map (\x -> S.smallInteger (unboxInt x)) [1..100]
                , map (\x -> N1.smallInteger (unboxInt x)) [1..100]
                , map (\x -> N2.smallInteger (unboxInt x)) [1..100]
                , map (\x -> N3.smallInteger (unboxInt x)) [1..100]
                )

    -- Run the benchmarks.
    C.defaultMain
        [ C.bgroup
                ( "Sum of " ++ show (length gmpSmallList)
                    ++ " small Integers (positive and negative, so sum likely to stay small)"
                )
            [ C.bench "GMP"     $ C.whnf (foldl1 G.plusInteger) gmpSmallList
            , C.bench "Simple"  $ C.whnf (foldl1 S.plusInteger) smpSmallList
            , C.bench "New1"    $ C.whnf (foldl1 N1.plusInteger) new1SmallList
            , C.bench "New2"    $ C.whnf (foldl1 N2.plusInteger) new2SmallList
            , C.bench "New3"    $ C.whnf (foldl1 N3.plusInteger) new3SmallList
            ]
        , C.bgroup
                ( "Sum of " ++ show (length gmpSmallPosList)
                    ++ " small positive Integers (only positive, so sum likely to grow to large Integer)"
                )
            [ C.bench "GMP"     $ C.whnf (foldl1 G.plusInteger) gmpSmallPosList
            , C.bench "Simple"  $ C.whnf (foldl1 S.plusInteger) smpSmallPosList
            , C.bench "New1"    $ C.whnf (foldl1 N1.plusInteger) new1SmallPosList
            , C.bench "New2"    $ C.whnf (foldl1 N2.plusInteger) new2SmallPosList
            , C.bench "New3"    $ C.whnf (foldl1 N3.plusInteger) new3SmallPosList
            ]
        , C.bgroup
                ( "Sum of " ++ show (length gmpLargePosList) ++ " large (~600 decimal digit) positive Integers"
                )
            [ C.bench "GMP"     $ C.whnf (foldl1 G.plusInteger) gmpLargePosList
            , C.bench "Simple"  $ C.whnf (foldl1 S.plusInteger) smpLargePosList
            , C.bench "New1"    $ C.whnf (foldl1 N1.plusInteger) new1LargePosList
            , C.bench "New2"    $ C.whnf (foldl1 N2.plusInteger) new2LargePosList
            , C.bench "New3"    $ C.whnf (foldl1 N3.plusInteger) new3LargePosList
            ]
        , C.bgroup "Product of [1..10] (final result fits in a 64 bit Word)"
            [ C.bench "GMP"     $ C.whnf (foldl1 G.timesInteger) gmpFirstTen
            , C.bench "Simple"  $ C.whnf (foldl1 S.timesInteger) smpFirstTen
            , C.bench "New1"    $ C.whnf (foldl1 N1.timesInteger) new1FirstTen
            , C.bench "New2"    $ C.whnf (foldl1 N2.timesInteger) new2FirstTen
            , C.bench "New3"    $ C.whnf (foldl1 N3.timesInteger) new3FirstTen
            ]
        , C.bgroup "Product of [1..100] (final result is >> a 64 bit Word)"
            [ C.bench "GMP"     $ C.whnf (foldl1 G.timesInteger) gmpFirstHundred
            , C.bench "Simple"  $ C.whnf (foldl1 S.timesInteger) smpFirstHundred
            , C.bench "New1"    $ C.whnf (foldl1 N1.timesInteger) new1FirstHundred
            , C.bench "New2"    $ C.whnf (foldl1 N2.timesInteger) new2FirstHundred
            , C.bench "New3"    $ C.whnf (foldl1 N3.timesInteger) new3FirstHundred
            ]
        , C.bgroup
                ( "Product of " ++ show (length gmpHugeList)
                    ++ " huge (~2500 decimal digit) Integers (drop Simple)"
                )
            [ C.bench "GMP"     $ C.whnf (foldl1 G.timesInteger) gmpHugeList
            -- , C.bench "Simple"  $ C.whnf (foldl1 S.timesInteger) smpHugeList
            , C.bench "New1"     $ C.whnf (foldl1 N1.timesInteger) new1HugeList
            , C.bench "New2"     $ C.whnf (foldl1 N2.timesInteger) new2HugeList
            , C.bench "New3"     $ C.whnf (foldl1 N3.timesInteger) new3HugeList
            ]
        ]

