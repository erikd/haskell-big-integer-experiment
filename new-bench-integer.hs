{-# LANGUAGE CPP, MagicHash #-}

import Prelude hiding (Integer)

import Control.Applicative ((<$>))
import GHC.Base

import qualified Criterion.Main as C
import qualified System.Random as R


import qualified GMP.Integer as G
import qualified New.Integer as N
import qualified Simple.Integer as S

main :: IO ()
main = do
    -- Generate all the data needed for testing.
    gmpSmallList <- mkSmallIntegerList 1000 (\x -> G.smallInteger (unboxInt x))
    smpSmallList <- mkSmallIntegerList 1000 (\x -> S.smallInteger (unboxInt x))
    newSmallList <- mkSmallIntegerList 1000 (\x -> N.smallInteger (unboxInt x))

    gmpLargeList <- mkLargeIntegerList 200 (20, 100) G.mkInteger
    smpLargeList <- mkLargeIntegerList 200 (20, 100) S.mkInteger
    newLargeList <- mkLargeIntegerList 200 (20, 100) N.mkInteger

    gmpHugeList <- mkLargeIntegerList 10 (200, 250) G.mkInteger
    smpHugeList <- mkLargeIntegerList 10 (200, 250) S.mkInteger
    newHugeList <- mkLargeIntegerList 10 (200, 250) N.mkInteger

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
                ( "Sum " ++ show (length gmpSmallList)
                    ++ " small Integers (positive and negative, so sum likely to stay small)"
                )
            [ C.bench "GMP"     $ C.whnf (foldl1 G.plusInteger) gmpSmallList
            , C.bench "Simple"  $ C.whnf (foldl1 S.plusInteger) smpSmallList
            , C.bench "New"     $ C.whnf (foldl1 N.plusInteger) newSmallList
            ]
        , C.bgroup
                ( "Sum " ++ show (length gmpSmallPosList)
                    ++ " small positive Integers (only positive, so sum likely to grow to large Integer)"
                )
            [ C.bench "GMP"     $ C.whnf (foldl1 G.plusInteger) gmpSmallPosList
            , C.bench "Simple"  $ C.whnf (foldl1 S.plusInteger) smpSmallPosList
            , C.bench "New"     $ C.whnf (foldl1 N.plusInteger) newSmallPosList
            ]
        , C.bgroup
                ( "Sum " ++ show (length gmpLargePosList) ++ " large positive Integers"
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
                    ++ " huge (~2500 decimal digit) Integers."
                )
            [ C.bench "GMP"     $ C.whnf (foldl1 G.timesInteger) gmpHugeList
            , C.bench "Simple"  $ C.whnf (foldl1 S.timesInteger) smpHugeList
            , C.bench "New"     $ C.whnf (foldl1 N.timesInteger) newHugeList
            ]
        ]

--------------------------------------------------------------------------------
-- Data generators.

mkSmallIntegerList :: Int -> (Int -> a) -> IO [a]
mkSmallIntegerList count constructor =
    fmap (map constructor . take count . R.randoms) R.newStdGen


mkLargeIntegerList :: Int -> (Int, Int) -> (Bool -> [Int] -> a) -> IO [a]
mkLargeIntegerList count range constructor = do
    signs <- (take count . R.randoms) <$> R.newStdGen
    lengths <- (take count . R.randomRs range) <$> R.newStdGen
    let mkIntList len =
            (take len . R.randomRs (0, 0x7fffffff)) <$> R.newStdGen
    ints <- mapM mkIntList lengths
    return . take count $ zipWith constructor signs ints

--------------------------------------------------------------------------------
-- Helpers.

unboxInt :: Int -> Int#
unboxInt (I# i) = i
