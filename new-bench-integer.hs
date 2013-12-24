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

    gmpLargeList <- mkLargeIntegerList 100 G.mkInteger
    smpLargeList <- mkLargeIntegerList 100 S.mkInteger
    newLargeList <- mkLargeIntegerList 100 N.mkInteger

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

    -- Run the benchmarks.
    C.defaultMain
        [ C.bgroup "Sum 1000 small Integers"
            [ C.bench "GMP"     $ C.whnf (foldl1 G.plusInteger) gmpSmallList
            , C.bench "Simple"  $ C.whnf (foldl1 S.plusInteger) smpSmallList
            , C.bench "New"     $ C.whnf (foldl1 N.plusInteger) newSmallList
            ]
        , C.bgroup "Sum 1000 small positive Integers"
            [ C.bench "GMP"     $ C.whnf (foldl1 G.plusInteger) gmpSmallPosList
            , C.bench "Simple"  $ C.whnf (foldl1 S.plusInteger) smpSmallPosList
            , C.bench "New"     $ C.whnf (foldl1 N.plusInteger) newSmallPosList
            ]
        , C.bgroup "Sum 100 large positive Integers"
            [ C.bench "GMP"     $ C.whnf (foldl1 G.plusInteger) gmpLargePosList
            , C.bench "Simple"  $ C.whnf (foldl1 S.plusInteger) smpLargePosList
            , C.bench "New"     $ C.whnf (foldl1 N.plusInteger) newLargePosList
            ]
        ]

--------------------------------------------------------------------------------
-- Data generators.

mkSmallIntegerList :: Int -> (Int -> a) -> IO [a]
mkSmallIntegerList count constructor =
    fmap (map constructor . take count . R.randoms) R.newStdGen


mkLargeIntegerList :: Int -> (Bool -> [Int] -> a) -> IO [a]
mkLargeIntegerList count constructor = do
    signs <- (take count . R.randoms) <$> R.newStdGen
    lengths <- (take count . R.randomRs (2, 100)) <$> R.newStdGen
    let mkIntList len =
            (take len . R.randomRs (0, 0x7fffffff)) <$> R.newStdGen
    ints <- mapM mkIntList lengths
    return . take count $ zipWith constructor signs ints


--------------------------------------------------------------------------------
-- Helpers.

unboxInt :: Int -> Int#
unboxInt (I# i) = i
