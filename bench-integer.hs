{-# LANGUAGE CPP, MagicHash #-}

import Prelude hiding (Integer)

import Control.Applicative ((<$>))
import qualified Criterion.Main as C
import qualified System.Random as R

import Check.BenchG as BenchG
import Check.Bench1 as Bench1
import Check.Bench2 as Bench2
import Check.Bench3 as Bench3
import Check.Bench4 as Bench4
import Check.BenchG as BenchS


main :: IO ()
main = do
    -- (loop count, increment, decrement) increment and decrement should be
    -- chosen so that loop count * (increment - decrement) < maximum value that
    -- can be held in a 64 bit machine word.
    let addSmallParam = (10000000, 63, 62)

    addBigParam <- mkBigParam 10000 2000

    -- Run the benchmarks.
    C.defaultMain
        [ C.bgroup
                ( "Small Integer addition and subtraction"
                )
            [ C.bench "GMP"     $ C.whnf BenchG.addSmallLoop addSmallParam
            , C.bench "Simple"  $ C.whnf BenchS.addSmallLoop addSmallParam
            , C.bench "New1"    $ C.whnf Bench1.addSmallLoop addSmallParam
            , C.bench "New2"    $ C.whnf Bench2.addSmallLoop addSmallParam
            , C.bench "New3"    $ C.whnf Bench3.addSmallLoop addSmallParam
            , C.bench "New4"    $ C.whnf Bench4.addSmallLoop addSmallParam
            ]
        , C.bgroup
                ( "Big Integer addition and subtraction"
                )
            [ C.bench "GMP"     $ C.whnf BenchG.addBigLoop addBigParam
            , C.bench "Simple"  $ C.whnf BenchS.addBigLoop addBigParam
            , C.bench "New1"    $ C.whnf Bench1.addBigLoop addBigParam
            , C.bench "New2"    $ C.whnf Bench2.addBigLoop addBigParam
            , C.bench "New3"    $ C.whnf Bench3.addBigLoop addBigParam
            , C.bench "New4"    $ C.whnf Bench4.addBigLoop addBigParam
            ]
        ]

-- | A function to create a a set of test parameters to pass to addBigLoop.
-- The three parameter are; a loop count, and two lists of 31 bit Ints to be
-- passed to the mkInteger function of the Integer API.
mkBigParam :: Int -> Int -> IO (Int, [Int], [Int])
mkBigParam loopCount len = do
    xs <- R.randomRs (0, 0x7fffffff) <$> R.newStdGen
    let (first, rest) = splitAt len xs
        second = take len rest
    return (loopCount, first, second)
