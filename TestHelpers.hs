{-# LANGUAGE MagicHash #-}
module TestHelpers where

import Prelude hiding (Integer)

import Control.Applicative ((<$>))
import Data.Bits
import GHC.Base

import qualified System.Random as R

--------------------------------------------------------------------------------
-- Data generators.

mkSmallIntegerList :: Int -> (Int -> a) -> IO [a]
mkSmallIntegerList count constructor =
    fmap (map constructor . take count . R.randoms) R.newStdGen


mkLargeIntegerList :: Int -> (Int, Int) -> IO [(Bool, [Int])]
mkLargeIntegerList count range = do
    signs <- (take count . R.randoms) <$> R.newStdGen
    lengths <- (take count . R.randomRs range) <$> R.newStdGen
    let mkIntList len =
            (take len . R.randomRs (0, 0x7fffffff)) <$> R.newStdGen
    ints <- mapM mkIntList lengths
    return . take count $ zipWith (,) signs ints

unboxInt :: Int -> Int#
unboxInt (I# i) = i

boxIntHash :: Int# -> Int
boxIntHash i = I# i

-- The mkInteger functions expect values in range [0, 0x7fffffff].
positive32bits :: [Int] -> [Int]
positive32bits = map (0x7fffffff .&.)
