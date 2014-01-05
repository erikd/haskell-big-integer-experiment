{-# LANGUAGE BangPatterns, MagicHash #-}
module Check.Helpers where

import Control.Applicative ((<$>))
import Data.Bits
import GHC.Base
import GHC.Word (Word8)

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

{-# INLINE unboxWord #-}
unboxWord :: Word -> Word#
unboxWord !(W# !w) = w

-- The mkInteger functions expect values in range [0, 0x7fffffff].
positive32bits :: [Int] -> [Int]
positive32bits = map (\i -> (abs i) .&. 0x7fffffff)


shiftCount :: Word8 -> Int#
shiftCount w =
    let !(I# i) = fromIntegral w
    in i

readInteger :: String -> Integer
readInteger = read


fromString :: String -> [Int]
fromString s =
    decompose $ readInteger s
  where
    decompose x
        | x <= 0 = []
        | otherwise =
            fromIntegral (x .&. 0x7fffffff) : decompose (x `shiftR` 31)
