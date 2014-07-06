{-# LANGUAGE BangPatterns, MagicHash, UnboxedTuples #-}
module Check.Helpers where

import Control.Applicative ((<$>))
import Data.Bits
import GHC.Base
import GHC.Word (Word8)

import qualified System.Random as R

--------------------------------------------------------------------------------
-- Data generators.

mkSmallIntRangeList :: Int -> (Int, Int) -> IO [Int]
mkSmallIntRangeList count (upper, lower) = do
    fmap (take count . R.randomRs (upper, lower)) R.newStdGen


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

{-# INLINE unboxWord #-}
unboxWord :: Word -> Word#
unboxWord !(W# !w) = w

{-# INLINE unboxDouble #-}
unboxDouble :: Double -> Double#
unboxDouble !(D# !d) = d

boxIntHash :: Int# -> Int
boxIntHash i = I# i

boxDoubleHash :: Double# -> Double
boxDoubleHash d = D# d

boxTuple :: (# a, b #) -> (a, b)
boxTuple (# a, b #) = (a, b)

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

showUT2 :: (Show a, Show b) => (# a, b #) -> String
showUT2 (# a, b #) = "(" ++ show a ++ "," ++ show b ++ ")"


-- A slow naive version of log2Word used to test fast versions.
log2WordSlow :: Word# -> Int#
log2WordSlow =
    loop 0#
  where
    loop :: Int# -> Word# -> Int#
    loop acc# w#
        | isTrue# (word2Int# w# ==# 0#) = acc#
        | otherwise = loop (acc# +# 1#) (uncheckedShiftRL# w# 1#)
