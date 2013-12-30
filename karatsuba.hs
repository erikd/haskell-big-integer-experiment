-- https://en.wikipedia.org/wiki/Karatsuba_algorithm

import Control.Monad
import Data.Bits

main :: IO ()
main = do
    isValidBase
    let result = karatsuba 123543512412 234452434456
        expected = 28965077246238452467872
    if result == expected
        then putStrLn "Good"
        else putStrLn $ " Result " ++ show result ++ " should be " ++ show expected


karatsuba :: Integer -> Integer -> Integer
karatsuba num1 num2
    | num1 < kBase || num2 < kBase = num1 * num2
    | otherwise =
        let m2 = (max (sizeAtBase num1) (sizeAtBase num2)) `div` 2
            (hi1, lo1) = split_at num1 m2
            (hi2, lo2) = split_at num2 m2
            z0 = karatsuba lo1 lo2
            z1 = karatsuba (lo1 + hi1) (lo2 + hi2)
            z2 = karatsuba hi1 hi2
        in (z2 * kBase ^ (2 * m2)) + ((z1 - z2 - z0) * kBase ^ m2) + z0


kBase :: Integer
kBase = 1024


isValidBase :: IO ()
isValidBase = do
    when (kBase < 8) $
        error "isValidBase : kBase too snall."
    let isPower2 = (kBase .&. (kBase - 1)) == 0
    let s = show kBase
        slen = length s
        isPower10 = s == ('1' : replicate (slen - 1) '0')
    unless (isPower2 || isPower10) $
        error $ "isValidBase failed for " ++ show kBase
    return ()

sizeAtBase :: Integer -> Int
sizeAtBase x =
    let calcSize acc 0 = acc
        calcSize acc i = calcSize (acc + 1) (i `div` kBase)
    in calcSize 0 (abs x)


split_at :: Integer -> Int -> (Integer, Integer)
split_at i c = quotRem i (kBase ^ c)
