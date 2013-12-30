-- https://en.wikipedia.org/wiki/Karatsuba_algorithm


main :: IO ()
main = do
    let result = karatsuba 123543512412 234452434456
        expected = 28965077246238452467872
    if result == expected
        then putStrLn "Good"
        else putStrLn $ " Result " ++ show result ++ " should be " ++ show expected


karatsuba :: Integer -> Integer -> Integer
karatsuba num1 num2
    | num1 < kBaseSquared || num2 < kBaseSquared = num1 * num2
    | otherwise =
        let m = max (sizeAtBase num1) (sizeAtBase num2)
            m2 = m `div` 2
            (hi1, lo1) = split_at num1 m2
            (hi2, lo2) = split_at num2 m2
            z0 = karatsuba lo1 lo2
            z1 = karatsuba (lo1 + hi1) (lo2 + hi2)
            z2 = karatsuba hi1 hi2
        in (z2 * kBase ^ (2 * m2)) + ((z1 - z2 - z0) * kBase ^ m2) + z0


kBase, kBaseSquared :: Integer
kBase = 100
kBaseSquared = kBase * 2



sizeAtBase :: Integer -> Int
sizeAtBase x = length (show x) `div` length (show (kBase - 1))

split_at :: Integer -> Int -> (Integer, Integer)
split_at i c = quotRem i (kBase ^ c)




{-
karatsubaMultiply :: Integer -> Integer -> Integer
karatsubaMultiply a@(Small _ _) b@(Small _ _) = timesInteger a b
karatsubaMultiply a@(Small _ _) b@(Large _ _ _) = timesInteger a b
karatsubaMultiply a@(Large _ _ _) b@(Small _ _) = timesInteger a b
karatsubaMultiply a@(Large s1 n1 arr1) b@(Large s2 n2 arr2)
    | n1 < 4 || n2 < 4 = timesInteger a b
    | otherwise = karatsubaArray
-}

