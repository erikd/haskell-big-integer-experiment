{-# LANGUAGE MagicHash #-}

import Prelude hiding (Integer)

import qualified Test.QuickCheck as QC

import qualified GMP.Integer as G
import qualified Simple.Integer as S

main :: IO ()
main = do
    QC.quickCheck prop_same_mkInteger
    QC.quickCheck prop_same_sum


prop_same_mkInteger :: [Int] -> Bool
prop_same_mkInteger lst =
    let pos = concat . replicate 10 $ map abs lst
    in show (G.mkInteger True pos) == show (S.mkInteger True pos)


prop_same_sum :: [Int] -> [Int] -> Bool
prop_same_sum a b =
    let ap = concat . replicate 7 $ map abs a
        bp = concat . replicate 7 $ map abs b
        ga = G.mkInteger True ap
        gb = G.mkInteger True bp
        sa = S.mkInteger True ap
        sb = S.mkInteger True bp
    in show (ga + gb) == show (sa + sb)
