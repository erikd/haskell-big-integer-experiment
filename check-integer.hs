{-# LANGUAGE MagicHash #-}

import Prelude hiding (Integer)

import GHC.Base
import Test.Hspec
import Test.Hspec.QuickCheck

import qualified GMP.Integer as G
import qualified Simple.Integer as S

main :: IO ()
main = hspec $
    describe "Comparing GMP and Simple Integer operations:" testInteger


testInteger :: Spec
testInteger = do
    prop "Can create Integers." $ \ lst ->
        let pos = concat . replicate 8 $ map abs lst
        in show (G.mkInteger True pos) == show (S.mkInteger True pos)
    prop "Can add Integers." $ \ a b ->
        let (ga, gb, sa, sb) = mkFourIntegers 6 a b
        in show (ga + gb) == show (sa + sb)
    prop "Can subtract Integers." $ \ a b ->
        let (ga, gb, sa, sb) = mkFourIntegers 6 a b
        in show (ga - gb) == show (sa - sb)
    prop "Can multiply Integers." $ \ a b ->
        let (ga, gb, sa, sb) = mkFourIntegers 4 a b
        in show (ga * gb) == show (sa * sb)
    prop "Can negate Integers." $ \ lst ->
        let pos = concat . replicate 8 $ map abs lst
            gi = G.mkInteger True pos
            si = S.mkInteger True pos
        in show (G.negateInteger gi) == show (S.negateInteger si)
            && show (gi + G.negateInteger gi) == "0"
            && show (si + S.negateInteger si) == "0"
    prop "Can convert to Int." $ \ lst ->
        let pos = concat . replicate 8 $ map abs lst
            gi = G.mkInteger True pos
            si = S.mkInteger True pos
        in show (boxIntHash (G.integerToInt gi)) == show (boxIntHash (S.integerToInt si))
    prop "Can hash an Integer." $ \ lst ->
        let pos = concat . replicate 8 $ map abs lst
            gi = G.mkInteger True pos
            si = S.mkInteger True pos
        in show (boxIntHash (G.hashInteger gi)) == show (boxIntHash (S.hashInteger si))


mkFourIntegers :: Int -> [Int] -> [Int] -> (G.Integer, G.Integer, S.Integer, S.Integer)
mkFourIntegers rep a b =
    let ap = concat . replicate rep $ map abs a
        bp = concat . replicate rep $ map abs b
    in (G.mkInteger True ap, G.mkInteger True bp, S.mkInteger True ap, S.mkInteger True bp)

boxIntHash :: Int# -> Int
boxIntHash i = I# i
