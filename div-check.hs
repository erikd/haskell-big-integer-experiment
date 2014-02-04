{-# LANGUAGE FlexibleInstances, MagicHash, ScopedTypeVariables #-}

import Test.Hspec
import Test.Hspec.QuickCheck

import Check.Helpers

import qualified GMP.Integer as G
import qualified New3.Integer as X


main :: IO ()
main = hspec $ describe "New check:" $ do
    testQuotRem
    testDivMod
    current


current :: Spec
current =
    it "Can quotRem Integers." $ do
        let n = X.mkInteger True [3, 0, 8]
            d = X.smallInteger (0x20000#)
        showUT2 (X.quotRemInteger n d) `shouldBe` "(+0x1000000000000,+0x3)"

   {-
    it "Can quotRem Integers." $ do
        let n = X.mkInteger True [3, 0, 8]
            d = X.mkInteger True [0, 0, 2]
        showUT2 (X.quotRemInteger n d) `shouldBe` showUT2 (G.quotRemInteger (G.smallInteger n) (G.smallInteger d))
    -}



testQuotRem :: Spec
testQuotRem =
    prop "Can quotRem small Integers." $ \ ni di ->
        if ni == 0 || di == 0
            then "a" `shouldBe` "a"
            else
                let n = unboxInt ni
                    d = unboxInt di
                in showUT2 (X.quotRemInteger (X.smallInteger n) (X.smallInteger d)) `shouldBe` showUT2 (G.quotRemInteger (G.smallInteger n) (G.smallInteger d))


testDivMod :: Spec
testDivMod =
    prop "Can divMod small Integers." $ \ ni di ->
        if ni == 0 || di == 0
            then "a" `shouldBe` "a"
            else
                let n = unboxInt ni
                    d = unboxInt di
                in showUT2 (X.divModInteger (X.smallInteger n) (X.smallInteger d)) `shouldBe` showUT2 (G.divModInteger (G.smallInteger n) (G.smallInteger d))
