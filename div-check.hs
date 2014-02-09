{-# LANGUAGE FlexibleInstances, MagicHash, ScopedTypeVariables #-}

import Test.Hspec
import Test.Hspec.QuickCheck

import Check.Helpers
import Check.New3

import qualified GMP.Integer as G
import qualified New3.Integer as X


main :: IO ()
main = hspec $ describe "div-check:" $ do
    testQuotRem
    testDivMod
    current


current :: Spec
current = do
    it "Can quotRem Integers." $ do
        let n = X.mkInteger True [3, 0, 8]
            d = X.smallInteger (0x20000#)
        showUT2 (X.quotRemInteger n d) `shouldBe` "(+0x1000000000000,+0x3)"

    prop "Can quotRemNaturalW Integers." $ \ (GNP gn sn) d -> do
        let dNonZero = if d == 0 then 42 else d
        showUT2 (X.quotRemInteger sn (X.smallInteger (unboxInt dNonZero))) `shouldBe` showUT2 (G.quotRemInteger gn (G.smallInteger (unboxInt dNonZero)))

    it "Can quotRem Integers #1." $ do
        let n = [1,2,3,4]
            d = [1,2,3,4]
        showUT2 (X.quotRemInteger (X.mkInteger True n) (X.mkInteger True d)) `shouldBe` showUT2 (G.quotRemInteger (G.mkInteger True n) (G.mkInteger True d))

    it "Can quotRem Integers #2." $ do
        let n = [1,2,3,4]
            d = [0,0,3,4]
        showUT2 (X.quotRemInteger (X.mkInteger True n) (X.mkInteger True d)) `shouldBe` showUT2 (G.quotRemInteger (G.mkInteger True n) (G.mkInteger True d))

    it "Can quotRem Integers #3." $ do
        let n = [1,2,3,4]
            d = [2,2,3,4]
        showUT2 (X.quotRemInteger (X.mkInteger True n) (X.mkInteger True d)) `shouldBe` showUT2 (G.quotRemInteger (G.mkInteger True n) (G.mkInteger True d))

    it "Can quotRem Integers #98." $ do
        let n = [3, 0, 24, 0, 128, 0, 512]
            d = [0, 0, 8]
        putStrLn $ X.hexShow $ X.mkInteger True n
        putStrLn $ X.hexShow $ X.mkInteger True d
        showUT2 (X.quotRemInteger (X.mkInteger True n) (X.mkInteger True d)) `shouldBe` showUT2 (G.quotRemInteger (G.mkInteger True n) (G.mkInteger True d))

    {-
    it "Can quotRem Integers #99." $ do
        let n = [3, 0, 16, 0, 64]
            d = [1, 0, 8]
        showUT2 (X.quotRemInteger (X.mkInteger True n) (X.mkInteger True d)) `shouldBe` showUT2 (G.quotRemInteger (G.mkInteger True n) (G.mkInteger True d))
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
