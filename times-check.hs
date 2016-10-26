{-# OPTIONS_GHC -fno-warn-orphans #-}

import Control.Monad (when)
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen

import New3.GHC.Integer.Internals
import New3.GHC.Integer.WordArray
import New3.GHC.Integer.Type
import New3.Integer ()

import Check.Helpers


main :: IO ()
main = do
    newTest
    newestTest



newestTest :: IO ()
newestTest  = hspec . describe "timesNaturalNewest" $ do
    it "Can multiply #1." $ do
        let a = mkNatural $ fromString "0x10000000000000001000000000000000100000000000000010000000000000001"
            b = mkNatural $ fromString "0x5000000000000000400000000000000030000000000000002"
            old = timesNatural a b
            new = timesNaturalNewest a b
        (show new, isMinNatural new) `shouldBe` (show old, True)

    it "Can multiply #2." $ do
        let a = mkNatural $ fromString "0x1800000000000000180000000000000018000000000000001"
            b = mkNatural $ fromString "0x500000000000000030000000000000001"
            old = timesNatural a b
            new = timesNaturalNewest a b
        (show new, isMinNatural new) `shouldBe` (show old, True)

    it "Can multiply #3." $ do
        let a = mkNatural $ fromString "0x18000000000000001800000000000000180000000000000018000000000000001"
            b = mkNatural $ fromString "0x7000000000000000500000000000000030000000000000001"
            old = timesNatural a b
            new = timesNaturalNewest a b
        (show new, isMinNatural new) `shouldBe` (show old, True)

    it "Can multiply #4." $ do
        let a = mkNatural . fromString $ "0x" ++ replicate 50 'f'
            b = mkNatural . fromString $ "0x" ++ replicate 40 'f'
            old = timesNatural a b
            new = timesNaturalNewest a b
        (show new, isMinNatural new) `shouldBe` (show old, True)


    it "Can multiply #5." $ do
        let a = mkNatural . fromString $ "0x8000000000000002000000040000001000000020000000200000008000000080000001"
            b = mkNatural . fromString $ "0x80000001000000020000000200000008000000000000002"
            old = timesNatural a b
            new = timesNaturalNewest a b
        (show new, isMinNatural new) `shouldBe` (show old, True)


    it "Can multiply #6." $ do
        let a = mkNatural . fromString $ "0x" ++ replicate 500 'f'
            b = mkNatural . fromString $ "0x" ++ replicate 400 'f'
            old = timesNatural a b
            new = timesNaturalNewest a b
        (show new, isMinNatural new) `shouldBe` (show old, True)


    when True $ modifyMaxSuccess (const 10000) $
        prop "Old and newest give same result (QuickCheck)." $ \ a b -> do
            let old = timesNatural a b
                new = timesNaturalNewest a b
            (show new, isMinNatural new) `shouldBe` (show old, True)

newTest :: IO ()
newTest = hspec . describe "timesNatural" $ do
    it "Old and new give same result #1." $ do
        let a = mkNatural $ fromString "0x100000000000000010000000000000001"
            b = mkNatural $ fromString "0x20000000000000001"
            old = timesNatural a b
            new = timesNaturalNew a b
        (show new, isMinNatural new) `shouldBe` (show old, True)
    it "Old and new give same result #2." $ do
        let a = mkNatural $ fromString "0x100000000000000010000000000000001000000000000000100000000000000010000000000000001"
            b = mkNatural $ fromString "0x40000000000000003000000000000000200000000000000000000000000000001"
            old = timesNatural a b
            new = timesNaturalNew a b
        (show new, isMinNatural new) `shouldBe` (show old, True)

    it "Old and new give same result #3." $ do
        let a = mkNatural $ fromString "0x100000002000000040000000800000000000000200000000000000080000001"
            b = mkNatural $ fromString "0x10000000200000000000000000000000"
            old = timesNatural a b
            new = timesNaturalNew a b
        (show new, isMinNatural new) `shouldBe` (show old, True)

    it "Old and new give same result #3." $ do
        let a = mkNatural $ fromString "0x8000000100000002000000040000000000000010000000200000000000000080000001"
            b = mkNatural $ fromString "0x200000000000000000000000"
            old = timesNatural a b
            new = timesNaturalNew a b
        (show new, isMinNatural new) `shouldBe` (show old, True)

    when True $ modifyMaxSuccess (const 10000) $
        prop "Old and new give same result (QuickCheck)." $ \ a b -> do
            let old = timesNatural a b
                new = timesNaturalNew a b
            (show new, isMinNatural new) `shouldBe` (show old, True)


isMinNatural :: Natural -> Bool
isMinNatural (Natural n arr) = indexWordArray arr (n - 1) /= 0

instance Arbitrary Natural where
    arbitrary = do
        wrds <- fmap positive32bits $ vectorOf 10 arbitrary
        pure $ mkNatural wrds

    -- shrink (Natural n arr) = map (\x -> Natural x arr) $ [ 1 .. (n - 1) ]
    shrink _ = []
