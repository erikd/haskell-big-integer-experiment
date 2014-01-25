{-# OPTIONS_GHC -fno-warn-orphans #-}

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
main = hspec . describe "timesNatural" $ do
    prop "Old and new give same result." $ \ a b -> do
        let old = timesNatural a b
            new = timesNaturalNew a b
        (show new, isMinNatural new) `shouldBe` (show old, True)


isMinNatural :: Natural -> Bool
isMinNatural (Natural n arr) = indexWordArray arr (n - 1) /= 0

instance Arbitrary Natural where
    arbitrary = do
        wrds <- fmap positive32bits $ vectorOf 10 arbitrary
        return $ mkNatural wrds

    -- shrink (Natural n arr) = map (\x -> Natural x arr) $ [ 0 .. (n - 1) ]
    shrink _ = []
