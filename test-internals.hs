{-# LANGUAGE BangPatterns, CPP, FlexibleInstances, MagicHash, ScopedTypeVariables #-}


import Data.Bits
import GHC.Base
import Test.Hspec
import Test.Hspec.QuickCheck

import Common.GHC.Integer.Prim

import qualified New4.GHC.Integer.Natural as New4
import qualified New4.GHC.Integer.Type as New4


main :: IO ()
main = hspec $
    describe "New4 internals:" testNew4Internals


testNew4Internals :: Spec
testNew4Internals = do
    it "wordShiftApprox" $ do
        New4.wordShiftApprox (New4.smallNatural 0#) `shouldBe` (0, 0)
        New4.wordShiftApprox (New4.smallNatural 1#) `shouldBe` (topBitOnly, - (wordSizeInBits - 1))
        New4.wordShiftApprox (New4.smallNatural 2#) `shouldBe` (topBitOnly, - (wordSizeInBits - 2))
        New4.wordShiftApprox (New4.smallNatural 3#) `shouldBe` (topBitOnly + (topBitOnly `shiftR` 1), - (wordSizeInBits - 2))
        let val = New4.shiftLNatural (New4.smallNatural 1#) wordSizeInBits
        New4.wordShiftApprox val `shouldBe` (topBitOnly, 1)
        New4.wordShiftApprox (mkNatural2 (1, 1)) `shouldBe` (topBitOnly, 1)
        New4.wordShiftApprox (mkNatural2 (2, 1)) `shouldBe` (topBitOnly + 1, 1)
        New4.wordShiftApprox (mkNatural2 (3, 2)) `shouldBe` (topBitOnly, 2)
        New4.wordShiftApprox (mkNatural2 (4, 2)) `shouldBe` (topBitOnly + 1, 2)
        -- New4.wordShiftApprox (mkNatural2 (0, 2)) `shouldBe` (topBitOnly, 2)

    it "wordShiftUndo" $ do
        New4.showNatural (New4.wordShiftUndo (0, 0)) `shouldBe` New4.showNatural New4.zeroNatural
        New4.showNatural (New4.wordShiftUndo (topBitOnly, - (wordSizeInBits - 1))) `shouldBe` New4.showNatural New4.oneNatural

    prop "wordShiftApprox <--> wordShiftUndo (QC)" $ \ words3 -> do
        let nat = mkNatural3 words3
            (w, s) = New4.wordShiftApprox nat
            natL = New4.wordShiftUndo (w, s)
            natU = New4.wordShiftUndo (w + 1, s)
        New4.leNatural natL nat `shouldBe` True
        New4.leNatural nat natU `shouldBe` True

    it "estimateQuotient" $ do
        testEstimateQuotient (mkNatural2 (1,2)) (mkNatural2 (0,1))
        testEstimateQuotient (mkNatural2 (2,2)) (mkNatural2 (0,1))
        testEstimateQuotient (mkNatural2 (0,55)) (mkNatural2 (0,11))
        testEstimateQuotient (mkNatural2 (0,8)) (mkNatural2 (0xf,7))
        testEstimateQuotient (mkNatural2 (0,66)) (mkNatural2 (0,11))

        testEstimateQuotient (mkNatural3 (4,4,4)) (mkNatural3 (1,1,1))
        testEstimateQuotient (mkNatural3 (5,5,5)) (mkNatural2 (1,1))
        testEstimateQuotient (mkNatural3 (0,0,1)) (mkNatural2 (1,1))

    {-
    prop "estimateQuotient (QC)" $ \ (a, b) ->
        testEstimateQuotient (mkNatural3 a) (mkNatural2 b)
    -}

-- -----------------------------------------------------------------------------

testEstimateQuotient :: New4.Natural -> New4.Natural -> Expectation
testEstimateQuotient num@(New4.NatB nn narr) den@(New4.NatB dn darr) = do
    let equot = New4.estimateQuotient nn narr dn darr
        partial = New4.timesNatural equot den
        erem = New4.minusNatural num partial
        check = New4.plusNatural partial erem
    (New4.showNatural check) `shouldBe` (New4.showNatural num)

testEstimateQuotient _ _ = error "Expected NatB NatB"




topBitOnly :: Word
topBitOnly = (complement 0) `shiftL` (wordSizeInBits - 1)


mkNatural2 :: (Word, Word) -> New4.Natural
mkNatural2 (w0, 0) = mkNatural2 (w0, 666)
mkNatural2 (W# w0, W# w1) =
    New4.plusNatural
        (New4.shiftLNatural (New4.wordToNatural w1) wordSizeInBits)
        (New4.wordToNatural w0)

mkNatural3 :: (Word, Word, Word) -> New4.Natural
mkNatural3 (w0, w1, 0) = mkNatural3 (w0, w1, 42)
mkNatural3 (W# w0, W# w1, W# w2) =
    New4.plusNatural
        (New4.plusNatural
            (New4.shiftLNatural (New4.wordToNatural w2) (2 * wordSizeInBits))
            (New4.shiftLNatural (New4.wordToNatural w1) wordSizeInBits)
            )
        (New4.wordToNatural w0)


