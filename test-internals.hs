{-# LANGUAGE FlexibleInstances, MagicHash, ScopedTypeVariables #-}


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

    it "wordShiftUndo" $ do
        New4.showNatural (New4.wordShiftUndo (0, 0)) `shouldBe` New4.showNatural New4.zeroNatural
        New4.showNatural (New4.wordShiftUndo (topBitOnly, - (wordSizeInBits - 1))) `shouldBe` New4.showNatural New4.oneNatural

    prop "wordShiftApprox <--> wordShiftUndo (QC)" $ \ words3@(_,_,_) -> do
        let nat = mkNatural3 words3
            (w, s) = New4.wordShiftApprox nat
            natL = New4.wordShiftUndo (w, s)
            natU = New4.wordShiftUndo (w + 1, s)
        New4.leNatural natL nat `shouldBe` True
        New4.leNatural nat natU `shouldBe` True


-- -----------------------------------------------------------------------------

topBitOnly :: Word
topBitOnly = (complement 0) `shiftL` (wordSizeInBits - 1)


mkNatural3 :: (Word, Word, Word) -> New4.Natural
mkNatural3 (w0, w1, 0) = mkNatural3 (w0, w1, 42)
mkNatural3 (W# w0, W# w1, W# w2) =
    New4.plusNatural
        (New4.plusNatural
            (New4.shiftLNatural (New4.wordToNatural w2) (2 * wordSizeInBits))
            (New4.shiftLNatural (New4.wordToNatural w1) wordSizeInBits)
            )
        (New4.wordToNatural w0)


