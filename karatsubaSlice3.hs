{-# LANGUAGE BangPatterns, NoImplicitPrelude, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

#include "MachDeps.h"

import Prelude hiding (Integer)

import Control.Monad (when)
import Control.Monad.Primitive
import GHC.Word (Word)
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck.Arbitrary

import New3.GHC.Integer.Prim
import New3.GHC.Integer.Type
import New3.GHC.Integer.WordArray
import New3.Integer ()

import Check.Helpers

main :: IO ()
main = hspec $ describe "Karatsuba Shifted Add:" testKaratsuba


testKaratsuba :: Spec
testKaratsuba = do
    it "kShiftedAdd #1." $ do
        let n2 = mkNatural (fromString "0x1")
            n1 = mkNatural (fromString "0x2")
            n0 = mkNatural (fromString "0x4")
        show (kShiftedAdd 1 n2 n1 n0) `shouldBe` show (kShiftedAddSlow 1 n2 n1 n0)
        show (kShiftedAdd 2 n2 n1 n0) `shouldBe` show (kShiftedAddSlow 2 n2 n1 n0)
        show (kShiftedAdd 3 n2 n1 n0) `shouldBe` show (kShiftedAddSlow 3 n2 n1 n0)

    it "kShiftedAdd #2." $ do
        let n2 = mkNatural (fromString "0x1")
            n1 = mkNatural (fromString "0x0")
            n0 = mkNatural (fromString "0x0")
        show (kShiftedAdd 1 n2 n1 n0) `shouldBe` show (kShiftedAddSlow 1 n2 n1 n0)
        show (kShiftedAdd 1 n1 n0 n2) `shouldBe` show (kShiftedAddSlow 1 n1 n0 n2)
        show (kShiftedAdd 1 n0 n2 n1) `shouldBe` show (kShiftedAddSlow 1 n0 n2 n1)

    it "kShiftedAdd #3." $ do
        let n2 = mkNatural (fromString "0x1")
            n1 = mkNatural (fromString "0x2")
            n0 = mkNatural (fromString "0x0")
        show (kShiftedAdd 1 n2 n1 n0) `shouldBe` show (kShiftedAddSlow 1 n2 n1 n0)
        show (kShiftedAdd 1 n1 n0 n2) `shouldBe` show (kShiftedAddSlow 1 n1 n0 n2)
        show (kShiftedAdd 1 n0 n2 n1) `shouldBe` show (kShiftedAddSlow 1 n0 n2 n1)

    it "kShiftedAdd #4." $ do
        let n2 = mkNatural (fromString "0x10000000000000001")
            n1 = mkNatural (fromString "0x20000000000000002")
            n0 = mkNatural (fromString "0x40000000000000004")
        show (kShiftedAdd 1 n2 n1 n0) `shouldBe` show (kShiftedAddSlow 1 n2 n1 n0)
        show (kShiftedAdd 2 n2 n1 n0) `shouldBe` show (kShiftedAddSlow 2 n2 n1 n0)
        show (kShiftedAdd 3 n2 n1 n0) `shouldBe` show (kShiftedAddSlow 3 n2 n1 n0)

    it "kShiftedAdd #5." $ do
        let n2 = mkNatural (fromString "0x100000000000000010000000000000001")
            n1 = mkNatural (fromString "0x200000000000000020000000000000002")
            n0 = mkNatural (fromString "0x400000000000000040000000000000004")
        show (kShiftedAdd 1 n2 n1 n0) `shouldBe` show (kShiftedAddSlow 1 n2 n1 n0)
        show (kShiftedAdd 2 n2 n1 n0) `shouldBe` show (kShiftedAddSlow 2 n2 n1 n0)
        show (kShiftedAdd 3 n2 n1 n0) `shouldBe` show (kShiftedAddSlow 3 n2 n1 n0)
        show (kShiftedAdd 4 n2 n1 n0) `shouldBe` show (kShiftedAddSlow 4 n2 n1 n0)

    it "kShiftedAdd #6." $ do
        let n2 = mkNatural (fromString "0x1")
            n1 = mkNatural (fromString "0x20000000000000002")
            n0 = mkNatural (fromString "0x4000000000000000400000000000000040000000000000004")
        show (kShiftedAdd 1 n2 n1 n0) `shouldBe` show (kShiftedAddSlow 1 n2 n1 n0)
        show (kShiftedAdd 2 n2 n1 n0) `shouldBe` show (kShiftedAddSlow 2 n2 n1 n0)
        show (kShiftedAdd 3 n2 n1 n0) `shouldBe` show (kShiftedAddSlow 3 n2 n1 n0)

    it "kShiftedAdd #7." $ do
        let n2 = mkNatural (fromString "0x10000000000000001")
            n1 = mkNatural (fromString "0x20000000000000002")
            n0 = mkNatural (fromString "0x4")
        show (kShiftedAdd 1 n2 n1 n0) `shouldBe` show (kShiftedAddSlow 1 n2 n1 n0)
        show (kShiftedAdd 2 n2 n1 n0) `shouldBe` show (kShiftedAddSlow 2 n2 n1 n0)
        show (kShiftedAdd 3 n2 n1 n0) `shouldBe` show (kShiftedAddSlow 3 n2 n1 n0)
        show (kShiftedAdd 4 n2 n1 n0) `shouldBe` show (kShiftedAddSlow 4 n2 n1 n0)

    it "kShiftedAdd #8." $ do
        let n2 = mkNatural (fromString "0x0")
            n1 = mkNatural (fromString "0x0")
            n0 = mkNatural (fromString "0x100000000000000010000000000000001")
        show (kShiftedAdd 1 n2 n1 n0) `shouldBe` show (kShiftedAddSlow 1 n2 n1 n0)
        show (kShiftedAdd 2 n2 n1 n0) `shouldBe` show (kShiftedAddSlow 2 n2 n1 n0)
        show (kShiftedAdd 3 n2 n1 n0) `shouldBe` show (kShiftedAddSlow 3 n2 n1 n0)

    it "kShiftedAdd #9." $ do
        let n2 = mkNatural (fromString "0x0")
            n1 = mkNatural (fromString "0x100000000000000010000000000000001")
            n0 = mkNatural (fromString "0x0")
        show (kShiftedAdd 1 n2 n1 n0) `shouldBe` show (kShiftedAddSlow 1 n2 n1 n0)
        show (kShiftedAdd 2 n2 n1 n0) `shouldBe` show (kShiftedAddSlow 2 n2 n1 n0)
        show (kShiftedAdd 3 n2 n1 n0) `shouldBe` show (kShiftedAddSlow 3 n2 n1 n0)

    it "kShiftedAdd #10." $ do
        let n2 = mkNatural (fromString "0x1")
            n1 = mkNatural (fromString "0x0")
            n0 = mkNatural (fromString "0x1000000000000000200000000000000030000000000000004")
        show (kShiftedAdd 1 n2 n1 n0) `shouldBe` show (kShiftedAddSlow 1 n2 n1 n0)
        show (kShiftedAdd 2 n2 n1 n0) `shouldBe` show (kShiftedAddSlow 2 n2 n1 n0)
        show (kShiftedAdd 3 n2 n1 n0) `shouldBe` show (kShiftedAddSlow 3 n2 n1 n0)

    it "kShiftedAdd #10." $ do
        let n2 = mkNatural (fromString "0x1")
            n1 = mkNatural (fromString "0x1000000000000000100000000000000010000000000000001000000000000000")
            n0 = mkNatural (fromString "0x2000000000000000200000000000000020000000000000002")
        show (kShiftedAdd 1 n2 n1 n0) `shouldBe` show (kShiftedAddSlow 1 n2 n1 n0)
        -- show (kShiftedAdd 2 n2 n1 n0) `shouldBe` show (kShiftedAddSlow 2 n2 n1 n0)
        -- show (kShiftedAdd 3 n2 n1 n0) `shouldBe` show (kShiftedAddSlow 3 n2 n1 n0)


    when False $
        prop "kShiftedAdd property #1." $ \ n1 n0 -> do
            let n2 = mkNatural (fromString "0x1")
            show (kShiftedAdd 1 n2 n1 n0) `shouldBe` show (kShiftedAddSlow 1 n2 n1 n0)

    when False $
        prop "kShiftedAdd works arbitrary overlap." $ \ n2 n1 n0 wshift -> do
            let shift = 1 + ((abs wshift) `mod` 8)
            print (n2, n1, n0, shift)
            show (kShiftedAdd shift n2 n1 n0) `shouldBe` show (kShiftedAddSlow shift n2 n1 n0)




kShiftedAddSlow :: Int -> Natural -> Natural -> Natural -> Natural
kShiftedAddSlow shift n2 n1 n0 =
    plusNatural (shiftLNatural n2 (2 * shift * WORD_SIZE_IN_BITS))
        (plusNatural (shiftLNatural n1 (shift * WORD_SIZE_IN_BITS)) n0)


kShiftedAdd :: Int -> Natural -> Natural -> Natural -> Natural
kShiftedAdd !shift !(Natural !n2 !arr2) !(Natural !n1 !arr1) !(Natural !n0 !arr0)
    | shift < 1 = error $ "kShiftedAdd with shift of " ++ show shift
    | otherwise = unsafeInlinePrim $ do
        len <- return . succ $ max n0 (max (n1 + shift) (n2 + 2 * shift))
        debugPrint __LINE__ $ "length is " ++ show len ++ " " ++ show (0 :: Int, (n2, n1, n0), shift)
        !marr <- newWordArray len
        setWordArray marr 0 len (0xaeaeaeaaeaeaeaea :: Word)
        !nlen <- start marr
        if nlen > len
            then error "Bad length"
            else return ()
        !narr <- unsafeFreezeWordArray marr
        returnNatural nlen narr
  where
    start !marr
        | n0 <= shift = short0 marr 0
        | otherwise = long0 marr 0

    short0 !marr !i
        | i < n0 = do
            !x <- indexWordArrayM arr0 i
            debugWriteWordArray __LINE__ marr i x
            short0 marr (i + 1)
        | i <= shift = short0fill marr i

        | n1 <= shift = do
            debugPrint __LINE__ $ show (i, (n2, n1, n0), shift)
            noOverlapC marr i

        | otherwise = do { debugPrint __LINE__ $ show (i, (n2, n1, n0), shift) ; return i }

    short0fill !marr !i
        | i < shift = do
            debugWriteWordArray __LINE__ marr i 0
            short0fill marr (i + 1)
        | n1 <= shift = short1 marr i
        | otherwise = long1 marr i

    short1 !marr !i
        | i - shift < n1 = do
            !x <- indexWordArrayM arr1 (i - shift)
            debugWriteWordArray __LINE__ marr i x
            short1 marr (i + 1)
        | otherwise = short1fill marr i

    short1fill !marr !i
        | i < 2 * shift = do
            debugWriteWordArray __LINE__ marr i 0
            short1fill marr (i + 1)
        | otherwise = final2 marr i

    long1 !marr !i
        | i < 2 * shift = do
            !x <- indexWordArrayM arr1 (i - shift)
            debugWriteWordArray __LINE__ marr i x
            long1 marr (i + 1)
        | n1 - shift < n2 = overlap12short1 marr i 0
        | otherwise = overlap12short2 marr i 0

    long0 !marr !i
        | i < shift = do
            !x <- indexWordArrayM arr0 i
            debugWriteWordArray __LINE__ marr i x
            long0 marr (i + 1)
        | n0 - shift > n1 =
            if n1 < shift
                then overlap01short0 marr i 0
                else overlap01long0 marr i 0
        | n1 > shift =
            if n0 < 2 * shift
                then overlap01short0long1 marr i 0
                else overlap01long01 marr i 0
        | n0 < 2 * shift =
            if n1 < n0 - shift
                then do { debugPrint __LINE__ $ show (i, (n2, n1, n0), shift) ; return i }
                else overlap01short01 marr i 0
        | otherwise = overlap01long0 marr i 0

    overlap01short01 !marr !i !carry
        | i < n0 = do
            !x <- indexWordArrayM arr0 i
            !y <- indexWordArrayM arr1 (i - shift)
            let (# !cry, !sm #) = plusWord2C x y carry
            debugWriteWordArray __LINE__ marr i sm
            overlap01short01 marr (i + 1) cry
        | otherwise = overlap01short01c marr i carry

    overlap01short01c !marr !i !carry
        | i - shift < n1 = do
            !x <- indexWordArrayM arr1 (i - shift)
            let (# !cry, !sm #) = plusWord2 x carry
            debugWriteWordArray __LINE__ marr i sm
            overlap01short01c marr (i + 1) cry
        | carry /= 0 = do
            debugWriteWordArray __LINE__ marr i carry
            overlap01fill marr (i + 1)
        | otherwise = overlap01fill marr i

    overlap01fill !marr !i
        | i < 2 * shift = do
            debugWriteWordArray __LINE__ marr i 0
            overlap01fill marr (i + 1)
        | otherwise = final2 marr i

    overlap01short0long1 !marr !i !carry
        | i < n0 = do
            !x <- indexWordArrayM arr0 i
            !y <- indexWordArrayM arr1 (i - shift)
            let (# !cry, !sm #) = plusWord2C x y carry
            debugWriteWordArray __LINE__ marr i sm
            overlap01short0long1 marr (i + 1) cry
        | n1 > shift =
            if carry == 0
                then overlap01copy1 marr i
                else overlap01add1 marr i carry
        | otherwise = do { debugPrint __LINE__ $ show (i, (n2, n1, n0), shift) ; return i }

    overlap01add1 !marr !i !carry
        | i < 2 * shift = do
            !x <- indexWordArrayM arr1 (i - shift)
            let (# !cry, !sm #) = plusWord2 x carry
            debugWriteWordArray __LINE__ marr i sm
            if cry == 0
                then overlap01copy1 marr (i + 1)
                else overlap01add1 marr (i + 1) cry
        | otherwise = do { debugPrint __LINE__ $ show (i, (n2, n1, n0), shift) ; return i }

    overlap01copy1 marr i
        | i < 2 * shift = do
            !x <- indexWordArrayM arr1 (i - shift)
            debugWriteWordArray __LINE__ marr i x
            overlap01copy1 marr (i + 1)
        | n1 - shift < n2 = overlap12long2 marr i 0
        | otherwise = do { debugPrint __LINE__ $ show (i, (n2, n1, n0), shift) ; return i }

    overlap12long2 !marr !i !carry
        | i - shift < n1 = do
            !x <- indexWordArrayM arr1 (i - shift)
            !y <- indexWordArrayM arr2 (i - 2 * shift)
            let (# !cry, !sm #) = plusWord2C x y carry
            debugWriteWordArray __LINE__ marr i sm
            overlap12long2 marr (i + 1) cry
        | carry == 0 = final2 marr i
        | otherwise = do { debugPrint __LINE__ $ show (i, (n2, n1, n0), shift) ; return i }

    overlap01long01 !marr !i !carry
        | i < 2 * shift = do
            !x <- indexWordArrayM arr0 i
            !y <- indexWordArrayM arr1 (i - shift)
            let (# !cry, !sm #) = plusWord2C x y carry
            debugWriteWordArray __LINE__ marr i sm
            overlap01long01 marr (i + 1) cry
        | n0 <= 2 * shift =
            if n1 <= shift
                then do { debugPrint __LINE__ $ show (i, (n2, n1, n0), shift) ; return i }
                else if n2 < n1 - shift
                        then do { debugPrint __LINE__ $ show (i, (n2, n1, n0), shift) ; return i }
                        else do
                            debugPrint __LINE__ $ show (i, (n2, n1, n0), shift)
                            overlap12short1 marr i carry
        | n0 - 2 * shift < n1 - shift && n0 - 2 * shift < n2 =
            overlap012short0 marr i carry
        | otherwise = do { debugPrint __LINE__ $ show (i, (n2, n1, n0), shift) ; return i }

    overlap012short0 !marr !i !carry
        | i < n0 = do
            !x <- indexWordArrayM arr0 i
            !y <- indexWordArrayM arr1 (i - shift)
            !z <- indexWordArrayM arr2 (i - 2 * shift)
            let (# !cry1, !sm1 #) = plusWord2C x y z
            let (# !cry, !sm #) = plusWord2C sm1 cry1 carry
            debugWriteWordArray __LINE__ marr i sm
            overlap012short0 marr (i + 1) cry
        | n1 - shift < n2 = overlap12short1 marr i carry
        | otherwise = do { debugPrint __LINE__ $ show (i, (n2, n1, n0), shift) ; return i }

    overlap12short2 !marr !i !carry
        | i - 2 * shift < n2 = do
            !x <- indexWordArrayM arr1 (i - shift)
            !y <- indexWordArrayM arr2 (i - 2 * shift)
            let (# !cry, !sm #) = plusWord2C x y carry
            debugWriteWordArray __LINE__ marr i sm
            overlap12short2 marr (i + 1) cry
        | carry == 0 = final1 marr i
        | otherwise = final1c marr i carry

    overlap12short1 !marr !i !carry
        | i - shift < n1 = do
            !x <- indexWordArrayM arr1 (i - shift)
            !y <- indexWordArrayM arr2 (i - 2 * shift)
            let (# !cry, !sm #) = plusWord2C x y carry
            debugWriteWordArray __LINE__ marr i sm
            overlap12short1 marr (i + 1) cry
        | i - 2 * shift < n2 =
            if carry /= 0
                then final2c marr i carry
                else final2 marr i
        | carry == 0 = do { debugPrint __LINE__ $ show (i, (n2, n1, n0), shift) ; return i }
        | otherwise = do { debugPrint __LINE__ $ show (i, (n2, n1, n0), shift) ; return i }

    final1 !marr !i
        | i - shift < n1 = do
            !x <- indexWordArrayM arr1 (i - shift)
            debugWriteWordArray __LINE__ marr i x
            final1 marr (i + 1)
        | otherwise = return i

    final1c marr i carry
        | i - shift < n1 = do
            !x <- indexWordArrayM arr2 (i - shift)
            let (# !cry, !sm #) = plusWord2 x carry
            debugWriteWordArray __LINE__ marr i sm
            if carry == 0
                then final1 marr (i + 1)
                else final1c marr i cry
        | otherwise = do
            debugWriteWordArray __LINE__ marr i carry
            return (i + 1)


    final2 !marr !i
        | i - 2 * shift < n2 = do
            !x <- indexWordArrayM arr2 (i - 2 * shift)
            debugWriteWordArray __LINE__ marr i x
            final2 marr (i + 1)
        | otherwise = return i

    final2c !marr !i !carry
        | i - 2 * shift < n2 = do
            !x <- indexWordArrayM arr2 (i - 2 * shift)
            let (# !cry, !sm #) = plusWord2 x carry
            debugWriteWordArray __LINE__ marr i sm
            if carry == 0
                then final2 marr (i + 1)
                else final2c marr i cry
        | otherwise = do
            debugWriteWordArray __LINE__ marr i carry
            return (i + 1)

    overlap01long0 !marr !i !carry
        | i < 2 * shift = do
            !x <- indexWordArrayM arr0 i
            !y <- indexWordArrayM arr1 (i - shift)
            let (# !cry, !sm #) = plusWord2C x y carry
            debugWriteWordArray __LINE__ marr i sm
            overlap01long0 marr (i + 1) cry

        | i - shift < n1 && i - 2 * shift < n0 =
            if n2 < n1 - shift
                then if n2 < n0 - 2 * shift
                        then do { debugPrint __LINE__ $ show (i, (n2, n1, n0), shift) ; return i }
                        else do { debugPrint __LINE__ $ show (i, (n2, n1, n0), shift) ; return i }
                else if n1 < n0 - 2 * shift
                        then do { debugPrint __LINE__ $ show (i, (n2, n1, n0), shift) ; return i }
                        else overlap012short1 marr i carry

        | i - 2 * shift < n0 = do
                if carry /= 0
                    then overlap01long0restc marr i carry
                    else overlap01long0rest marr i
        | otherwise = do { debugPrint __LINE__ $ show (i, (n2, n1, n0), shift) ; return i }

    overlap01long0restc !marr !i !carry
        | i < 2 * shift = do
            !x <- indexWordArrayM arr0 i
            let (# !cry, !sm #) = plusWord2 x carry
            debugWriteWordArray __LINE__ marr i sm
            if cry == 0
                then overlap01long0rest marr (i + 1)
                else overlap01long0restc marr (i + 1) cry
        | carry == 0 = do { debugPrint __LINE__ $ show (i, (n2, n1, n0), shift) ; return i }
        | otherwise = do { debugPrint __LINE__ $ show (i, (n2, n1, n0), shift) ; return i }

    overlap01long0rest !marr !i
        | i < 2 * shift = do
            !x <- indexWordArrayM arr0 i
            debugWriteWordArray __LINE__ marr i x
            overlap01long0rest marr (i + 1)
        | n0 - 2 * shift < n2 = loverlap02short0 marr i 0
        | otherwise = overlap02short2 marr i 0

    loverlap02short0 !marr !i !carry
        | i < n0 = do
            !x <- indexWordArrayM arr0 i
            !y <- indexWordArrayM arr2 (i - 2 * shift)
            let (# !cry, !sm #) = plusWord2C x y carry
            debugWriteWordArray __LINE__ marr i sm
            loverlap02short0 marr (i + 1) cry
        | carry /= 0 = final2c marr i carry
        | otherwise = final2 marr i

    overlap012short1 !marr !i !carry
        | i - shift < n1 = do
            !x <- indexWordArrayM arr0 i
            !y <- indexWordArrayM arr1 (i - shift)
            !z <- indexWordArrayM arr2 (i - 2 * shift)
            let (# !cry1, !sm1 #) = plusWord2C x y z
            let (# !cry, !sm #) = plusWord2C sm1 cry1 carry
            debugWriteWordArray __LINE__ marr i sm
            overlap012short1 marr (i + 1) cry
        | n0 - 2 * shift <= n2 = do { debugPrint __LINE__ $ show (i, (n2, n1, n0), shift) ; return i }
        | otherwise = overlap02short2 marr i carry

    overlap02short2 marr i carry
        | i - 2 * shift < n2 = do
            !x <- indexWordArrayM arr0 i
            !y <- indexWordArrayM arr2 (i - 2 * shift)
            let (# !cry, !sm #) = plusWord2C x y carry
            debugWriteWordArray __LINE__ marr i sm
            overlap02short2 marr (i + 1) cry
        | carry /= 0 = final0c marr i carry
        | otherwise = final0 marr i


    final0 !marr !i
        | i < n0 = do
            !x <- indexWordArrayM arr0 i
            debugWriteWordArray __LINE__ marr i x
            final0 marr (i + 1)
        | otherwise = return i

    final0c !marr !i !carry
        | i < n0 = do
            !x <- indexWordArrayM arr0 i
            let (# !cry, !sm #) = plusWord2 x carry
            debugWriteWordArray __LINE__ marr i sm
            if carry == 0
                then final0 marr (i + 1)
                else final0c marr i cry
        | otherwise = do
            debugWriteWordArray __LINE__ marr i carry
            return (i + 1)

    overlap01short0 !marr !i !carry
        | i < n0 = do
            !x <- indexWordArrayM arr0 i
            !y <- indexWordArrayM arr1 (i - shift)
            let (# !cry, !sm #) = plusWord2C x y carry
            debugWriteWordArray __LINE__ marr i sm
            overlap01short0 marr (i + 1) cry

        | n1 > shift = do { debugPrint __LINE__ $ show (i, (n2, n1, n0), shift) ; return i }
        | carry /= 0 = do { debugPrint __LINE__ $ show (i, (n2, n1, n0), shift) ; return i }
        | otherwise =  overlap01short01 marr i 0




    noOverlapC !marr !i
        | i - shift < n1 = do
            !x <- indexWordArrayM arr1 (i - shift)
            debugWriteWordArray __LINE__ marr i x
            noOverlapC marr (i + 1)

        | n1 > shift = do { debugPrint __LINE__ $ show (i, (n2, n1, n0), shift) ; return i }

        | i < 2 * shift = noOverlapD marr i
        | otherwise = noOverlapE marr i

    noOverlapD !marr !i
        | i < 2 * shift = do
            debugWriteWordArray __LINE__ marr i 0
            noOverlapD marr (i + 1)
        | otherwise = noOverlapE marr i

    noOverlapE !marr !i
        | i - 2 * shift < n2 = do
            !x <- indexWordArrayM arr2 (i - 2 * shift)
            debugWriteWordArray __LINE__ marr i x
            noOverlapE marr (i + 1)
        | otherwise = return i

--------------------------------------------------------------------------------

debugWriteWordArray :: Int -> MutableWordArray IO -> Int -> Word -> IO ()
debugWriteWordArray line marr i x = do
    debugPrint line $ "writing " ++ hexShowW x ++ " at " ++ show i
    writeWordArray marr i x

debugPrint :: Int -> String -> IO ()
#if DEBUG
debugPrint line str =
    let ls
            | line < 100 = ' ' : show line
            | otherwise = show line
    in putStrLn $ ls ++ " : " ++ str
#else
debugPrint _ _ = return ()
#endif


instance Arbitrary Natural where
    arbitrary = do
        wrds <- fmap (take 10 . positive32bits) arbitrary
        return $ mkNatural wrds

    shrink (Natural n arr) =
        map (\x -> Natural x arr) $ [ 0 .. (n - 1) ]

