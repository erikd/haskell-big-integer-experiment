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
            n1 = mkNatural (fromString "0x1000000000000000100000000000000010000000000000001")
            n0 = mkNatural (fromString "0x200000000000000020000000000000002")
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
        | n0 <= shift = stage0short0 marr 0
        | otherwise = stage0long0 marr 0

    -- Stage 0.
    stage0short0 !marr !i
        | i < n0 = do
            !x <- indexWordArrayM arr0 i
            debugWriteWordArray __LINE__ marr i x
            stage0short0 marr (i + 1)
        | otherwise = stage0short0fill marr i

    stage0short0fill !marr !i
        | i < shift = do
            debugWriteWordArray __LINE__ marr i 0
            stage0short0fill marr (i + 1)
        | n1 <= shift = stage1do1 marr i
        | otherwise = stage1copy1 marr i

    stage0long0 !marr !i
        | i < shift = do
            !x <- indexWordArrayM arr0 i
            debugWriteWordArray __LINE__ marr i x
            stage0long0 marr (i + 1)
        | n0 - shift > n1 =
            if n1 < shift
                then stage1do01 marr i 0
                else stage1long01 marr i 0
        | n1 > shift =
            if n0 < 2 * shift
                then stage1do01 marr i 0
                else stage1long01 marr i 0
        | n0 < 2 * shift =
            if n1 < n0 - shift
                then do { debugPrint __LINE__ $ show (i, (n2, n1, n0), shift) ; return i }
                else stage1do01 marr i 0
        | otherwise = stage1long01 marr i 0

    -- Stage 1.
    stage1do1 !marr !i
        | i - shift < n1 = do
            !x <- indexWordArrayM arr1 (i - shift)
            debugWriteWordArray __LINE__ marr i x
            stage1do1 marr (i + 1)
        | i < 2 * shift = stage1fill marr i
        | n1 - shift < n2 = stage2do12 marr i 0
        | otherwise = stage2do21 marr i 0

    stage1copy1 !marr !i
        | i < 2 * shift = do
            !x <- indexWordArrayM arr1 (i - shift)
            debugWriteWordArray __LINE__ marr i x
            stage1copy1 marr (i + 1)
        | n1 - shift < n2 = stage2do12 marr i 0
        | otherwise = stage2do21 marr i 0

    stage1do01 !marr !i !carry
        | i < n0 = do
            !x <- indexWordArrayM arr0 i
            !y <- indexWordArrayM arr1 (i - shift)
            let (# !cry, !sm #) = plusWord2C x y carry
            debugWriteWordArray __LINE__ marr i sm
            stage1do01 marr (i + 1) cry
        | n1 > shift =
            if carry == 0
                then stage1copy1 marr i
                else stage1add1 marr i carry
        | otherwise = stage1final1c marr i carry

    stage1final1c !marr !i !carry
        | i - shift < n1 = do
            !x <- indexWordArrayM arr1 (i - shift)
            let (# !cry, !sm #) = plusWord2 x carry
            debugWriteWordArray __LINE__ marr i sm
            stage1final1c marr (i + 1) cry
        | carry /= 0 = do
            debugWriteWordArray __LINE__ marr i carry
            stage1fill marr (i + 1)
        | otherwise = stage1fill marr i

    stage1fill !marr !i
        | i < 2 * shift = do
            debugWriteWordArray __LINE__ marr i 0
            stage1fill marr (i + 1)
        | otherwise = stage2final2 marr i

    stage1add1 !marr !i !carry
        | i < 2 * shift = do
            !x <- indexWordArrayM arr1 (i - shift)
            let (# !cry, !sm #) = plusWord2 x carry
            debugWriteWordArray __LINE__ marr i sm
            if cry == 0
                then stage1copy1 marr (i + 1)
                else stage1add1 marr (i + 1) cry
        | otherwise = do { debugPrint __LINE__ $ show (i, (n2, n1, n0), shift) ; return i }

    stage1long01 !marr !i !carry
        | i < 2 * shift = do
            !x <- indexWordArrayM arr0 i
            !y <- indexWordArrayM arr1 (i - shift)
            let (# !cry, !sm #) = plusWord2C x y carry
            debugWriteWordArray __LINE__ marr i sm
            stage1long01 marr (i + 1) cry

        | n0 <= 2 * shift =
            if n1 <= shift
                then stage2final2c marr i carry
                else if n2 < n1 - shift
                        then do { debugPrint __LINE__ $ show (i, (n2, n1, n0), shift) ; return i }
                        else stage2do12 marr i carry

        | n0 - 2 * shift < n1 - shift && n0 - 2 * shift < n2 =
            stage2short0 marr i carry


        | n1 > shift && n0 > 2 * shift =
            if n2 < n1 - shift
                then if n2 < n0 - 2 * shift
                        then do { debugPrint __LINE__ $ show (i, (n2, n1, n0), shift) ; return i }
                        else do { debugPrint __LINE__ $ show (i, (n2, n1, n0), shift) ; return i }
                else if n1 < n0 - 2 * shift
                        then do { debugPrint __LINE__ $ show (i, (n2, n1, n0), shift) ; return i }
                        else stage12short1 marr i carry

        | carry /= 0 = stage1long0restc marr i carry
        | otherwise = stage1long0rest marr i


    stage1long0restc !marr !i !carry
        | i < 2 * shift = do
            !x <- indexWordArrayM arr0 i
            let (# !cry, !sm #) = plusWord2 x carry
            debugWriteWordArray __LINE__ marr i sm
            if cry == 0
                then stage1long0rest marr (i + 1)
                else stage1long0restc marr (i + 1) cry
        | carry == 0 = do { debugPrint __LINE__ $ show (i, (n2, n1, n0), shift) ; return i }
        | otherwise = do { debugPrint __LINE__ $ show (i, (n2, n1, n0), shift) ; return i }

    stage1long0rest !marr !i
        | i < 2 * shift = do
            !x <- indexWordArrayM arr0 i
            debugWriteWordArray __LINE__ marr i x
            stage1long0rest marr (i + 1)
        | n0 - 2 * shift < n2 = stage2do02 marr i 0
        | otherwise = stage2with20 marr i 0

    stage2short0 !marr !i !carry
        | i < n0 = do
            !x <- indexWordArrayM arr0 i
            !y <- indexWordArrayM arr1 (i - shift)
            !z <- indexWordArrayM arr2 (i - 2 * shift)
            let (# !cry1, !sm1 #) = plusWord2C x y z
            let (# !cry, !sm #) = plusWord2C sm1 cry1 carry
            debugWriteWordArray __LINE__ marr i sm
            stage2short0 marr (i + 1) cry
        | n1 - shift < n2 = stage2do12 marr i carry
        | otherwise = do { debugPrint __LINE__ $ show (i, (n2, n1, n0), shift) ; return i }

    -- Stage 2.
    stage2do21 !marr !i !carry
        | i - 2 * shift < n2 = do
            !x <- indexWordArrayM arr1 (i - shift)
            !y <- indexWordArrayM arr2 (i - 2 * shift)
            let (# !cry, !sm #) = plusWord2C x y carry
            debugWriteWordArray __LINE__ marr i sm
            stage2do21 marr (i + 1) cry
        | carry == 0 = stage2final1 marr i
        | otherwise = stage2final1c marr i carry

    stage2do12 !marr !i !carry
        | i - shift < n1 = do
            !x <- indexWordArrayM arr1 (i - shift)
            !y <- indexWordArrayM arr2 (i - 2 * shift)
            let (# !cry, !sm #) = plusWord2C x y carry
            debugWriteWordArray __LINE__ marr i sm
            stage2do12 marr (i + 1) cry
        | i - 2 * shift < n2 =
            if carry /= 0
                then stage2final2c marr i carry
                else stage2final2 marr i
        | carry == 0 = do { debugPrint __LINE__ $ show (i, (n2, n1, n0), shift) ; return i }
        | otherwise = do { debugPrint __LINE__ $ show (i, (n2, n1, n0), shift) ; return i }


    stage2do02 !marr !i !carry
        | i < n0 = do
            !x <- indexWordArrayM arr0 i
            !y <- indexWordArrayM arr2 (i - 2 * shift)
            let (# !cry, !sm #) = plusWord2C x y carry
            debugWriteWordArray __LINE__ marr i sm
            stage2do02 marr (i + 1) cry
        | carry /= 0 = stage2final2c marr i carry
        | otherwise = stage2final2 marr i

    stage12short1 !marr !i !carry
        | i - shift < n1 = do
            !x <- indexWordArrayM arr0 i
            !y <- indexWordArrayM arr1 (i - shift)
            !z <- indexWordArrayM arr2 (i - 2 * shift)
            let (# !cry1, !sm1 #) = plusWord2C x y z
            let (# !cry, !sm #) = plusWord2C sm1 cry1 carry
            debugWriteWordArray __LINE__ marr i sm
            stage12short1 marr (i + 1) cry
        | n0 - 2 * shift <= n2 = do { debugPrint __LINE__ $ show (i, (n2, n1, n0), shift) ; return i }
        | otherwise = stage2with20 marr i carry

    stage2with20 marr i carry
        | i - 2 * shift < n2 = do
            !x <- indexWordArrayM arr0 i
            !y <- indexWordArrayM arr2 (i - 2 * shift)
            let (# !cry, !sm #) = plusWord2C x y carry
            debugWriteWordArray __LINE__ marr i sm
            stage2with20 marr (i + 1) cry
        | carry /= 0 = stage2final0c marr i carry
        | otherwise = stage2final0 marr i

    -- Stage 2 with last element.
    stage2final0 !marr !i
        | i < n0 = do
            !x <- indexWordArrayM arr0 i
            debugWriteWordArray __LINE__ marr i x
            stage2final0 marr (i + 1)
        | otherwise = return i

    stage2final0c !marr !i !carry
        | i < n0 = do
            !x <- indexWordArrayM arr0 i
            let (# !cry, !sm #) = plusWord2 x carry
            debugWriteWordArray __LINE__ marr i sm
            if carry == 0
                then stage2final0 marr (i + 1)
                else stage2final0c marr i cry
        | otherwise = do
            debugWriteWordArray __LINE__ marr i carry
            return (i + 1)

    stage2final1 !marr !i
        | i - shift < n1 = do
            !x <- indexWordArrayM arr1 (i - shift)
            debugWriteWordArray __LINE__ marr i x
            stage2final1 marr (i + 1)
        | otherwise = return i

    stage2final1c marr i carry
        | i - shift < n1 = do
            !x <- indexWordArrayM arr2 (i - shift)
            let (# !cry, !sm #) = plusWord2 x carry
            debugWriteWordArray __LINE__ marr i sm
            if carry == 0
                then stage2final1 marr (i + 1)
                else stage2final1c marr i cry
        | otherwise = do
            debugWriteWordArray __LINE__ marr i carry
            return (i + 1)

    stage2final2 !marr !i
        | i - 2 * shift < n2 = do
            !x <- indexWordArrayM arr2 (i - 2 * shift)
            debugWriteWordArray __LINE__ marr i x
            stage2final2 marr (i + 1)
        | otherwise = return i

    stage2final2c !marr !i !carry
        | i - 2 * shift < n2 = do
            !x <- indexWordArrayM arr2 (i - 2 * shift)
            let (# !cry, !sm #) = plusWord2 x carry
            debugWriteWordArray __LINE__ marr i sm
            if carry == 0
                then stage2final2 marr (i + 1)
                else stage2final2c marr i cry
        | otherwise = do
            debugWriteWordArray __LINE__ marr i carry
            return (i + 1)

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

