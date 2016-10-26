{-# LANGUAGE BangPatterns, CPP, NoImplicitPrelude, ScopedTypeVariables, UnboxedTuples #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

#include "MachDeps.h"

import Prelude hiding (Integer)

import Control.Monad (forM_, when)
import GHC.Word (Word)
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen

import Common.GHC.Integer.Prim
import Common.GHC.Integer.StrictPrim
import Common.GHC.Integer.WordArray
import New3.GHC.Integer.Natural
import New3.GHC.Integer.Type
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

    it "kShiftedAdd #11." $ do
        let n2 = mkNatural (fromString "0x1")
            n1 = mkNatural (fromString "0x1000000000000000100000000000000010000000000000001")
            n0 = mkNatural (fromString "0x200000000000000020000000000000002")
        show (kShiftedAdd 1 n2 n1 n0) `shouldBe` show (kShiftedAddSlow 1 n2 n1 n0)
        show (kShiftedAdd 2 n2 n1 n0) `shouldBe` show (kShiftedAddSlow 2 n2 n1 n0)
        show (kShiftedAdd 3 n2 n1 n0) `shouldBe` show (kShiftedAddSlow 3 n2 n1 n0)

    it "kShiftedAdd #12." $ do
        let n2 = mkNatural (fromString "0x1")
            n1 = mkNatural (fromString "0x2")
            n0 = mkNatural (fromString "0x40000000000000004000000000000000400000000000000040000000000000004")
        show (kShiftedAdd 1 n2 n1 n0) `shouldBe` show (kShiftedAddSlow 1 n2 n1 n0)
        show (kShiftedAdd 2 n2 n1 n0) `shouldBe` show (kShiftedAddSlow 2 n2 n1 n0)
        show (kShiftedAdd 3 n2 n1 n0) `shouldBe` show (kShiftedAddSlow 3 n2 n1 n0)

    it "kShiftedAdd #13." $ do
        let n2 = mkNatural (fromString "0x1")
            n1 = mkNatural (fromString "0x20000000000000002")
            n0 = mkNatural (fromString "0x400000000000000400000000000000040000000000000004")
        show (kShiftedAdd 1 n2 n1 n0) `shouldBe` show (kShiftedAddSlow 1 n2 n1 n0)
        show (kShiftedAdd 2 n2 n1 n0) `shouldBe` show (kShiftedAddSlow 2 n2 n1 n0)
        show (kShiftedAdd 3 n2 n1 n0) `shouldBe` show (kShiftedAddSlow 3 n2 n1 n0)

    it "kShiftedAdd #14." $ do
        let n2 = mkNatural (fromString "0x8000000040000000")
            n1 = mkNatural (fromString "0x800000002000000080000000200000008000000020000000")
            n0 = mkNatural (fromString "0x80000000100000008000000010000000800000001000000080000000100000008000000010000000")
        show (kShiftedAdd 1 n2 n1 n0) `shouldBe` show (kShiftedAddSlow 1 n2 n1 n0)
        show (kShiftedAdd 2 n2 n1 n0) `shouldBe` show (kShiftedAddSlow 2 n2 n1 n0)
        show (kShiftedAdd 3 n2 n1 n0) `shouldBe` show (kShiftedAddSlow 3 n2 n1 n0)
        show (kShiftedAdd 4 n2 n1 n0) `shouldBe` show (kShiftedAddSlow 4 n2 n1 n0)

    it "kShiftedAdd #15." $ do
        let n2 = mkNatural (fromString "0x8000000040000000")
            n1 = mkNatural (fromString "0x80000000200000008000000020000000800000002000000080000000200000008000000020000000")
            n0 = mkNatural (fromString "0x800000001000000080000000100000008000000010000000")
        forM_ [1 .. 6] $ \shift ->
            show (kShiftedAdd shift n2 n1 n0) `shouldBe` show (kShiftedAddSlow shift n2 n1 n0)

    it "kShiftedAdd #16." $ do
        let n2 = mkNatural (fromString "0x1")
            n1 = mkNatural (fromString "0x58000000b800000440000003e00000048000000d8000006e")
            n0 = mkNatural (fromString "0xc00000054000000f000000218000002c")
        forM_ [1 .. 4] $ \shift ->
            show (kShiftedAdd shift n2 n1 n0) `shouldBe` show (kShiftedAddSlow shift n2 n1 n0)

    it "kShiftedAdd #17." $ do
        let n2 = mkNatural (fromString "0x1")
            n1 = mkNatural (fromString "0x2000000000000000200000000000000020000000000000002")
            n0 = mkNatural (fromString "0x1000000040000000000000004000000000000000400000000000000040000000000000004")
        forM_ [1 .. 6] $ \shift ->
            show (kShiftedAdd shift n2 n1 n0) `shouldBe` show (kShiftedAddSlow shift n2 n1 n0)

    it "kShiftedAdd #18." $ do
        let n2 = mkNatural (fromString "0x1")
            n1 = mkNatural (fromString "0x200000000000000020000000000000002")
            n0 = mkNatural (fromString "0x48000000000000004000000000000000400000000000000040000000000000004")
        forM_ [1 .. 4] $ \shift ->
            show (kShiftedAdd shift n2 n1 n0) `shouldBe` show (kShiftedAddSlow shift n2 n1 n0)

    it "kShiftedAdd #19." $ do
        let n2 = mkNatural (fromString "0x6c00000000000000f0000000000000001000000000000001")
            n1 = mkNatural (fromString "0x3c0000000000000020000000000000002000000000000002")
            n0 = mkNatural (fromString "0x0")
        forM_ [1 .. 4] $ \shift ->
            show (kShiftedAdd shift n2 n1 n0) `shouldBe` show (kShiftedAddSlow shift n2 n1 n0)

    it "kShiftedAdd #20." $ do
        let n2 = mkNatural (fromString "0x3d0000004a00000088000000a00000024")
            n1 = mkNatural (fromString "0xf4000001a00000006")
            n0 = mkNatural (fromString "0xf000000300000009800000108000003f")
        forM_ [1 .. 4] $ \shift ->
            show (kShiftedAdd shift n2 n1 n0) `shouldBe` show (kShiftedAddSlow shift n2 n1 n0)

    it "kShiftedAdd #21." $ do
        let n2 = mkNatural (fromString "0x18000000000000001")
            n1 = mkNatural (fromString "0x1000000002")
            n0 = mkNatural (fromString "0x1300000000000000040000000000000004000000000000004")
        forM_ [1 .. 2] $ \shift ->
            show (kShiftedAdd shift n2 n1 n0) `shouldBe` show (kShiftedAddSlow shift n2 n1 n0)

    it "kShiftedAdd #22." $ do
        let n2 = mkNatural (fromString "0x0")
            n1 = mkNatural (fromString "0x8000000000000002")
            n0 = mkNatural (fromString "0x4800000000000000400000000000000040000000000000004")
        forM_ [1 .. 4] $ \shift ->
            show (kShiftedAdd shift n2 n1 n0) `shouldBe` show (kShiftedAddSlow shift n2 n1 n0)

    it "kShiftedAdd #23." $ do
        let n2 = mkNatural (fromString "0xc000000280000002")
            n1 = mkNatural (fromString "0xc000000200000004")
            n0 = mkNatural (fromString "0x40000000600000004000000200000002")
        forM_ [1 .. 4] $ \shift ->
            show (kShiftedAdd shift n2 n1 n0) `shouldBe` show (kShiftedAddSlow shift n2 n1 n0)


    when True .
        modifyMaxSuccess (const 10000) .
            prop "kShiftedAdd QuickChecked." $ \ n2 n1 n0 -> do
                let maxShift = 1 + max (lengthNatrual n1) (lengthNatrual n0)
                forM_ [1 .. maxShift] $ \shift ->
                    show (kShiftedAdd shift n2 n1 n0) `shouldBe` show (kShiftedAddSlow shift n2 n1 n0)




kShiftedAddSlow :: Int -> Natural -> Natural -> Natural -> Natural
kShiftedAddSlow shift n2 n1 n0 =
    plusNatural (shiftLNatural n2 (2 * shift * WORD_SIZE_IN_BITS))
        (plusNatural (shiftLNatural n1 (shift * WORD_SIZE_IN_BITS)) n0)


kShiftedAdd :: Int -> Natural -> Natural -> Natural -> Natural
kShiftedAdd !shift !(Natural !n2 !arr2) !(Natural !n1 !arr1) !(Natural !n0 !arr0)
    | shift < 1 = error $ "kShiftedAdd with shift of " ++ show shift
    | otherwise = runStrictPrim $ do
        len <- pure . succ $ max n0 (max (n1 + shift) (n2 + 2 * shift))
        debugPrint __LINE__ $ "length is " ++ show len ++ " " ++ show (0 :: Int, (n2, n1, n0), shift)
        !marr <- newWordArray len
        setWordArray marr 0 len (0xaeaeaeaaeaeaeaea :: Word)
        !nlen <- start marr
        if nlen > len
            then error "Bad length"
            else pure ()
        !narr <- unsafeFreezeWordArray marr
        pure $! Natural nlen narr
  where
    start !marr
        | n0 <= shift = stage0short0 marr 0
        | otherwise = stage0copy  marr 0

    -- Stage 0.
    stage0short0 !marr !i
        | i < n0 = do
            !x <- indexWordArrayM arr0 i
            debugWriteWordArrayLocal __LINE__ marr i x
            stage0short0 marr (i + 1)
        | otherwise = stage0short0fill marr i

    stage0copy  !marr !i
        | i < shift = do
            !x <- indexWordArrayM arr0 i
            debugWriteWordArrayLocal __LINE__ marr i x
            stage0copy  marr (i + 1)
        | n0 > 2 * shift =
            if n1 > shift
                then stage1long01 marr i 0
                else stage1do10 marr i 0
        | n0 > shift =
            if n1 > shift
                then stage1do01 marr i 0
                else if n0 - shift <= n1
                        then stage1do01 marr i 0
                        else stage1do10 marr i 0
        | n1 > shift = stage1copy1 marr i
        | otherwise = stage1do1 marr i

    stage0short0fill !marr !i
        | i < shift = do
            debugWriteWordArrayLocal __LINE__ marr i 0
            stage0short0fill marr (i + 1)
        | n1 <= shift = stage1do1 marr i
        | otherwise = stage1copy1 marr i

    -- Stage 1.
    stage1do0 !marr !i
        | i < n0 = do
            !x <- indexWordArrayM arr0 i
            debugWriteWordArrayLocal __LINE__ marr i x
            stage1do0 marr (i + 1)
        | otherwise = stage1fill marr i

    stage1do0c !marr !i !carry
        | i < n0 = do
            !x <- indexWordArrayM arr0 i
            let (# !cry, !sm #) = plusWord2 x carry
            debugWriteWordArrayLocal __LINE__ marr i sm
            if cry /= 0
                then stage1do0c marr (i + 1) cry
                else stage1do0 marr (i + 1)
        | carry /= 0 = do
            debugWriteWordArrayLocal __LINE__ marr i carry
            stage1fill marr (i + 1)
        | otherwise = stage1fill marr i

    stage1do1 !marr !i
        | i - shift < n1 = do
            !x <- indexWordArrayM arr1 (i - shift)
            debugWriteWordArrayLocal __LINE__ marr i x
            stage1do1 marr (i + 1)
        | i < 2 * shift = stage1fill marr i
        | n1 - shift < n2 = stage2do12 marr i 0
        | otherwise = stage2do21 marr i 0

    stage1copy1 !marr !i
        | i < 2 * shift = do
            !x <- indexWordArrayM arr1 (i - shift)
            debugWriteWordArrayLocal __LINE__ marr i x
            stage1copy1 marr (i + 1)
        | n1 - shift < n2 = stage2do12 marr i 0
        | otherwise = stage2do21 marr i 0

    stage1do01 !marr !i !carry
        | i < n0 = do
            !x <- indexWordArrayM arr0 i
            !y <- indexWordArrayM arr1 (i - shift)
            let (# !cry, !sm #) = plusWord3 x y carry
            debugWriteWordArrayLocal __LINE__ marr i sm
            stage1do01 marr (i + 1) cry
        | n1 >= shift =
            if carry == 0
                then stage1copy1 marr i
                else stage1add1 marr i carry
        | otherwise = stage1long1c marr i carry

    stage1do10 !marr !i !carry
        | i - shift < n1 = do
            !x <- indexWordArrayM arr0 i
            !y <- indexWordArrayM arr1 (i - shift)
            let (# !cry, !sm #) = plusWord3 x y carry
            debugWriteWordArrayLocal __LINE__ marr i sm
            stage1do10 marr (i + 1) cry
        | n0 > 2 * shift =
            if carry /= 0
                then stage1long0c marr i carry
                else stage1long0 marr i
        | carry /= 0 = stage1do0c marr i carry
        | otherwise = stage1do0 marr i

    stage1long0c !marr !i !carry
        | i < 2 * shift = do
            !x <- indexWordArrayM arr0 i
            let (# !cry, !sm #) = plusWord2 x carry
            debugWriteWordArrayLocal __LINE__ marr i sm
            if cry /= 0
                then stage1long0c marr (i + 1) cry
                else stage1long0 marr (i + 1)
        | n0 - 2 * shift <= n2 = stage2do02 marr i carry
        | otherwise = stage2do20 marr i carry

    stage1long0 !marr !i
        | i < 2 * shift = do
            !x <- indexWordArrayM arr0 i
            debugWriteWordArrayLocal __LINE__ marr i x
            stage1long0 marr (i + 1)
        | n0 - 2 * shift <= n2 = stage2do02 marr i 0
        | otherwise = stage2do20 marr i 0

    stage1long1c !marr !i !carry
        | i - shift < n1 = do
            !x <- indexWordArrayM arr1 (i - shift)
            let (# !cry, !sm #) = plusWord2 x carry
            debugWriteWordArrayLocal __LINE__ marr i sm
            stage1long1c marr (i + 1) cry
        | carry /= 0 = do
            debugPrint __LINE__ $ show (i, n1, shift)
            debugWriteWordArrayLocal __LINE__ marr i carry
            stage1fill marr (i + 1)
        | otherwise = stage1fill marr i

    stage1fill !marr !i
        | i < 2 * shift = do
            debugWriteWordArrayLocal __LINE__ marr i 0
            stage1fill marr (i + 1)
        | otherwise = stage2final2 marr i

    stage1add1 !marr !i !carry
        | i < 2 * shift = do
            !x <- indexWordArrayM arr1 (i - shift)
            let (# !cry, !sm #) = plusWord2 x carry
            debugWriteWordArrayLocal __LINE__ marr i sm
            if cry == 0
                then stage1copy1 marr (i + 1)
                else stage1add1 marr (i + 1) cry
        | n1 - shift > n2 = stage2do21 marr i carry
        | otherwise = stage2do12 marr i carry

    stage1long01 !marr !i !carry
        | i < 2 * shift = do
            !x <- indexWordArrayM arr0 i
            !y <- indexWordArrayM arr1 (i - shift)
            let (# !cry, !sm #) = plusWord3 x y carry
            debugWriteWordArrayLocal __LINE__ marr i sm
            stage1long01 marr (i + 1) cry
        | otherwise = stage2have012 marr i carry

    -- Stage 2.
    stage2have012 !marr !i !carry =
        case (# n0 - shift <= n1, n0 - 2 * shift <= n2, n1 - shift <= n2 #) of
            (# True, True, True #)      -> stage2do012 marr i carry
            (# True, True, False #)     -> stage2do021 marr i carry
            (# True, False, _ #)        -> stage2do201 marr i carry
            (# False, False, True #)    -> stage2do120 marr i carry
            (# False, False, _ #)       -> stage2do210 marr i carry
            (# False, True, _ #)        -> stage2do102 marr i carry

    stage2do012 !marr !i !carry
        | i < n0 = do
            !x <- indexWordArrayM arr0 i
            !y <- indexWordArrayM arr1 (i - shift)
            !z <- indexWordArrayM arr2 (i - 2 * shift)
            let (# !cry, !sm #) = plusWord4 x y z carry
            debugWriteWordArrayLocal __LINE__ marr i sm
            stage2do012 marr (i + 1) cry
        | otherwise = stage2do12 marr i carry

    stage2do021 !marr !i !carry
        | i < n0 = do
            !x <- indexWordArrayM arr0 i
            !y <- indexWordArrayM arr1 (i - shift)
            !z <- indexWordArrayM arr2 (i - 2 * shift)
            let (# !cry, !sm #) = plusWord4 x y z carry
            debugWriteWordArrayLocal __LINE__ marr i sm
            stage2do021 marr (i + 1) cry
        | otherwise = stage2do21 marr i carry

    stage2do102 !marr !i !carry
        | i - shift < n1 = do
            !x <- indexWordArrayM arr0 i
            !y <- indexWordArrayM arr1 (i - shift)
            !z <- indexWordArrayM arr2 (i - 2 * shift)
            let (# !cry, !sm #) = plusWord4 x y z carry
            debugWriteWordArrayLocal __LINE__ marr i sm
            stage2do102 marr (i + 1) cry
        | otherwise = stage2do02 marr i carry

    stage2do120 !marr !i !carry
        | i - shift < n1 = do
            !x <- indexWordArrayM arr0 i
            !y <- indexWordArrayM arr1 (i - shift)
            !z <- indexWordArrayM arr2 (i - 2 * shift)
            let (# !cry, !sm #) = plusWord4 x y z carry
            debugWriteWordArrayLocal __LINE__ marr i sm
            stage2do120 marr (i + 1) cry
        | otherwise = stage2do20 marr i carry

    stage2do201 !marr !i !carry
        | i - 2 * shift < n2 = do
            !x <- indexWordArrayM arr0 i
            !y <- indexWordArrayM arr1 (i - shift)
            !z <- indexWordArrayM arr2 (i - 2 * shift)
            let (# !cry, !sm #) = plusWord4 x y z carry
            debugWriteWordArrayLocal __LINE__ marr i sm
            stage2do201 marr (i + 1) cry
        | otherwise = stage2do01 marr i carry

    stage2do210 !marr !i !carry
        | i - 2 * shift < n2 = do
            !x <- indexWordArrayM arr0 i
            !y <- indexWordArrayM arr1 (i - shift)
            !z <- indexWordArrayM arr2 (i - 2 * shift)
            let (# !cry, !sm #) = plusWord4 x y z carry
            debugWriteWordArrayLocal __LINE__ marr i sm
            stage2do210 marr (i + 1) cry
        | otherwise = stage2do10 marr i carry

    -- Stage 2, two elements.
    stage2do01 !marr !i !carry
        | i < n0 = do
            !x <- indexWordArrayM arr0 i
            !y <- indexWordArrayM arr1 (i - shift)
            let (# !cry, !sm #) = plusWord3 x y carry
            debugWriteWordArrayLocal __LINE__ marr i sm
            stage2do01 marr (i + 1) cry
        | carry /= 0 = stage2final1c marr i carry
        | otherwise = stage2final1 marr i

    stage2do02 !marr !i !carry
        | i < n0 = do
            !x <- indexWordArrayM arr0 i
            !y <- indexWordArrayM arr2 (i - 2 * shift)
            let (# !cry, !sm #) = plusWord3 x y carry
            debugWriteWordArrayLocal __LINE__ marr i sm
            stage2do02 marr (i + 1) cry
        | carry /= 0 = stage2final2c marr i carry
        | otherwise = stage2final2 marr i

    stage2do10 !marr !i !carry
        | i - shift < n1 = do
            !x <- indexWordArrayM arr0 i
            !y <- indexWordArrayM arr1 (i - shift)
            let (# !cry, !sm #) = plusWord3 x y carry
            debugWriteWordArrayLocal __LINE__ marr i sm
            stage2do10 marr (i + 1) cry
        | carry == 0 = stage2final0 marr i
        | otherwise = stage2final0c marr i carry

    stage2do12 !marr !i !carry
        | i - shift < n1 = do
            !x <- indexWordArrayM arr1 (i - shift)
            !y <- indexWordArrayM arr2 (i - 2 * shift)
            let (# !cry, !sm #) = plusWord3 x y carry
            debugWriteWordArrayLocal __LINE__ marr i sm
            stage2do12 marr (i + 1) cry
        | carry /= 0 = stage2final2c marr i carry
        | otherwise = stage2final2 marr i

    stage2do20 !marr !i !carry
        | i - 2 * shift < n2 = do
            !x <- indexWordArrayM arr0 i
            !y <- indexWordArrayM arr2 (i - 2 * shift)
            let (# !cry, !sm #) = plusWord3 x y carry
            debugWriteWordArrayLocal __LINE__ marr i sm
            stage2do20 marr (i + 1) cry
        | carry /= 0 = stage2final0c marr i carry
        | otherwise = stage2final0 marr i

    stage2do21 !marr !i !carry
        | i - 2 * shift < n2 = do
            !x <- indexWordArrayM arr1 (i - shift)
            !y <- indexWordArrayM arr2 (i - 2 * shift)
            let (# !cry, !sm #) = plusWord3 x y carry
            debugWriteWordArrayLocal __LINE__ marr i sm
            stage2do21 marr (i + 1) cry
        | carry == 0 = stage2final1 marr i
        | otherwise = stage2final1c marr i carry

    -- Stage 2 with last element.
    stage2final0 !marr !i
        | i < n0 = do
            !x <- indexWordArrayM arr0 i
            debugWriteWordArrayLocal __LINE__ marr i x
            stage2final0 marr (i + 1)
        | otherwise = pure i

    stage2final0c !marr !i !carry
        | i < n0 = do
            !x <- indexWordArrayM arr0 i
            let (# !cry, !sm #) = plusWord2 x carry
            debugWriteWordArrayLocal __LINE__ marr i sm
            if carry == 0
                then stage2final0 marr (i + 1)
                else stage2final0c marr (i + 1) cry
        | carry /= 0 = do
            debugWriteWordArrayLocal __LINE__ marr i carry
            pure (i + 1)
        | otherwise = pure i

    stage2final1 !marr !i
        | i - shift < n1 = do
            !x <- indexWordArrayM arr1 (i - shift)
            debugWriteWordArrayLocal __LINE__ marr i x
            stage2final1 marr (i + 1)
        | otherwise = pure i

    stage2final1c !marr !i !carry
        | i - shift < n1 = do
            !x <- indexWordArrayM arr1 (i - shift)
            let (# !cry, !sm #) = plusWord2 x carry
            debugWriteWordArrayLocal __LINE__ marr i sm
            if carry == 0
                then stage2final1 marr (i + 1)
                else stage2final1c marr (i + 1) cry
        | carry /= 0 = do
            debugWriteWordArrayLocal __LINE__ marr i carry
            pure (i + 1)
        | otherwise = pure i

    stage2final2 !marr !i
        | i - 2 * shift < n2 = do
            !x <- indexWordArrayM arr2 (i - 2 * shift)
            debugWriteWordArrayLocal __LINE__ marr i x
            stage2final2 marr (i + 1)
        | otherwise = pure i

    stage2final2c !marr !i !carry
        | i - 2 * shift < n2 = do
            !x <- indexWordArrayM arr2 (i - 2 * shift)
            let (# !cry, !sm #) = plusWord2 x carry
            debugWriteWordArrayLocal __LINE__ marr i sm
            if carry == 0
                then stage2final2 marr (i + 1)
                else stage2final2c marr (i + 1) cry
        | otherwise = do
            debugWriteWordArrayLocal __LINE__ marr i carry
            pure (i + 1)

--------------------------------------------------------------------------------

lengthNatrual :: Natural -> Int
lengthNatrual (Natural n _) = n


instance Arbitrary Natural where
    arbitrary = do
        randLen <- choose (0, 20)
        wrds <- vectorOf randLen $ choose (0, 0x7fffffff)
        pure $ mkNatural wrds

    -- shrink (Natural n arr) = map (\x -> Natural x arr) $ [ 0 .. (n - 1) ]
    shrink _ = []

debugWriteWordArrayLocal :: Int -> MutableWordArray (StrictPrim s) -> Int -> Word -> StrictPrim s ()
# if 0
debugWriteWordArrayLocal line marr i x = do
    debugPrint line $ "writing " ++ hexShowW x ++ " at " ++ show i
    writeWordArray marr i x
#else
debugWriteWordArrayLocal _ marr i x = writeWordArray marr i x
#endif

debugPrint :: Int -> String -> StrictPrim s ()
#if 0
debugPrint line s = trace (show line ++ " : " ++ s) $ pure ()
#else
debugPrint _ _ = pure ()
#endif
