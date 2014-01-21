{-# LANGUAGE BangPatterns, NoImplicitPrelude, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

#include "MachDeps.h"

import Prelude hiding (Integer)

import Control.Monad.Primitive
import GHC.Word (Word)
import Test.Hspec
-- import Test.Hspec.QuickCheck
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
        let n2 = mkNatural (fromString "0x0")
            n1 = mkNatural (fromString "0x20000000000000002")
            n0 = mkNatural (fromString "0x40000000000000004")
        show (kShiftedAdd 1 n2 n1 n0) `shouldBe` show (kShiftedAddSlow 1 n2 n1 n0)
        show (kShiftedAdd 2 n2 n1 n0) `shouldBe` show (kShiftedAddSlow 2 n2 n1 n0)

    it "kShiftedAdd #4." $ do
        let n2 = mkNatural (fromString "0x10000000000000001")
            n1 = mkNatural (fromString "0x20000000000000002")
            n0 = mkNatural (fromString "0x40000000000000004")
        show (kShiftedAdd 1 n2 n1 n0) `shouldBe` show (kShiftedAddSlow 1 n2 n1 n0)
        show (kShiftedAdd 2 n2 n1 n0) `shouldBe` show (kShiftedAddSlow 2 n2 n1 n0)

{-


    it "kShiftedAdd #4." $ do
        let n2 = mkNatural (fromString "0x10000000000000001")
            n1 = mkNatural (fromString "0x20000000000000002")
            n0 = mkNatural (fromString "0x40000000000000004")
        show (kShiftedAdd 1 n2 n1 n0) `shouldBe` show (kShiftedAddSlow 1 n2 n1 n0)
        show (kShiftedAdd 2 n2 n1 n0) `shouldBe` show (kShiftedAddSlow 2 n2 n1 n0)
-}

{-
    prop "kShiftedAdd works arbitrary overlap." $ \ n2 n1 n0 wshift -> do
        let shift = 1 + ((abs wshift) `mod` 8)
        show (kShiftedAdd shift n2 n1 n0) `shouldBe` show (kShiftedAddSlow shift n2 n1 n0)
-}



kShiftedAddSlow :: Int -> Natural -> Natural -> Natural -> Natural
kShiftedAddSlow shift n2 n1 n0 =
    plusNatural (shiftLNatural n2 (2 * shift * WORD_SIZE_IN_BITS))
        (plusNatural (shiftLNatural n1 (shift * WORD_SIZE_IN_BITS)) n0)


kShiftedAdd :: Int -> Natural -> Natural -> Natural -> Natural
kShiftedAdd !shift !(Natural !n2 !arr2) !(Natural !n1 !arr1) !(Natural !n0 !arr0)
    | shift < 1 = error $ "kShiftedAdd with shift of " ++ show shift
    | otherwise = unsafeInlinePrim $ do
        len <- return . succ $ max n0 (max (n1 + shift) (n2 + 2 * shift))
        debugPrint __LINE__ $ "length is " ++ show len
        !marr <- newWordArray len
        setWordArray marr 0 len (0xaeaeaeaaeaeaeaea :: Word)
        !nlen <- dispatch marr
        if nlen > len
            then error "Bad length"
            else return ()
        !narr <- unsafeFreezeWordArray marr
        returnNatural nlen narr
  where
    dispatch !marr
        | n0 <= shift && n1 <= shift = noOverlapA marr 0
        | otherwise = loop_1 marr 0

    loop_1 !marr !i
        | i < n0 && i < shift = do
            !x <- indexWordArrayM arr0 i
            debugWriteWordArray __LINE__ marr i x
            loop_1 marr (i + 1)
        | otherwise = do
            debugPrint __LINE__ $ "goto loop_1_2 " ++ show i
            loop_1_2 marr i 0

    loop_1_2 !marr !i !carry
        | i < shift = do
            debugWriteWordArray __LINE__ marr i 0
            loop_1_2 marr (i + 1) 0
        | i < n0 && i - shift < n1 && i < 2 * shift = do
            !x <- indexWordArrayM arr0 i
            !y <- indexWordArrayM arr1 (i - shift)
            let (# !cry, !sm #) = plusWord2C x y carry
            debugWriteWordArray __LINE__ marr i sm
            debugPrint __LINE__ $ "Carry is " ++ show cry
            loop_1_2 marr (i + 1) cry
        | i - shift < n1 && i < 2 * shift = do
            !y <- indexWordArrayM arr1 (i - shift)
            let (# !cry, !sm #) = plusWord2 y carry
            debugWriteWordArray __LINE__ marr i sm
            loop_1_2 marr (i + 1) cry


        | otherwise = do
            debugPrint __LINE__ $ "goto loop_2 " ++ show i
            loop_2 marr i carry

    loop_2 marr !i !carry
        | i - shift < n1 = do
            !y <- indexWordArrayM arr1 (i - shift)
            let (# !cry, !sm #) = plusWord2 y carry
            debugWriteWordArray __LINE__ marr i sm
            loop_2 marr (i + 1) cry

        | i - 2 * shift < n2 = do
            debugPrint __LINE__ $ "goto loop_pre_3 " ++ show i ++ " " ++ show (n2, n1, n0) ++ " " ++ show carry
            loop_pre_3 marr i carry

        | otherwise  = do
            debugPrint __LINE__ $ "goto loop_pre_3 " ++ show i ++ " " ++ show (n2, n1, n0) ++ " " ++ show carry
            loop_pre_3 marr i carry

    loop_pre_3 !marr !i !carry
        | i < 2 * shift = do
            debugWriteWordArray __LINE__ marr i carry
            loop_pre_3 marr (i + 1) 0
        | otherwise = do
            debugPrint __LINE__ $ "goto loop_3 " ++ show i
            loop_3 marr i carry

    loop_3 !marr !i !carry
        | i - 2 * shift < n2 = do
            !z <- indexWordArrayM arr2 (i - 2 * shift)
            let (# !cry, !sm #) = plusWord2 z carry
            debugWriteWordArray __LINE__ marr i sm
            loop_3 marr (i + 1) cry
        | carry /= 0 = do
            debugWriteWordArray __LINE__ marr i carry
            return (i + 1)
        | otherwise = do
            debugPrint __LINE__ $ "returning length " ++ show i
            return i

    noOverlapA !marr !i
        | i < n0 = do
            !x <- indexWordArrayM arr0 i
            debugWriteWordArray __LINE__ marr i x
            noOverlapA marr (i + 1)
        | i < shift = noOverlapB marr i
        | otherwise = noOverlapC marr i

    noOverlapB !marr !i
        | i < shift = do
            debugWriteWordArray __LINE__ marr i 0
            noOverlapB marr (i + 1)
        | otherwise = noOverlapC marr i

    noOverlapC !marr !i
        | i - shift < n1 = do
            !x <- indexWordArrayM arr1 (i - shift)
            debugWriteWordArray __LINE__ marr i x
            noOverlapC marr (i + 1)
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



{-
                      1
                     2
                    4

                    111
                   222
                  444

                    111
                     2
                   44

                      1
                  2222
                   44

-}


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

