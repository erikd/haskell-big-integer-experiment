{-# LANGUAGE BangPatterns, NoImplicitPrelude, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

#include "MachDeps.h"

import Prelude hiding (Integer)

import Control.Monad (forM_)
import Control.Monad.Primitive
import Data.Primitive
import GHC.Word (Word)
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck.Arbitrary
import Unsafe.Coerce (unsafeCoerce)

import New3.GHC.Integer.Prim
import New3.GHC.Integer.Type
import New3.GHC.Integer.WordArray
import New3.Integer ()

import Check.Helpers

main :: IO ()
main = hspec $ describe "Karatsuba Shifted Add:" testKaratsuba


testKaratsuba :: Spec
testKaratsuba = do
    it "A kSplit Natural can be recombined correctly." $ forM_ [0 .. 10] $ \shift ->
        let nat = mkNatural [ 1, 0, 4 * 2, 0, 16 * 3, 0, 64 * 4, 0, 256 * 5, 0, 256 * 4 * 6 ]
            (hi, lo) = kSplit nat shift
        in show (plusNatural (shiftLNatural hi (shift * WORD_SIZE_IN_BITS)) lo) `shouldBe` show nat
    prop "kShiftedAdd works without overlap." $ \ n2 n1 n0 -> do
        let shift = max (lengthNatrual n1) (lengthNatrual n0)
        show (kShiftedAdd shift n2 n1 n0) `shouldBe` show (kShiftedAddSlow shift n2 n1 n0)
        show (kShiftedAdd (shift + 1) n2 n1 n0) `shouldBe` show (kShiftedAddSlow (shift + 1) n2 n1 n0)
    prop "kShiftedAdd works with overlap of lower two." $ \ n1 n0@(Natural _ arr) wshift -> do
        let n2 = Natural 0 arr
            shift = 1 + ((abs wshift) `mod` 8)
        show (kShiftedAdd shift n2 n1 n0) `shouldBe` show (kShiftedAddSlow shift n2 n1 n0)

    prop "kShiftedAdd works with overlap of upper two." $ \ n1@(Natural _ arr) n0 wshift -> do
        let n2 = Natural 0 arr
            shift = 1 + ((abs wshift) `mod` 8)
        show (kShiftedAdd shift n2 n1 n0) `shouldBe` show (kShiftedAddSlow shift n2 n1 n0)

    it "kShiftedAdd works arbitrary overlap #1." $ do
        let n2 = mkNatural (fromString "0x1")
            n1 = mkNatural (fromString "0x0")
            n0 = mkNatural (fromString "0x0")
        show (kShiftedAdd 1 n2 n1 n0) `shouldBe` show (kShiftedAddSlow 1 n2 n1 n0)
        show (kShiftedAdd 2 n2 n1 n0) `shouldBe` show (kShiftedAddSlow 2 n2 n1 n0)

    it "kShiftedAdd works arbitrary overlap #2." $ do
        let n2 = mkNatural (fromString "0x1")
            n1 = mkNatural (fromString "0x20")
            n0 = mkNatural (fromString "0x300")
        show (kShiftedAdd 1 n2 n1 n0) `shouldBe` show (kShiftedAddSlow 1 n2 n1 n0)
        show (kShiftedAdd 2 n2 n1 n0) `shouldBe` show (kShiftedAddSlow 2 n2 n1 n0)

    it "kShiftedAdd works arbitrary overlap #3." $ do
        let n2 = mkNatural (fromString "0x0")
            n1 = mkNatural (fromString "0x200000000000000020")
            n0 = mkNatural (fromString "0x3000000000000000300")
        show (kShiftedAdd 1 n2 n1 n0) `shouldBe` show (kShiftedAddSlow 1 n2 n1 n0)
        show (kShiftedAdd 2 n2 n1 n0) `shouldBe` show (kShiftedAddSlow 2 n2 n1 n0)

#define DEBUG 1

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
        !nlen <- loop_1 marr 0
        if nlen > len
            then error "Bad length"
            else return ()
        !narr <- unsafeFreezeWordArray marr
        returnNatural nlen narr
  where
    loop_1 !marr !i
        | i >= n0 = do
            debugPrint __LINE__ $ "goto loop_pre_2 " ++ show i
            loop_pre_2 marr i
        | i < shift = do
            !x <- indexWordArrayM arr0 i
            debugWriteWordArray __LINE__ marr i x
            loop_1 marr (i + 1)
        | otherwise = do
            debugPrint __LINE__ $ "goto loop_1_2 " ++ show i
            loop_1_2 marr i 0

    loop_1_2 !marr !i !carry
        | i < n0 && i - shift < n1 = do
            !x <- indexWordArrayM arr0 i
            !y <- indexWordArrayM arr1 (i - shift)
            let (# !cry, !sm #) = plusWord2C x y carry
            debugWriteWordArray __LINE__ marr i sm
            debugPrint __LINE__ $ "Carry is " ++ show cry
            loop_1_2 marr (i + 1) cry
        | i - shift < n1 = do
            !y <- indexWordArrayM arr1 (i - shift)
            let (# !cry, !sm #) = plusWord2 y carry
            debugWriteWordArray __LINE__ marr i sm
            loop_1_2 marr (i + 1) cry


{-
        | i < n0 && i - 2 * shift < n2 = do
            !y <- indexWordArrayM arr1 (i - shift)
            !z <- indexWordArrayM arr2 (i - 2 * shift)
            let (# !cry, !sm #) = plusWord2C y z carry
            debugWriteWordArray __LINE__ marr i sm
            loop_1_2 marr (i + 1) cry
-}


        | i < n0 = do
            !x <- indexWordArrayM arr0 i
            let (# !cry, !sm #) = plusWord2 x carry
            debugWriteWordArray __LINE__ marr i sm
            loop_1_2 marr (i + 1) cry
        | otherwise = do
            debugPrint __LINE__ $ "goto loop_2 " ++ show i
            loop_2 marr i carry

    loop_pre_2 !marr !i
        | i < shift = do
            debugWriteWordArray __LINE__ marr i 0
            loop_pre_2 marr (i + 1)
        | otherwise = do
            debugPrint __LINE__ $ "goto loop_2 " ++ show i
            loop_2 marr i 0

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


kSplit :: Natural -> Int -> (Natural, Natural)
kSplit nat@(Natural n arr) i
    | i <= 0 = ( nat, Natural 0 arr)
    | i > n = ( Natural 0 arr, nat )
    | otherwise = ( sliceOfNatural nat i (n - i), sliceOfNatural nat 0 i )


-- | sliceOfNatural : Potentially dangerous. The returned Natural should only
-- be accessed while the Natural it is a slice of is in scope.
sliceOfNatural :: Natural -> Int -> Int -> Natural
sliceOfNatural !(Natural !n !(WA !arr)) !start !count
    | count < 0 = Natural 0 (WA arr)
    | start + count > n = Natural 0 (WA arr)
    | start == 0 = Natural count (WA arr)
    | otherwise =
        let !addr = unsafeCoerce arr :: Addr
            !newaddr = plusAddr addr ((max 0 start) * sizeOf (0:: Word))
        in Natural (min (n - start) count) (unsafeCoerce newaddr)


lengthNatrual :: Natural -> Int
lengthNatrual (Natural n _) = n

instance Arbitrary Natural where
    arbitrary = do
        wrds <- fmap (take 10 . positive32bits) arbitrary
        return $ mkNatural wrds

    shrink (Natural n arr) =
        map (\x -> Natural x arr) $ [ 0 .. (n - 1) ]

