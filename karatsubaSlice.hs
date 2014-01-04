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

import New3.GHC.Integer.Type
import New3.GHC.Integer.WordArray
import New3.Integer ()

import Check.Helpers

main :: IO ()
main = hspec $ describe "Karatsuba functions:" testKaratsuba


testKaratsuba :: Spec
testKaratsuba = do
    it "A kSplit Natural can be recombined correctly." $ forM_ [1..10] $ \shift ->
        let nat = mkNatural [ 1, 0, 4 * 2, 0, 16 * 3, 0, 64 * 4, 0, 256 * 5, 0, 256 * 4 * 6 ]
            (hi, lo) = kSplit nat shift
        in show (plusNatural (shiftLNatural hi (shift * WORD_SIZE_IN_BITS)) lo) `shouldBe` show nat
    prop "kShiftedAdd works without overlap." $ \n1 n2 n3 -> do
        let shift = max (lengthNatrual n2) (lengthNatrual n3)
        show (kShiftedAdd shift n1 n2 n3) `shouldBe` show (kShiftedAddSlow shift n1 n2 n3)
        show (kShiftedAdd (shift + 1) n1 n2 n3) `shouldBe` show (kShiftedAddSlow (shift + 1) n1 n2 n3)


kShiftedAddSlow :: Int -> Natural -> Natural -> Natural -> Natural
kShiftedAddSlow shift n1 n2 n3 =
    plusNatural (shiftLNatural n1 (2 * shift * WORD_SIZE_IN_BITS))
        (plusNatural (shiftLNatural n2 (shift * WORD_SIZE_IN_BITS)) n3)

kShiftedAdd :: Int -> Natural -> Natural -> Natural -> Natural
kShiftedAdd !shift !(Natural !n3 !arr3) !(Natural !n2 !arr2) !(Natural !n1 !arr1)
    | shift < 1 = error $ "kShiftedAdd with shift of " ++ show shift
    | otherwise = unsafeInlinePrim $ do
        !marr <- newWordArray (succ (n3 + 2 * shift))
        !nlen <- loop_1 marr 0
        !narr <- unsafeFreezeWordArray marr
        returnNatural nlen narr
  where
    loop_1 !marr !i
        | i >= n1 = loop_pre_2 marr i
        -- | i = shift = loop_1_2 marr i carry
        | otherwise = do
            !x <- indexWordArrayM arr1 i
            debugWriteWordArray __LINE__ marr i x
            loop_1 marr (i + 1)

    loop_pre_2 !marr !i
        | i < shift = do
            debugWriteWordArray __LINE__ marr i 0
            loop_pre_2 marr (i + 1)
        | otherwise = loop_2 marr i

    loop_2 marr i
        | i - shift >= n2 = loop_pre_3 marr i
        | otherwise  = do
            !x <- indexWordArrayM arr2 (i - shift)
            debugWriteWordArray __LINE__ marr i x
            loop_2 marr (i + 1)

    loop_pre_3 !marr !i
        | i < 2 * shift = do
            debugWriteWordArray __LINE__ marr i 0
            loop_pre_3 marr (i + 1)
        | otherwise = loop_3 marr i

    loop_3 !marr !i
        | i - 2 * shift >= n3 = return i
        | otherwise = do
            !x <- indexWordArrayM arr3 (i - 2 * shift)
            debugWriteWordArray __LINE__ marr i x
            loop_3 marr (i + 1)

debugWriteWordArray :: Int -> MutableWordArray IO -> Int -> Word -> IO ()
#if 1
debugWriteWordArray _ = writeWordArray
#else
debugWriteWordArray line marr i x = do
    putStrLn $ show line ++ " : writing " ++ hexShowW x ++ " at " ++ show i
    writeWordArray marr i x
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
