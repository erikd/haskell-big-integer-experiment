{-# LANGUAGE BangPatterns, NoImplicitPrelude, ScopedTypeVariables #-}

#include "MachDeps.h"

import Prelude hiding (Integer)

import Control.Monad (forM_)
import Control.Monad.Primitive
import Data.Primitive
import GHC.Word (Word)
import Test.Hspec
-- import Test.Hspec.QuickCheck
-- import Test.QuickCheck.Arbitrary
import Unsafe.Coerce (unsafeCoerce)

import New3.GHC.Integer.Type
import New3.GHC.Integer.WordArray
import New3.Integer ()

-- import Check.Helpers

main :: IO ()
main = hspec $ describe "Karatsuba functions:" testKaratsuba


testKaratsuba :: Spec
testKaratsuba = do
    it "A kSplit Natural can be recombined correctly." $ forM_ [1..10] $ \shift ->
        let nat = mkNatural [ 1, 0, 4 * 2, 0, 16 * 3, 0, 64 * 4, 0, 256 * 5, 0, 256 * 4 * 6 ]
            (hi, lo) = kSplit nat shift
        in show (plusNatural (shiftLNatural hi (shift * WORD_SIZE_IN_BITS)) lo) `shouldBe` show nat
    it "A kShiftedAdd works with shift of zero." $ do
        let (n1, n2, n3) = (mkNatural [1], mkNatural [2], mkNatural [3])
        show (kShiftedAdd 0 n1 n2 n3) `shouldBe` "0x6"
    it "A kShiftedAdd works without overlap." $ do
        let (n1, n2, n3) = (mkNatural [1], mkNatural [2], mkNatural [3])
        show (kShiftedAdd 1 n1 n2 n3) `shouldBe` "0x100000000000000020000000000000003"
        show (kShiftedAdd 2 n1 n2 n3) `shouldBe` "0x10000000000000000000000000000000200000000000000000000000000000003"


kShiftedAdd :: Int -> Natural -> Natural -> Natural -> Natural
kShiftedAdd !shift !a@(Natural !n3 !arr3) !b@(Natural !n2 !arr2) !c@(Natural !n1 !arr1)
    | shift <= 0 = plusNatural a (plusNatural b c)
    | otherwise = unsafeInlinePrim $ do
        !marr <- newWordArray (succ (n1 + 2 * shift))
        !nlen <- loop_1 marr 0
        !narr <- unsafeFreezeWordArray marr
        returnNatural nlen narr
  where
    loop_1 !marr !i
        | i >= n1 = loop_pre_2 marr i
        -- | i = shift = loop_1_2 marr i carry
        | otherwise = do
            !x <- indexWordArrayM arr1 i
            writeWordArray marr i x
            loop_1 marr (i + 1)

    loop_pre_2 !marr !i
        | i < shift = do
            writeWordArray marr i 0
            loop_pre_2 marr (i + 1)
        | otherwise = loop_2 marr i

    loop_2 marr i
        | i - shift >= n2 = loop_pre_3 marr i
        | otherwise  = do
            !x <- indexWordArrayM arr2 (i - shift)
            writeWordArray marr i x
            loop_2 marr (i + 1)

    loop_pre_3 !marr !i
        | i < 2 * shift = do
            writeWordArray marr i 0
            loop_pre_3 marr (i + 1)
        | otherwise = loop_3 marr i

    loop_3 !marr !i
        | i - 2 * shift >= n3 = return i
        | otherwise = do
            !x <- indexWordArrayM arr3 (i - 2 * shift)
            writeWordArray marr i x
            loop_1 marr (i + 1)



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


