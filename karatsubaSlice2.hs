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
import Test.QuickCheck.Gen
import Unsafe.Coerce (unsafeCoerce)

import New3.GHC.Integer.Prim
import New3.GHC.Integer.Type
import New3.GHC.Integer.WordArray
import New3.Integer ()

import Check.Helpers


main :: IO ()
main = hspec $ describe "Karatsuba functions:" testKaratsuba


testKaratsuba :: Spec
testKaratsuba = do
    it "A kSplit Natural can be recombined correctly." $ forM_ [0 .. 10] $ \shift ->
        let nat = mkNatural [ 1, 0, 4 * 2, 0, 16 * 3, 0, 64 * 4, 0, 256 * 5, 0, 256 * 4 * 6 ]
            (hi, lo) = kSplit nat shift
        in show (plusNatural (shiftLNatural hi (shift * WORD_SIZE_IN_BITS)) lo) `shouldBe` show nat

    prop "kShiftedAdd is equivalent to kShiftedAddSlow." $ \ n2 n1 n0 wshift -> do
        let shift = 1 + ((abs wshift) `mod` 16)
        show (kShiftedAdd shift n2 n1 n0) `shouldBe` show (kShiftedAddSlow shift n2 n1 n0)

    it "A kSplitSlow Natural can be recombined correctly." $ forM_ [0 .. 10] $ \shift -> do
        let nat = mkNatural [ 1, 0, 4 * 2, 0, 16 * 3, 0, 64 * 4, 0, 256 * 5, 0, 256 * 4 * 6 ]
            (hi, lo) = kSplitSlow nat shift
        show (plusNatural (shiftLNatural hi (shift * WORD_SIZE_IN_BITS)) lo) `shouldBe` show nat

    it "A kSplit Natural can be recombined correctly." $ forM_ [0 .. 10] $ \shift -> do
        let nat = mkNatural [ 1, 0, 4 * 2, 0, 16 * 3, 0, 64 * 4, 0, 256 * 5, 0, 256 * 4 * 6 ]
            (hi, lo) = kSplit nat shift
        show (plusNatural (shiftLNatural hi (shift * WORD_SIZE_IN_BITS)) lo) `shouldBe` show nat

    prop "kSplit and kSplitSlow give the same result." $ \ nat wshift -> do
        let split = (abs wshift) `mod` 16
        show (kSplit nat split) `shouldBe` show (kSplitSlow nat split)

    prop "karatsubaSlow multiply works without recursion." $ \ n1 n2 -> do
        show (karatsubaSlow False n1 n2) `shouldBe` show (timesNatural n1 n2)
    prop "karatsubaSlow multiply works with recursion." $ \ n1 n2 -> do
        show (karatsubaSlow True n1 n2) `shouldBe` show (timesNatural n1 n2)

    prop "karatsuba multiply works without recursion." $ \ n1 n2 -> do
        show (karatsuba False n1 n2) `shouldBe` show (timesNatural n1 n2)
    prop "karatsuba multiply works with recursion." $ \ n1 n2 -> do
        show (karatsuba True n1 n2) `shouldBe` show (timesNatural n1 n2)




karatsuba :: Bool -> Natural -> Natural -> Natural
karatsuba recursive !num1@(Natural !n1 _) !num2@(Natural !n2 _)
    | n1 < 4 || n2 < 4 = timesNatural num1 num2
    | otherwise = do
        let fmultiply = if recursive then karatsuba recursive else timesNatural
            m2 = (max n1 n2) `div` 2
            (hi1, lo1) = kSplit num1 m2
            (hi2, lo2) = kSplit num2 m2
            z0 = fmultiply lo1 lo2
            z1 = fmultiply (plusNatural lo1 hi1) (plusNatural lo2 hi2)
            z2 = fmultiply hi1 hi2
            middle = minusNatural z1 (plusNatural z2 z0)
        kShiftedAdd m2 z2 middle z0


karatsubaSlow :: Bool -> Natural -> Natural -> Natural
karatsubaSlow recursive num1@(Natural n1 _) num2@(Natural n2 _)
    | n1 < 4 || n2 < 4 = timesNatural num1 num2
    | otherwise =
        let fmultiply = if recursive then karatsubaSlow recursive else timesNatural
            m2 = (max n1 n2) `div` 2
            (hi1, lo1) = kSplit num1 m2
            (hi2, lo2) = kSplit num2 m2
            z0 = fmultiply lo1 lo2
            z1 = fmultiply (plusNatural lo1 hi1) (plusNatural lo2 hi2)
            z2 = fmultiply hi1 hi2
            middle = minusNatural z1 (plusNatural z2 z0)
        in kShiftedAddSlow m2 z2 middle z0




kSplit :: Natural -> Int -> (Natural, Natural)
kSplit nat@(Natural n arr) i
    | i <= 0 = ( nat, Natural 0 arr)
    | i >= n = ( Natural 0 arr, nat )
    | otherwise = ( sliceOfNatural nat i (n - i), sliceOfNatural nat 0 i )


kSplitSlow :: Natural -> Int -> (Natural, Natural)
kSplitSlow nat@(Natural n arr) i
    | i <= 0 = ( nat, Natural 0 arr)
    | i > n = ( Natural 0 arr, nat )
    | otherwise = ( shiftRNatural nat (i * WORD_SIZE_IN_BITS), andNatural nat (mkShiftMask i))


mkShiftMask :: Int -> Natural
mkShiftMask shift =
    unsafeInlinePrim mkMask
  where
    mkMask :: IO Natural
    mkMask = do
        !marr <- newWordArray shift
        setWordArray marr 0 shift ((0 :: Word) - 1)
        narr <- unsafeFreezeWordArray marr
        return $ Natural shift narr

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


kShiftedAddSlow :: Int -> Natural -> Natural -> Natural -> Natural
kShiftedAddSlow shift n2 n1 n0 =
    plusNatural (shiftLNatural n2 (2 * shift * WORD_SIZE_IN_BITS))
        (plusNatural (shiftLNatural n1 (shift * WORD_SIZE_IN_BITS)) n0)


kShiftedAdd :: Int -> Natural -> Natural -> Natural -> Natural
kShiftedAdd !shift !(Natural !n2 !arr2) !(Natural !n1 !arr1) !(Natural !n0 !arr0)
    | shift < 1 = error $ "kShiftedAdd with shift of " ++ show shift
    | otherwise = unsafeInlinePrim $ do
        let !len = succ $ max n0 (max (n1 + shift) (n2 + 2 * shift))
        debugPrint __LINE__ $ "length is " ++ show len ++ " " ++ show (0 :: Int, (n2, n1, n0), shift)
        !marr <- newWordArray len
        setWordArray marr 0 len (0xaeaeaeaaeaeaeaea :: Word)
        !nlen <- start marr
        if nlen > len
            then error "Bad length"
            else return ()
        !narr <- unsafeFreezeWordArray marr
        finalizeNatural nlen narr
  where
    start !marr
        | n0 <= shift = stage0short0 marr 0
        | otherwise = stage0copy  marr 0

    -- Stage 0.
    stage0short0 !marr !i
        | i < n0 = do
            !x <- indexWordArrayM arr0 i
            debugWriteWordArray __LINE__ marr i x
            stage0short0 marr (i + 1)
        | otherwise = stage0short0fill marr i

    stage0copy  !marr !i
        | i < shift = do
            !x <- indexWordArrayM arr0 i
            debugWriteWordArray __LINE__ marr i x
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
            debugWriteWordArray __LINE__ marr i 0
            stage0short0fill marr (i + 1)
        | n1 <= shift = stage1do1 marr i
        | otherwise = stage1copy1 marr i

    -- Stage 1.
    stage1do0 !marr !i
        | i < n0 = do
            !x <- indexWordArrayM arr0 i
            debugWriteWordArray __LINE__ marr i x
            stage1do0 marr (i + 1)
        | otherwise = stage1fill marr i

    stage1do0c !marr !i !carry
        | i < n0 = do
            !x <- indexWordArrayM arr0 i
            let (# !cry, !sm #) = plusWord2 x carry
            debugWriteWordArray __LINE__ marr i sm
            if cry /= 0
                then stage1do0c marr (i + 1) cry
                else stage1do0 marr (i + 1)
        | carry /= 0 = do
            debugWriteWordArray __LINE__ marr i carry
            stage1fill marr (i + 1)
        | otherwise = stage1fill marr i

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
            let (# !cry, !sm #) = plusWord3 x y carry
            debugWriteWordArray __LINE__ marr i sm
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
            debugWriteWordArray __LINE__ marr i sm
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
            debugWriteWordArray __LINE__ marr i sm
            if cry /= 0
                then stage1long0c marr (i + 1) cry
                else stage1long0 marr (i + 1)
        | n0 - 2 * shift <= n2 = stage2do02 marr i carry
        | otherwise = stage2do20 marr i carry

    stage1long0 !marr !i
        | i < 2 * shift = do
            !x <- indexWordArrayM arr0 i
            debugWriteWordArray __LINE__ marr i x
            stage1long0 marr (i + 1)
        | n0 - 2 * shift <= n2 = stage2do02 marr i 0
        | otherwise = stage2do20 marr i 0

    stage1long1c !marr !i !carry
        | i - shift < n1 = do
            !x <- indexWordArrayM arr1 (i - shift)
            let (# !cry, !sm #) = plusWord2 x carry
            debugWriteWordArray __LINE__ marr i sm
            stage1long1c marr (i + 1) cry
        | carry /= 0 = do
            debugPrint __LINE__ $ show (i, n1, shift)
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
        | n1 - shift > n2 = stage2do21 marr i carry
        | otherwise = stage2do12 marr i carry

    stage1long01 !marr !i !carry
        | i < 2 * shift = do
            !x <- indexWordArrayM arr0 i
            !y <- indexWordArrayM arr1 (i - shift)
            let (# !cry, !sm #) = plusWord3 x y carry
            debugWriteWordArray __LINE__ marr i sm
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
            debugWriteWordArray __LINE__ marr i sm
            stage2do012 marr (i + 1) cry
        | otherwise = stage2do12 marr i carry

    stage2do021 !marr !i !carry
        | i < n0 = do
            !x <- indexWordArrayM arr0 i
            !y <- indexWordArrayM arr1 (i - shift)
            !z <- indexWordArrayM arr2 (i - 2 * shift)
            let (# !cry, !sm #) = plusWord4 x y z carry
            debugWriteWordArray __LINE__ marr i sm
            stage2do021 marr (i + 1) cry
        | otherwise = stage2do21 marr i carry

    stage2do102 !marr !i !carry
        | i - shift < n1 = do
            !x <- indexWordArrayM arr0 i
            !y <- indexWordArrayM arr1 (i - shift)
            !z <- indexWordArrayM arr2 (i - 2 * shift)
            let (# !cry, !sm #) = plusWord4 x y z carry
            debugWriteWordArray __LINE__ marr i sm
            stage2do102 marr (i + 1) cry
        | otherwise = stage2do02 marr i carry

    stage2do120 !marr !i !carry
        | i - shift < n1 = do
            !x <- indexWordArrayM arr0 i
            !y <- indexWordArrayM arr1 (i - shift)
            !z <- indexWordArrayM arr2 (i - 2 * shift)
            let (# !cry, !sm #) = plusWord4 x y z carry
            debugWriteWordArray __LINE__ marr i sm
            stage2do120 marr (i + 1) cry
        | otherwise = stage2do20 marr i carry

    stage2do201 !marr !i !carry
        | i - 2 * shift < n2 = do
            !x <- indexWordArrayM arr0 i
            !y <- indexWordArrayM arr1 (i - shift)
            !z <- indexWordArrayM arr2 (i - 2 * shift)
            let (# !cry, !sm #) = plusWord4 x y z carry
            debugWriteWordArray __LINE__ marr i sm
            stage2do201 marr (i + 1) cry
        | otherwise = stage2do01 marr i carry

    stage2do210 !marr !i !carry
        | i - 2 * shift < n2 = do
            !x <- indexWordArrayM arr0 i
            !y <- indexWordArrayM arr1 (i - shift)
            !z <- indexWordArrayM arr2 (i - 2 * shift)
            let (# !cry, !sm #) = plusWord4 x y z carry
            debugWriteWordArray __LINE__ marr i sm
            stage2do210 marr (i + 1) cry
        | otherwise = stage2do10 marr i carry

    -- Stage 2, two elements.
    stage2do01 !marr !i !carry
        | i < n0 = do
            !x <- indexWordArrayM arr0 i
            !y <- indexWordArrayM arr1 (i - shift)
            let (# !cry, !sm #) = plusWord3 x y carry
            debugWriteWordArray __LINE__ marr i sm
            stage2do01 marr (i + 1) cry
        | carry /= 0 = stage2final1c marr i carry
        | otherwise = stage2final1 marr i

    stage2do02 !marr !i !carry
        | i < n0 = do
            !x <- indexWordArrayM arr0 i
            !y <- indexWordArrayM arr2 (i - 2 * shift)
            let (# !cry, !sm #) = plusWord3 x y carry
            debugWriteWordArray __LINE__ marr i sm
            stage2do02 marr (i + 1) cry
        | carry /= 0 = stage2final2c marr i carry
        | otherwise = stage2final2 marr i

    stage2do10 !marr !i !carry
        | i - shift < n1 = do
            !x <- indexWordArrayM arr0 i
            !y <- indexWordArrayM arr1 (i - shift)
            let (# !cry, !sm #) = plusWord3 x y carry
            debugWriteWordArray __LINE__ marr i sm
            stage2do10 marr (i + 1) cry
        | carry == 0 = stage2final0 marr i
        | otherwise = stage2final0c marr i carry

    stage2do12 !marr !i !carry
        | i - shift < n1 = do
            !x <- indexWordArrayM arr1 (i - shift)
            !y <- indexWordArrayM arr2 (i - 2 * shift)
            let (# !cry, !sm #) = plusWord3 x y carry
            debugWriteWordArray __LINE__ marr i sm
            stage2do12 marr (i + 1) cry
        | carry /= 0 = stage2final2c marr i carry
        | otherwise = stage2final2 marr i

    stage2do20 !marr !i !carry
        | i - 2 * shift < n2 = do
            !x <- indexWordArrayM arr0 i
            !y <- indexWordArrayM arr2 (i - 2 * shift)
            let (# !cry, !sm #) = plusWord3 x y carry
            debugWriteWordArray __LINE__ marr i sm
            stage2do20 marr (i + 1) cry
        | carry /= 0 = stage2final0c marr i carry
        | otherwise = stage2final0 marr i

    stage2do21 !marr !i !carry
        | i - 2 * shift < n2 = do
            !x <- indexWordArrayM arr1 (i - shift)
            !y <- indexWordArrayM arr2 (i - 2 * shift)
            let (# !cry, !sm #) = plusWord3 x y carry
            debugWriteWordArray __LINE__ marr i sm
            stage2do21 marr (i + 1) cry
        | carry == 0 = stage2final1 marr i
        | otherwise = stage2final1c marr i carry

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
                else stage2final0c marr (i + 1) cry
        | carry /= 0 = do
            debugWriteWordArray __LINE__ marr i carry
            return (i + 1)
        | otherwise = return i

    stage2final1 !marr !i
        | i - shift < n1 = do
            !x <- indexWordArrayM arr1 (i - shift)
            debugWriteWordArray __LINE__ marr i x
            stage2final1 marr (i + 1)
        | otherwise = return i

    stage2final1c !marr !i !carry
        | i - shift < n1 = do
            !x <- indexWordArrayM arr1 (i - shift)
            let (# !cry, !sm #) = plusWord2 x carry
            debugWriteWordArray __LINE__ marr i sm
            if carry == 0
                then stage2final1 marr (i + 1)
                else stage2final1c marr (i + 1) cry
        | carry /= 0 = do
            debugWriteWordArray __LINE__ marr i carry
            return (i + 1)
        | otherwise = return i

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
                else stage2final2c marr (i + 1) cry
        | otherwise = do
            debugWriteWordArray __LINE__ marr i carry
            return (i + 1)

--------------------------------------------------------------------------------

debugWriteWordArray :: Int -> MutableWordArray IO -> Int -> Word -> IO ()
#if 0
debugWriteWordArray line marr i x = do
    debugPrint line $ "writing " ++ hexShowW x ++ " at " ++ show i
    writeWordArray marr i x
#else
debugWriteWordArray _ marr i x = writeWordArray marr i x
#endif


debugPrint :: Int -> String -> IO ()
#if 1
debugPrint _ _ = return ()
#else
debugPrint line str =
    let ls
            | line < 100 = ' ' : show line
            | otherwise = show line
    in putStrLn $ ls ++ " : " ++ str
#endif


instance Arbitrary Natural where
    arbitrary = do
        randLen <- choose (8, 100)
        wrds <- fmap positive32bits $ vectorOf randLen arbitrary
        return $ mkNatural wrds

    shrink (Natural n arr) = map (\x -> Natural x arr) [1 .. (n - 1)]
