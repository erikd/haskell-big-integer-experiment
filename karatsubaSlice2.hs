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

    it "A kSplitSlow Natural can be recombined correctly." $ forM_ [0 .. 10] $ \shift -> do
        let nat = mkNatural [ 1, 0, 4 * 2, 0, 16 * 3, 0, 64 * 4, 0, 256 * 5, 0, 256 * 4 * 6 ]
            (hi, lo) = kSplitSlow nat shift
        show (plusNatural (shiftLNatural hi (shift * WORD_SIZE_IN_BITS)) lo) `shouldBe` show nat

    prop "kShiftedAdd works arbitrary overlap." $ \ n2 n1 n0 wshift -> do
        let shift = 1 + ((abs wshift) `mod` 8)
        show (kShiftedAdd shift n2 n1 n0) `shouldBe` show (kShiftedAddSlow shift n2 n1 n0)

    prop "Slow multiply works." $ \ n1 n2 -> do
        show (karatsuba n1 n2) `shouldBe` show (timesNatural n1 n2)


    {-
    prop "kSPlit and kSplitSlow give the same result." $ \ nat wshift -> do
        let split = (abs wshift) `mod` 16
        show (kSplit nat split) `shouldBe` show (kSplitSlow nat split)
    -}



karatsuba :: Natural -> Natural -> Natural
karatsuba num1@(Natural n1 _) num2@(Natural n2 _)
    | n1 < 3 || n2 < 3 = timesNatural num1 num2
    | otherwise =
        let multiply = if True then karatsuba else checkedMultiply
            m2 = (max n1 n2) `div` 2
            (hi1, lo1) = kSplitSlow num1 m2
            (hi2, lo2) = kSplitSlow num2 m2
            z0 = multiply lo1 lo2
            z1 = multiply (plusNatural lo1 hi1) (plusNatural lo2 hi2)
            z2 = multiply hi1 hi2
            middle = minusNatural z1 (plusNatural z2 z0)
        in kShiftedAddSlow m2 z2 middle z0


checkedMultiply :: Natural -> Natural -> Natural
checkedMultiply a b =
    let correct = timesNatural a b
        check = karatsuba a b
    in if not (eqNatural correct check)
        then error $ show a ++ " * " ++ show b ++ "\n should be " ++ show correct ++ " ft(" ++ show (lengthNatural correct) ++ ")\n    but is " ++ show check ++ " (" ++ show (lengthNatural check) ++ ")"
        else correct


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
        len <- return . succ $ max n0 (max (n1 + shift) (n2 + 2 * shift))
        debugPrint __LINE__ $ "length is " ++ show len
        !marr <- newWordArray len
        setWordArray marr 0 len (0xaeaeaeaaeaeaeaea :: Word)
        !nlen <- loop_1 marr 0 0
        if nlen > len
            then error "Bad length"
            else return ()
        !narr <- unsafeFreezeWordArray marr
        returnNatural nlen narr
  where
    loop_1 !marr !i !carry
        | i < succ (max n0 (max (n1 + shift) (n2 + 2 * shift))) = do
                (c0, s0) <-
                    if i < n0
                        then do
                            x <- indexWordArrayM arr0 i
                            debugPrint __LINE__ $ "read " ++ hexShowW x ++ " from arr0"
                            let (# c, s #) = plusWord2 x carry
                            return (c, s)
                        else return (0, carry)
                (c1, s1) <-
                    if i >= shift && i - shift < n1
                        then do
                            y <- indexWordArrayM arr1 (i - shift)
                            debugPrint __LINE__ $ "read " ++ hexShowW y ++ " from arr1"
                            let (# c, s #) = plusWord2 s0 y
                            return $ (c, s)
                        else return (c0, s0)
                (c2, s2) <-
                    if i >= 2 * shift && i - 2 * shift < n2
                        then do
                            z <- indexWordArrayM arr2 (i - 2 * shift)
                            debugPrint __LINE__ $ "read " ++ hexShowW z ++ " from arr2"
                            let (# c, s #) = plusWord2 s1 z
                            return (c + c1, s)
                        else return (c1, s1)
                debugWriteWordArray __LINE__ marr i s2
                loop_1 marr (i + 1) c2
        | carry /= 0 = do
                debugWriteWordArray __LINE__ marr i carry
                debugPrint __LINE__ $ "returning " ++ show (i + 1)
                return (i + 1)
        | otherwise = do
                debugPrint __LINE__ $ "returning " ++ show i
                return i

--------------------------------------------------------------------------------

debugWriteWordArray :: Int -> MutableWordArray IO -> Int -> Word -> IO ()
debugWriteWordArray line marr i x = do
    debugPrint line $ "writing " ++ hexShowW x ++ " at " ++ show i
    writeWordArray marr i x

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


lengthNatural :: Natural -> Int
lengthNatural (Natural n _) = n

instance Arbitrary Natural where
    arbitrary = do
        randLen <- choose (8, 100)
        wrds <- fmap positive32bits $ vectorOf randLen arbitrary
        return $ mkNatural wrds
