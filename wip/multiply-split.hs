{-# LANGUAGE BangPatterns, CPP, MagicHash, NoImplicitPrelude, ScopedTypeVariables, Strict, UnboxedTuples #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

#include "MachDeps.h"

import Prelude hiding (Integer)

import Control.Monad (forM_, when)
import GHC.Word (Word)
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen

import Common.GHC.Integer.Debug
import Common.GHC.Integer.Prim
import Common.GHC.Integer.StrictPrim
import Common.GHC.Integer.WordArray
import New3.GHC.Integer.Natural
import New3.GHC.Integer.Type
import New3.Integer ()

import Check.Helpers

main :: IO ()
main = hspec $ describe "Shifted Add:" testSplitMultiply


testSplitMultiply :: Spec
testSplitMultiply = do
    it "shiftedAdd2 #1." $ do
        let n1 = readNatural "0x3"
            n0 = readNatural "0x0"
            shift = 1
        show (shiftedAdd2 shift n1 n0) `shouldBe` show (shiftedAdd2Slow shift n1 n0)

    it "shiftedAdd2 #2." $ do
        let n0 = readNatural "0x100000000000000010000000000000000"
            n1 = readNatural "0xffffffffffffffff"
            shift = 1
        show (shiftedAdd2 shift n1 n0) `shouldBe` show (shiftedAdd2Slow shift n1 n0)

    it "shiftedAdd2 #3." $ do
        let n0 = readNatural "0xffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"
            n1 = readNatural "0xffffffffffffffff"
            shift = 1
        show (shiftedAdd2 shift n1 n0) `shouldBe` show (shiftedAdd2Slow shift n1 n0)



    when True .
        modifyMaxSuccess (const 10000) .
            prop "shiftedAdd2 QuickChecked." $ \ n0 n1 -> do
                let maxShift = 2 + max (lengthNatrual n1) (lengthNatrual n0)
                forM_ [1 .. maxShift] $ \shift ->
                    show (shiftedAdd2 shift n1 n0) `shouldBe` show (shiftedAdd2Slow shift n1 n0)

instance Arbitrary Natural where
    arbitrary = do
        randLen <- choose (2, 8)
        wrds <- vectorOf randLen $ elements [minBound, maxBound]
        pure $ mkNaturalW wrds

    shrink nat =
        let xs = toWordList nat in
        if length xs <= 1
          then []
          else map mkNaturalW [init xs, tail xs]

-- -----------------------------------------------------------------------------

shiftedAdd2Slow :: Int -> Natural -> Natural -> Natural
shiftedAdd2Slow shift n1 n0 =
    plusNatural (shiftLNatural n1 (shift * WORD_SIZE_IN_BITS)) n0

{-# NOINLINE shiftedAdd2 #-}
shiftedAdd2 :: Int -> Natural -> Natural -> Natural
shiftedAdd2 !shift !(Natural !n1 !arr1) !(Natural !n0 !arr0)
    | shift < 1 = error $ "shiftedAdd2 with shift of " ++ show shift
    | otherwise = runStrictPrim $ do
        len <- pure $ 1 + max n0 (n1 + shift)
        !marr <- newWordArray len
        when debug $
            setWordArray marr 0 (len - 1) 0x123456789
        !nlen <- start marr
        when (nlen > len) $
            error "Bad length"
        !narr <- unsafeFreezeWordArray marr
        pure $! Natural nlen narr
  where
    start !marr
        | n0 <= shift = stage0short marr 0
        | otherwise = stage0copy marr 0

    -- Stage 0.
    stage0short !marr !i
        | i < n0 = do
            !x <- indexWordArrayM arr0 i
            debugWriteWordArrayLocal __LINE__ marr i x
            stage0short marr (i + 1)

        | otherwise =
            stage0fill marr i

    stage0fill !marr !i
        | i < shift = do
            debugWriteWordArrayLocal __LINE__ marr i 0
            stage0fill marr (i + 1)

        | otherwise = stage1copy0 marr i 0

    stage0copy marr i
        | i < shift = do
            !x <- indexWordArrayM arr0 i
            debugWriteWordArrayLocal __LINE__ marr i x
            stage0copy marr (i + 1)

        | otherwise = do
            !x <- indexWordArrayM arr0 i
            !y <- indexWordArrayM arr1 0
            let (# !cry, !sm #) = plusWord2 x y
            debugWriteWordArrayLocal __LINE__ marr i sm
            if n0 - shift < n1
                then stage1addshort marr (i + 1) 1 cry
                else stage1addlong marr (i + 1) 1 cry

    -- Stage1.
    stage1copy0 marr i j
        | j < n1 = do
            !y <- indexWordArrayM arr1 j
            debugWriteWordArrayLocal __LINE__ marr i y
            stage1copy0 marr (i + 1) (j + 1)

        | otherwise =
            pure i

    stage1addshort marr i j c
        | i < n0 = do
            !x <- indexWordArrayM arr0 i
            !y <- indexWordArrayM arr1 j
            let (# !cry, !sm #) = plusWord3 x y c
            debugWriteWordArrayLocal __LINE__ marr i sm
            stage1addshort marr (i + 1) (j + 1) cry

        | otherwise =
            stage2carry marr i j c

    stage2carry marr i j c
        | c == 0 =
            stage1copy0 marr i j

        | j < n1 = do
            !y <- indexWordArrayM arr1 j
            let (# !cry, !sm #) = plusWord2 y c
            debugWriteWordArrayLocal __LINE__ marr i sm
            stage2carry marr (i + 1) (j + 1) cry

        | otherwise = do
            debugWriteWordArrayLocal __LINE__ marr i c
            pure $! i + 1

    stage1addlong marr i j c
        | j < n1 = do
            !x <- indexWordArrayM arr0 i
            !y <- indexWordArrayM arr1 j
            let (# !cry, !sm #) = plusWord3 x y c
            debugWriteWordArrayLocal __LINE__ marr i sm
            stage1addlong marr (i + 1) (j + 1) cry

        | otherwise = do
            debugPrintLocal __LINE__ $ show ((i, n0), (j, n1), c)
            stage1addcontinue marr i j c

    stage1addcontinue marr i j c
        | j < n1 = do
            !y <- indexWordArrayM arr1 j
            let (# !cry, !sm #) = plusWord2 y c
            debugWriteWordArrayLocal __LINE__ marr i sm
            stage1addlong marr (i + 1) (j + 1) cry

        | i < n0 =
            stage2copy0carry marr i c

        | otherwise = do
            debugPrintLocal __LINE__ $ show ((i, n0), (j, n1), c)
            debugWriteWordArrayLocal __LINE__ marr i c
            pure $! i + 1

    -- Stage 2.
    stage2copy0carry marr i c
        | c == 0 = do
            debugPrintLocal __LINE__ $ show ((i, n0), c)
            stage2copy0 marr i

        | i < n0 = do
            !x <- indexWordArrayM arr0 i
            let (# !cry, !sm #) = plusWord2 x c
            debugWriteWordArrayLocal __LINE__ marr i sm
            stage2copy0carry marr (i + 1) cry

        | otherwise = do
            debugPrintLocal __LINE__ $ show ((i, n0), c)
            debugWriteWordArrayLocal __LINE__ marr i c
            pure $! i + 1

    stage2copy0 marr i
        | i < n0 = do
            !x <- indexWordArrayM arr0 i
            debugWriteWordArrayLocal __LINE__ marr i x
            stage2copy0 marr (i + 1)

        | otherwise =
            pure i

--------------------------------------------------------------------------------

mkNaturalW :: [Word] -> Natural
mkNaturalW xs =
    let len = length xs in
    runStrictPrim $ do
        !marr <- newWordArray len
        !nlen <- fill marr 0 xs
        !narr <- unsafeFreezeWordArray marr
        pure $! Natural nlen narr
  where
    fill _ i [] = pure i
    fill marr i (w:ws) = do
        debugWriteWordArrayLocal __LINE__ marr i w
        fill marr (i + 1) ws

lengthNatrual :: Natural -> Int
lengthNatrual (Natural n _) = n


toWordList :: Natural -> [Word]
toWordList (Natural n arr) =
    reverse $ loop 0 []
  where
    loop :: Int -> [Word] -> [Word]
    loop i acc
        | i >= n = acc
        | otherwise =
            loop (i + 1) ((indexWordArray arr i) : acc)

readNatural :: String -> Natural
readNatural = mkNatural . fromString

debug :: Bool
debug = False

debugWriteWordArrayLocal :: Int -> MutableWordArray (StrictPrim s) -> Int -> Word -> StrictPrim s ()
debugWriteWordArrayLocal line marr i x = do
    when debug $
        debugPrintLocal line $ "writing " ++ hexShowW x ++ " at " ++ show i
    writeWordArray marr i x

debugPrintLocal :: Int -> String -> StrictPrim s ()
debugPrintLocal line s =
    when debug $
        debugPrint line s
