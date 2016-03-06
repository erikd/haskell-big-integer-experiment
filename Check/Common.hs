{-# LANGUAGE UnboxedTuples, FlexibleInstances, ScopedTypeVariables #-}
module Check.Common
    ( testCommon
    ) where

import Prelude hiding (Integer)

import Data.List (sort)
import Test.Hspec
import Test.Hspec.QuickCheck

import qualified GMP.Integer as G

import Common.GHC.Integer.Prim

import Check.Helpers

testCommon :: Spec
testCommon = do
    prop "Can add Words catching the carry." $ \ (w1, w2) ->
        let (# c, s #) = plusWord2 w1 w2
            f1 = G.plusInteger (G.wordToInteger (unboxWord w1)) (G.wordToInteger (unboxWord w2))
            f2 = G.plusInteger (G.wordToInteger (unboxWord s)) (G.shiftLInteger (G.wordToInteger (unboxWord c)) (unboxInt bitsPerWord))
        in f1 `shouldBe` f2
    prop "Can add Words add a carry and catch overflow." $ \ (w1, w2, c) ->
        let (# cry, sm #) = plusWord2C w1 w2 c
            f1 = foldl1 G.plusInteger [G.wordToInteger (unboxWord w1), G.wordToInteger (unboxWord w2), G.wordToInteger (unboxWord c)]
            f2 = G.plusInteger (G.wordToInteger (unboxWord sm)) (G.shiftLInteger (G.wordToInteger (unboxWord cry)) (unboxInt bitsPerWord))
        in f2 `shouldBe` f1
    prop "Can subtract Words and catch overflow." $ \ (w1, w2) ->
        let (# cry, sm #) = minusWord2 w1 w2
            f1 = G.minusInteger (G.wordToInteger (unboxWord w1)) (G.wordToInteger (unboxWord w2))
            f2 = G.minusInteger (G.wordToInteger (unboxWord sm)) (G.shiftLInteger (G.wordToInteger (unboxWord cry)) (unboxInt bitsPerWord))
        in f2 `shouldBe` f1
    prop "Can subtract Words with a carry and catch overflow." $ \ (w1, w2, c) ->
        let (# cry, sm #) = minusWord2C w1 w2 c
            f1 = G.minusInteger (G.wordToInteger (unboxWord w1)) (G.plusInteger (G.wordToInteger (unboxWord w2)) (G.wordToInteger (unboxWord c)))
            f2 = G.minusInteger (G.wordToInteger (unboxWord sm)) (G.shiftLInteger (G.wordToInteger (unboxWord cry)) (unboxInt bitsPerWord))
        in f2 `shouldBe` f1
    prop "Can multiply Words catching overflow." $ \ (w1, w2) ->
        let (# ov, prod #) = timesWord2 w1 w2
            f1 = G.timesInteger (G.wordToInteger (unboxWord w1)) (G.wordToInteger (unboxWord w2))
            f2 = G.plusInteger (G.wordToInteger (unboxWord prod)) (G.shiftLInteger (G.wordToInteger (unboxWord ov)) (unboxInt bitsPerWord))
        in f2 `shouldBe` f1
    prop "Can multiply Words add a carry and catch overflow." $ \ (w1, w2, c) ->
        let (# ov, prod #) = timesWord2C w1 w2 c
            f1 = G.plusInteger (G.timesInteger (G.wordToInteger (unboxWord w1)) (G.wordToInteger (unboxWord w2))) (G.wordToInteger (unboxWord c))
            f2 = G.plusInteger (G.wordToInteger (unboxWord prod)) (G.shiftLInteger (G.wordToInteger (unboxWord ov)) (unboxInt bitsPerWord))
        in f2 `shouldBe` f1
    prop "Function quotRemWord works." $ \ x yMaybeZero ->
        let yNotZero = if yMaybeZero == 0 then 42 else yMaybeZero
            (# q, r #) = quotRemWord x yNotZero
        in q * yNotZero + r `shouldBe` x
    prop "Function quotRemWord2 works." $ \ a b c ->
        let [r, yMaybeZero, q] = sort [a, b, c]
            yNotZero = if yMaybeZero == 0 then 42 else yMaybeZero
            (# xhi, xlo #) = timesWord2C q yNotZero r
            (# qt, rt #) = quotRemWord2 xhi xlo yNotZero
        in showUT2 (timesWord2C qt yNotZero rt) `shouldBe` showUT2 (# xhi, xlo #)

