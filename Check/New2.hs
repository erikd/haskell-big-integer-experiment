{-# LANGUAGE FlexibleInstances, ScopedTypeVariables #-}
module Check.New2
    ( testNew2Internal
    , testNew2Integer
    ) where

import Prelude hiding (Integer)

import Control.Applicative ((<$>))
import Data.Bits ((.&.))
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck.Arbitrary


import qualified GMP.Integer as G
import qualified New2.Integer as N2

import New2.GHC.Integer.Prim
import New2.GHC.Integer.Type

import Check.Helpers

testNew2Internal :: Spec
testNew2Internal = do
    prop "Can add Words catching the carry." $ \ (w1, w2) ->
        let (# c, s #) = plusWord2 w1 w2
            f1 = G.plusInteger (G.wordToInteger (unboxWord w1)) (G.wordToInteger (unboxWord w2))
            f2 = G.plusInteger (G.wordToInteger (unboxWord s)) (G.shiftLInteger (G.wordToInteger (unboxWord c)) 64#)
        in f1 `shouldBe` f2
    prop "Can add Words add a carry and catch overflow." $ \ (w1, w2, c) ->
        let (# cry, sm #) = plusWord2C w1 w2 c
            f1 = foldl1 G.plusInteger [G.wordToInteger (unboxWord w1), G.wordToInteger (unboxWord w2), G.wordToInteger (unboxWord c)]
            f2 = G.plusInteger (G.wordToInteger (unboxWord sm)) (G.shiftLInteger (G.wordToInteger (unboxWord cry)) 64#)
        in f2 `shouldBe` f1
    prop "Can multiply Words catching overflow." $ \ (w1, w2) ->
        let (ov, prod) = timesWord2 w1 w2
            f1 = G.timesInteger (G.wordToInteger (unboxWord w1)) (G.wordToInteger (unboxWord w2))
            f2 = G.plusInteger (G.wordToInteger (unboxWord prod)) (G.shiftLInteger (G.wordToInteger (unboxWord ov)) 64#)
        in f2 `shouldBe` f1
    prop "Can multiply Words add a carry and catch overflow." $ \ (w1, w2, c) ->
        let (ov, prod) = timesWord2C w1 w2 c
            f1 = G.plusInteger (G.timesInteger (G.wordToInteger (unboxWord w1)) (G.wordToInteger (unboxWord w2))) (G.wordToInteger (unboxWord c))
            f2 = G.plusInteger (G.wordToInteger (unboxWord prod)) (G.shiftLInteger (G.wordToInteger (unboxWord ov)) 64#)
        in f2 `shouldBe` f1


testNew2Integer :: Spec
testNew2Integer = do
    prop "Can convert from Int." $ \ i ->
        show (N2.smallInteger (unboxInt i)) `shouldBe` show (G.smallInteger (unboxInt i))
    prop "Can convert to Int." $ \ i ->
        boxIntHash (N2.integerToInt (N2.smallInteger (unboxInt i))) `shouldBe` i

    it "Can create Integers." $ do
        show (N2.mkInteger True [0x7fffffff, 0x7fffffff, 0x3f]) `shouldBe` "+0xfffffffffffffffff"
    prop "Can create Integers." $ \ (GNP g s) ->
        show g == show s

    prop "Can complement an Integer." $ \ (GNP g s) ->
        show (N2.complementInteger s) `shouldBe` show (G.complementInteger g)
    prop "Can negate an Integer." $ \ (GNP g s) ->
        show (N2.negateInteger s) `shouldBe` show (G.negateInteger g)
    prop "Can take abs of an Integer." $ \ (GNP g s) ->
        show (N2.absInteger s) `shouldBe` show (G.absInteger g)

    it "Can shiftL Integers." $ do
        let s = N2.smallInteger 0x12345#
        show (N2.shiftLInteger s 4#) `shouldBe` "+0x123450"
        show (N2.shiftLInteger s 64#) `shouldBe` "+0x123450000000000000000"
        show (N2.shiftLInteger s 68#) `shouldBe` "+0x1234500000000000000000"
        show (N2.shiftLInteger (N2.mkInteger False [0x7fffffff]) 127#) `shouldBe` "-0x3fffffff80000000000000000000000000000000"

    prop "Can shiftL Integers by up to 128 bits." $ \ (GNP g s, int) -> do
        let bits = unboxInt (int .&. 0x7f)
        show (N2.shiftLInteger s bits) `shouldBe` show (G.shiftLInteger g bits)

    prop "Can compare > two Integers." $ \ (GNP ga sa, GNP gb sb) ->
        N2.gtInteger sa sb `shouldBe` G.gtInteger ga gb
    prop "Can compare < two Integers." $ \ (GNP ga sa, GNP gb sb) ->
        N2.ltInteger sa sb `shouldBe` G.ltInteger ga gb
    prop "Can compare >= two Integers." $ \ (GNP ga sa, GNP gb sb) ->
        N2.geInteger sa sb `shouldBe` G.geInteger ga gb
    prop "Can compare <= two Integers." $ \ (GNP ga sa, GNP gb sb) ->
        N2.leInteger sa sb `shouldBe` G.leInteger ga gb

    prop "Can add two Integers." $ \ (GNP ga sa, GNP gb sb) ->
        show (N2.plusInteger sa sb) `shouldBe` show (G.plusInteger ga gb)
    prop "Can subtract two Integers." $ \ (GNP ga sa, GNP gb sb) ->
        show (N2.minusInteger sa sb) `shouldBe` show (G.minusInteger ga gb)

    prop "Can AND two positive Integers." $ \ (GNP ga sa, GNP gb sb) ->
        show (N2.andInteger (N2.absInteger sa) (N2.absInteger sb)) `shouldBe` show (G.andInteger (G.absInteger ga) (G.absInteger gb))
    prop "Can OR two positive Integers." $ \ (GNP ga sa, GNP gb sb) ->
        show (N2.orInteger (N2.absInteger sa) (N2.absInteger sb)) `shouldBe` show (G.orInteger (G.absInteger ga) (G.absInteger gb))

    it "Can multiply two small Integers." $ do
        show (N2.timesInteger (N2.smallInteger 0x100#) (N2.smallInteger 0x22#)) `shouldBe` "+0x2200"
        show (N2.timesInteger (N2.smallInteger -0x100#) (N2.smallInteger 0x22#)) `shouldBe` "-0x2200"
        show (N2.timesInteger (N2.smallInteger 0x100#) (N2.smallInteger -0x22#)) `shouldBe` "-0x2200"
        show (N2.timesInteger (N2.smallInteger -0x100#) (N2.smallInteger -0x22#)) `shouldBe` "+0x2200"

    it "Can multiply large Integer by small." $ do
        show (N2.timesInteger (N2.mkInteger True [0,2]) (N2.smallInteger 0x22#)) `shouldBe` "+0x2200000000"
        show (N2.timesInteger (N2.mkInteger False [0,2]) (N2.smallInteger 0x22#)) `shouldBe` "-0x2200000000"
        show (N2.timesInteger (N2.mkInteger True [0,2]) (N2.smallInteger -0x22#)) `shouldBe` "-0x2200000000"
        show (N2.timesInteger (N2.mkInteger False [0,2]) (N2.smallInteger -0x22#)) `shouldBe` "+0x2200000000"

    it "Can multiply two Integers (old failures)." $ do
        show (N2.timesInteger (N2.mkInteger True [1, 2, 4]) (N2.mkInteger True [1, 2])) `shouldBe` "+0x1000000020000000200000001"
        show (N2.timesInteger (N2.mkInteger True [1, 2, 4, 8]) (N2.mkInteger True [1, 2, 4, 8])) `shouldBe` "+0x1000000020000000300000004000000030000000200000001"
        show (N2.timesInteger (N2.mkInteger True [0x7ffffffe, 0x7ffffffe, 4]) (N2.mkInteger True [0x7ffffffe, 0x7ffffffe, 4])) `shouldBe` "+0x18ffffffebffffffb4000000200000004"
        show (N2.timesInteger (N2.mkInteger False [0, 0x7fffffff]) (N2.mkInteger False [0, 0xfffffffe])) `shouldBe` "+0x1fffffff800000008000000000000000"
        show (N2.timesInteger (N2.mkInteger False [1, 0x7fffffff]) (N2.mkInteger False [1, 0xfffffffe])) `shouldBe` "+0x1fffffff800000013ffffffe80000001"
        show (N2.timesInteger (N2.mkInteger False [0x3b129743, 0x6b866650]) (N2.mkInteger False [0x18865e53,0x6295e0a])) `shouldBe` "+0xa5a19af9c4da2c1eaac6f46fa3a4b9"
        show (N2.timesInteger (N2.mkInteger True [1, 1, 6]) (N2.mkInteger True [1, 1, 6])) `shouldBe` "+0x240000001800000034000000100000001"
        show (N2.timesInteger (N2.mkInteger True [ 0, 0, 0, 4, 8, 1 ]) (N2.mkInteger True [ 0, 0, 0, 8 ])) `shouldBe` "+0x800000080000000800000000000000000000000000000000000000000000000"
        show (N2.timesInteger (N2.mkInteger True [0, 2, 4, 8, 16, 32]) (N2.mkInteger True [0,2])) `shouldBe` "+0x1000000010000000100000001000000010000000000000000"
        show (N2.timesInteger (N2.mkInteger True [0, 1, 1, 1, 1 , 1]) (N2.mkInteger True [0, 1, 0, 0, 1])) `shouldBe` "+0x8000000100000002000000080000001000000010000000200000004000000000000000"
        let a4 = [0x7ffffc3d,0x7ffffdc4,0x7ffffeab]
            b4 = [0x7fffff03,0x7ffffc71,0x7fffff6e,0x7ffffd6d,0x7fffff7a,0x294]
        show (N2.timesInteger (N2.mkInteger True a4) (N2.mkInteger True b4)) `shouldBe` "+0x294fff9232dffebaeebffd6dbf80086cfb001f4c78002d7cc4007c9bc8003b7b7"


    prop "Can multiply two Integers." $ \ (GNP ga sa, GNP gb sb) ->
        show (N2.timesInteger sa sb) `shouldBe` show (G.timesInteger ga gb)

    it "Tests that have failed once for no good reason." $ do
        let a1 = [0x1c0f0ec5,0x6f32398e]
            b1 = [0x24d73dd2,0x2cce01af]
        show (N2.plusInteger (N2.mkInteger False a1) (N2.mkInteger True b1)) `shouldBe` show (G.plusInteger (G.mkInteger False a1) (G.mkInteger True b1))
        let a2 = [0x730755a,0x24d01108]
            b2 = [0x13dded6c,0x319f8341]
        show (N2.minusInteger (N2.mkInteger False a2) (N2.mkInteger True b2)) `shouldBe` show (G.minusInteger (G.mkInteger False a2) (G.mkInteger True b2))

    it "Can calculate product [1..n]." $ do
        show (foldl1 N2.timesInteger $ map (\x -> N2.smallInteger (unboxInt x)) [1..10])
            `shouldBe` show (foldl1 G.timesInteger $ map (\x -> G.smallInteger (unboxInt x)) [1..10])
        show (foldl1 N2.timesInteger $ map (\x -> N2.smallInteger (unboxInt x)) [1..100])
            `shouldBe` show (foldl1 G.timesInteger $ map (\x -> G.smallInteger (unboxInt x)) [1..100])

    it "Can shiftR Integers." $ do
        show (N2.shiftRInteger (N2.smallInteger 0x12345#) 4#) `shouldBe` "+0x1234"
        show (N2.shiftRInteger (N2.mkInteger True [0, 0, 4]) 0#) `shouldBe` "+0x10000000000000000"
        show (N2.shiftRInteger (N2.mkInteger True [0, 0, 4]) 4#) `shouldBe` "+0x1000000000000000"
        show (N2.shiftRInteger (N2.mkInteger True [0, 0, 4]) 64#) `shouldBe` "+0x1"
        show (N2.shiftRInteger (N2.mkInteger True [0, 0, 8]) 65#) `shouldBe` "+0x1"
        show (N2.shiftRInteger (N2.mkInteger True [0x7fffffff, 0x7fffffff, 3]) 64#) `shouldBe` "0x0"

        show (N2.shiftRInteger (N2.smallInteger (-1#)) 1#) `shouldBe` "-0x1"
        show (N2.shiftRInteger (N2.smallInteger (-2#)) 1#) `shouldBe` "-0x1"
        show (N2.shiftRInteger (N2.smallInteger (-3#)) 1#) `shouldBe` "-0x2"
        show (N2.shiftRInteger (N2.smallInteger (-4#)) 1#) `shouldBe` "-0x2"
        show (N2.shiftRInteger (N2.smallInteger (-5#)) 1#) `shouldBe` "-0x3"
        show (N2.shiftRInteger (N2.smallInteger (-6#)) 1#) `shouldBe` "-0x3"
        show (N2.shiftRInteger (N2.smallInteger (-7#)) 1#) `shouldBe` "-0x4"

        show (N2.shiftRInteger (N2.smallInteger (-3#)) 3#) `shouldBe` "-0x1"
        show (N2.shiftRInteger (N2.smallInteger (-1#)) 1271#) `shouldBe` "-0x1"
        show (N2.shiftRInteger (N2.smallInteger (-1123123123223#)) 127#) `shouldBe` "-0x1"

        show (N2.shiftRInteger (N2.mkInteger False [0x7ffffffd,0x2]) 2#) `shouldBe` "-0x60000000"
        show (G.shiftRInteger (G.mkInteger False [0,0x2]) 1#) `shouldBe` "-0x80000000"


    prop "Can shiftR Integers by up to 128 bits." $ \ (GNP g s, int) -> do
        let bits = unboxInt (int .&. 0x7f)
        show (N2.shiftRInteger s bits) `shouldBe` show (G.shiftRInteger g bits)

    it "Get correct result at boundaries." $ do
        let maxSmall = Positive (Small 0xffffffffffffffff)
            oneSmall = Positive (Small 1)
            twoSmall = Positive (Small 2)
        show (N2.plusInteger maxSmall oneSmall) `shouldBe` "+0x10000000000000000"
        show (N2.plusInteger oneSmall maxSmall) `shouldBe` "+0x10000000000000000"
        show (N2.timesInteger maxSmall twoSmall) `shouldBe` "+0x1fffffffffffffffe"
        show (N2.timesInteger twoSmall maxSmall) `shouldBe` "+0x1fffffffffffffffe"

        let allBitsSet = N2.mkInteger True (replicate 8 0x7fffffff ++ [0xff])
        show (N2.timesInteger allBitsSet allBitsSet) `shouldBe` "+0x" ++ replicate 63 'f' ++ "e" ++ replicate 63 '0' ++ "1"

        show (N2.complementInteger (N2.smallInteger 0#)) `shouldBe` "-0x1"
        show (N2.complementInteger (N2.mkInteger True [0])) `shouldBe` "-0x1"



{-
================================================================================
timesInteger

================================================================================
plusInteger

================================================================================
minusInteger

================================================================================
ltInteger

================================================================================
-}

{-
    prop "Can OR two Integers." $ \ (GNP ga sa, GNP gb sb) ->
        show (N2.orInteger sa sb) `shouldBe` show (G.orInteger ga gb)
    prop "Can AND two Integers." $ \ (GNP ga na, GNP gb nb) ->
        show (N2.andInteger na nb) `shouldBe` show (G.andInteger ga gb)
    prop "Can XOR two Integers." $ \ (GNP ga na, GNP gb nb) ->
        show (N2.xorInteger na nb) `shouldBe` show (G.xorInteger ga gb)
-}


--------------------------------------------------------------------------------

data GmpNewPair
    = GNP G.Integer N2.Integer
    deriving Show

instance Arbitrary GmpNewPair where
    arbitrary = do
        bool <- arbitrary
        if bool
            then do
                i <- arbitrary
                return $! GNP (G.smallInteger (unboxInt i)) (N2.smallInteger (unboxInt i))
            else do
                sign <- arbitrary
                pos <- positive32bits <$> arbitrary
                return $! GNP (G.mkInteger sign pos) (N2.mkInteger sign pos)

