{-# LANGUAGE FlexibleInstances, ScopedTypeVariables #-}

import Prelude hiding (Integer)

import Control.Applicative ((<$>))
import Data.Bits ((.&.))
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck.Arbitrary


import qualified GMP.Integer as G
import qualified New.Integer as N
import qualified Simple.Integer as S

import New.GHC.Integer.Prim
import New.GHC.Integer.Sign
import New.GHC.Integer.Type

import TestHelpers

main :: IO ()
main = hspec $ do
    describe "Comparing GMP and Simple Integer operations:" testExistingInteger
    describe "Testing low level primitives for New Integer library:" testNewInternal
    describe "Comparing GMP and New Integer operations:" testNewInteger


testExistingInteger :: Spec
testExistingInteger = do
    prop "Can convert from Int." $ \ i ->
        show (G.smallInteger (unboxInt i)) `shouldBe` show (S.smallInteger (unboxInt i))
    prop "Can convert to Int." $ \ (GSP g s) ->
        show (boxIntHash (G.integerToInt g)) `shouldBe` show (boxIntHash (S.integerToInt s))
    prop "Can create Integers." $ \ (GSP g s) ->
        show g `shouldBe` show s
    prop "Can add Integers." $ \ (GSP ga sa, GSP gb sb) ->
        show (ga + gb) `shouldBe` show (sa + sb)
    prop "Can subtract Integers." $ \ (GSP ga sa, GSP gb sb) ->
        show (ga - gb) `shouldBe` show (sa - sb)
    prop "Can multiply Integers." $ \ (GSP ga sa, GSP gb sb) ->
        show (ga * gb) `shouldBe` show (sa * sb)
    prop "Can negate Integers." $ \ (GSP g s) -> do
        show (G.negateInteger g) `shouldBe` show (S.negateInteger s)
        show (g + G.negateInteger g) `shouldBe` "0x0"
        show (s + S.negateInteger s) `shouldBe` "0x0"
    prop "Can OR two Integers." $ \ (GSP ga sa, GSP gb sb) ->
        show (G.orInteger ga gb) `shouldBe` show (S.orInteger sa sb)
    prop "Can AND two Integers." $ \ (GSP ga sa, GSP gb sb) ->
        show (G.andInteger ga gb) `shouldBe` show (S.andInteger sa sb)
    prop "Can XOR two Integers." $ \ (GSP ga sa, GSP gb sb) ->
        show (G.xorInteger ga gb) `shouldBe` show (S.xorInteger sa sb)
    prop "Can hash an Integer." $ \ (GSP g s) ->
        show (boxIntHash (G.hashInteger g)) `shouldBe` show (boxIntHash (S.hashInteger s))
    prop "Can shiftL Integers." $ \ (GSP g s, int) ->
        let bits = unboxInt (int .&. 31)
        in show (G.shiftLInteger g bits) `shouldBe` show (S.shiftLInteger s bits)


testNewInternal :: Spec
testNewInternal = do
    prop "Can split and make FullWords." $ \ fw ->
        fw == makeFullWord (splitFullWord fw)
    prop "Can add HalfWords." $ \ (h1, h2) ->
        let f1 = makeFullWord (plusHalfWord h1 h2)
            f2 = (promoteHalfWord h1) + (promoteHalfWord h2)
        in f1 `shouldBe` f2
    prop "Can add HalfWords with a carry." $ \ (h1, h2, hc) ->
        let f1 = makeFullWord (plusHalfWordC h1 h2 hc)
            f2 = (promoteHalfWord h1) + (promoteHalfWord h2) + (promoteHalfWord hc)
        in f1 `shouldBe` f2
    prop "Can subtract HalfWords." $ \ (h1, h2) ->
        let f1 = makeFullWord (minusHalfWord h1 h2)
            f2 = ((promoteHalfWord h1) - (promoteHalfWord h2)) .&. 0x1ffffffff
        in f1 `shouldBe` f2
    prop "Can multiply HalfWords." $ \ (h1, h2) ->
        let f1 = makeFullWord (timesHalfWord h1 h2)
            f2 = (promoteHalfWord h1) * (promoteHalfWord h2)
        in f2 `shouldBe` f1
    prop "Can multiply HalfWords with a carry." $ \ (h1, h2, hc) ->
        let f1 = makeFullWord (timesHalfWordC h1 h2 hc)
            f2 = (promoteHalfWord h1) * (promoteHalfWord h2) + (promoteHalfWord hc)
        in f2 `shouldBe` f1

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


testNewInteger :: Spec
testNewInteger = do
    prop "Can convert from Int." $ \ i ->
        show (N.smallInteger (unboxInt i)) `shouldBe` show (G.smallInteger (unboxInt i))
    prop "Can convert to Int." $ \ i ->
        boxIntHash (N.integerToInt (N.smallInteger (unboxInt i))) `shouldBe` i

    it "Can create Integers." $ do
        show (N.mkInteger True [0x7fffffff, 0x7fffffff, 0x3f]) `shouldBe` "+0xfffffffffffffffff"
    prop "Can create Integers." $ \ (GNP g s) ->
        show g == show s

    prop "Can complement an Integer." $ \ (GNP g s) ->
        show (N.complementInteger s) `shouldBe` show (G.complementInteger g)
    prop "Can negate an Integer." $ \ (GNP g s) ->
        show (N.negateInteger s) `shouldBe` show (G.negateInteger g)
    prop "Can take abs of an Integer." $ \ (GNP g s) ->
        show (N.absInteger s) `shouldBe` show (G.absInteger g)

    it "Can shiftL Integers." $ do
        let s = N.smallInteger 0x12345#
        show (N.shiftLInteger s 4#) `shouldBe` "+0x123450"
        show (N.shiftLInteger s 64#) `shouldBe` "+0x123450000000000000000"
        show (N.shiftLInteger s 68#) `shouldBe` "+0x1234500000000000000000"
        show (N.shiftLInteger (N.mkInteger False [0x7fffffff]) 127#) `shouldBe` "-0x3fffffff80000000000000000000000000000000"

    prop "Can shiftL Integers by up to 128 bits." $ \ (GNP g s, int) -> do
        let bits = unboxInt (int .&. 0x7f)
        show (N.shiftLInteger s bits) `shouldBe` show (G.shiftLInteger g bits)

    prop "Can compare > two Integers." $ \ (GNP ga sa, GNP gb sb) ->
        N.gtInteger sa sb `shouldBe` G.gtInteger ga gb
    prop "Can compare < two Integers." $ \ (GNP ga sa, GNP gb sb) ->
        N.ltInteger sa sb `shouldBe` G.ltInteger ga gb
    prop "Can compare >= two Integers." $ \ (GNP ga sa, GNP gb sb) ->
        N.geInteger sa sb `shouldBe` G.geInteger ga gb
    prop "Can compare <= two Integers." $ \ (GNP ga sa, GNP gb sb) ->
        N.leInteger sa sb `shouldBe` G.leInteger ga gb

    prop "Can add two Integers." $ \ (GNP ga sa, GNP gb sb) ->
        show (N.plusInteger sa sb) `shouldBe` show (G.plusInteger ga gb)
    prop "Can subtract two Integers." $ \ (GNP ga sa, GNP gb sb) ->
        show (N.minusInteger sa sb) `shouldBe` show (G.minusInteger ga gb)

    prop "Can AND two positive Integers." $ \ (GNP ga sa, GNP gb sb) ->
        show (N.andInteger (N.absInteger sa) (N.absInteger sb)) `shouldBe` show (G.andInteger (G.absInteger ga) (G.absInteger gb))
    prop "Can OR two positive Integers." $ \ (GNP ga sa, GNP gb sb) ->
        show (N.orInteger (N.absInteger sa) (N.absInteger sb)) `shouldBe` show (G.orInteger (G.absInteger ga) (G.absInteger gb))

    it "Can multiply two small Integers." $ do
        show (N.timesInteger (N.smallInteger 0x100#) (N.smallInteger 0x22#)) `shouldBe` "+0x2200"
        show (N.timesInteger (N.smallInteger -0x100#) (N.smallInteger 0x22#)) `shouldBe` "-0x2200"
        show (N.timesInteger (N.smallInteger 0x100#) (N.smallInteger -0x22#)) `shouldBe` "-0x2200"
        show (N.timesInteger (N.smallInteger -0x100#) (N.smallInteger -0x22#)) `shouldBe` "+0x2200"

    it "Can multiply large Integer by small." $ do
        show (N.timesInteger (N.mkInteger True [0,2]) (N.smallInteger 0x22#)) `shouldBe` "+0x2200000000"
        show (N.timesInteger (N.mkInteger False [0,2]) (N.smallInteger 0x22#)) `shouldBe` "-0x2200000000"
        show (N.timesInteger (N.mkInteger True [0,2]) (N.smallInteger -0x22#)) `shouldBe` "-0x2200000000"
        show (N.timesInteger (N.mkInteger False [0,2]) (N.smallInteger -0x22#)) `shouldBe` "+0x2200000000"

    it "Can multiply two Integers (old failures)." $ do
        show (N.timesInteger (N.mkInteger True [1, 2, 4]) (N.mkInteger True [1, 2])) `shouldBe` "+0x1000000020000000200000001"
        show (N.timesInteger (N.mkInteger True [1, 2, 4, 8]) (N.mkInteger True [1, 2, 4, 8])) `shouldBe` "+0x1000000020000000300000004000000030000000200000001"
        show (N.timesInteger (N.mkInteger True [0x7ffffffe, 0x7ffffffe, 4]) (N.mkInteger True [0x7ffffffe, 0x7ffffffe, 4])) `shouldBe` "+0x18ffffffebffffffb4000000200000004"
        show (N.timesInteger (N.mkInteger False [0, 0x7fffffff]) (N.mkInteger False [0, 0xfffffffe])) `shouldBe` "+0x1fffffff800000008000000000000000"
        show (N.timesInteger (N.mkInteger False [1, 0x7fffffff]) (N.mkInteger False [1, 0xfffffffe])) `shouldBe` "+0x1fffffff800000013ffffffe80000001"
        show (N.timesInteger (N.mkInteger False [0x3b129743, 0x6b866650]) (N.mkInteger False [0x18865e53,0x6295e0a])) `shouldBe` "+0xa5a19af9c4da2c1eaac6f46fa3a4b9"
        show (N.timesInteger (N.mkInteger True [1, 1, 6]) (N.mkInteger True [1, 1, 6])) `shouldBe` "+0x240000001800000034000000100000001"
        show (N.timesInteger (N.mkInteger True [ 0, 0, 0, 4, 8, 1 ]) (N.mkInteger True [ 0, 0, 0, 8 ])) `shouldBe` "+0x800000080000000800000000000000000000000000000000000000000000000"
        show (N.timesInteger (N.mkInteger True [0, 2, 4, 8, 16, 32]) (N.mkInteger True [0,2])) `shouldBe` "+0x1000000010000000100000001000000010000000000000000"
        show (N.timesInteger (N.mkInteger True [0, 1, 1, 1, 1 , 1]) (N.mkInteger True [0, 1, 0, 0, 1])) `shouldBe` "+0x8000000100000002000000080000001000000010000000200000004000000000000000"
        let a4 = [0x7ffffc3d,0x7ffffdc4,0x7ffffeab]
            b4 = [0x7fffff03,0x7ffffc71,0x7fffff6e,0x7ffffd6d,0x7fffff7a,0x294]
        show (N.timesInteger (N.mkInteger True a4) (N.mkInteger True b4)) `shouldBe` "+0x294fff9232dffebaeebffd6dbf80086cfb001f4c78002d7cc4007c9bc8003b7b7"


    prop "Can multiply two Integers." $ \ (GNP ga sa, GNP gb sb) ->
        show (N.timesInteger sa sb) `shouldBe` show (G.timesInteger ga gb)

    it "Tests that have failed once for no good reason." $ do
        let a1 = [0x1c0f0ec5,0x6f32398e]
            b1 = [0x24d73dd2,0x2cce01af]
        show (N.plusInteger (N.mkInteger False a1) (N.mkInteger True b1)) `shouldBe` show (G.plusInteger (G.mkInteger False a1) (G.mkInteger True b1))
        let a2 = [0x730755a,0x24d01108]
            b2 = [0x13dded6c,0x319f8341]
        show (N.minusInteger (N.mkInteger False a2) (N.mkInteger True b2)) `shouldBe` show (G.minusInteger (G.mkInteger False a2) (G.mkInteger True b2))

    it "Can calculate product [1..n]." $ do
        show (foldl1 N.timesInteger $ map (\x -> N.smallInteger (unboxInt x)) [1..10])
            `shouldBe` show (foldl1 G.timesInteger $ map (\x -> G.smallInteger (unboxInt x)) [1..10])
        show (foldl1 N.timesInteger $ map (\x -> N.smallInteger (unboxInt x)) [1..100])
            `shouldBe` show (foldl1 G.timesInteger $ map (\x -> G.smallInteger (unboxInt x)) [1..100])

    it "Can shiftR Integers." $ do
        show (N.shiftRInteger (N.smallInteger 0x12345#) 4#) `shouldBe` "+0x1234"
        show (N.shiftRInteger (N.mkInteger True [0, 0, 4]) 0#) `shouldBe` "+0x10000000000000000"
        show (N.shiftRInteger (N.mkInteger True [0, 0, 4]) 4#) `shouldBe` "+0x1000000000000000"
        show (N.shiftRInteger (N.mkInteger True [0, 0, 4]) 64#) `shouldBe` "+0x1"
        show (N.shiftRInteger (N.mkInteger True [0, 0, 8]) 65#) `shouldBe` "+0x1"
        show (N.shiftRInteger (N.mkInteger True [0x7fffffff, 0x7fffffff, 3]) 64#) `shouldBe` "0x0"

        show (N.shiftRInteger (N.smallInteger (-1#)) 1#) `shouldBe` "-0x1"
        show (N.shiftRInteger (N.smallInteger (-2#)) 1#) `shouldBe` "-0x1"
        show (N.shiftRInteger (N.smallInteger (-3#)) 1#) `shouldBe` "-0x2"
        show (N.shiftRInteger (N.smallInteger (-4#)) 1#) `shouldBe` "-0x2"
        show (N.shiftRInteger (N.smallInteger (-5#)) 1#) `shouldBe` "-0x3"
        show (N.shiftRInteger (N.smallInteger (-6#)) 1#) `shouldBe` "-0x3"
        show (N.shiftRInteger (N.smallInteger (-7#)) 1#) `shouldBe` "-0x4"

        show (N.shiftRInteger (N.smallInteger (-3#)) 3#) `shouldBe` "-0x1"
        show (N.shiftRInteger (N.smallInteger (-1#)) 1271#) `shouldBe` "-0x1"
        show (N.shiftRInteger (N.smallInteger (-1123123123223#)) 127#) `shouldBe` "-0x1"

        show (N.shiftRInteger (N.mkInteger False [0x7ffffffd,0x2]) 2#) `shouldBe` "-0x60000000"
        show (G.shiftRInteger (G.mkInteger False [0,0x2]) 1#) `shouldBe` "-0x80000000"


    prop "Can shiftR Integers by up to 128 bits." $ \ (GNP g s, int) -> do
        let bits = unboxInt (int .&. 0x7f)
        show (N.shiftRInteger s bits) `shouldBe` show (G.shiftRInteger g bits)

    it "Get correct result at boundaries." $ do
        let maxSmall = Small Pos 0xffffffffffffffff
            oneSmall = Small Pos 1
            twoSmall = Small Pos 2
        show (N.plusInteger maxSmall oneSmall) `shouldBe` "+0x10000000000000000"
        show (N.plusInteger oneSmall maxSmall) `shouldBe` "+0x10000000000000000"
        show (N.timesInteger maxSmall twoSmall) `shouldBe` "+0x1fffffffffffffffe"
        show (N.timesInteger twoSmall maxSmall) `shouldBe` "+0x1fffffffffffffffe"

        let allBitsSet = N.mkInteger True (replicate 8 0x7fffffff ++ [0xff])
        show (N.timesInteger allBitsSet allBitsSet) `shouldBe` "+0x" ++ replicate 63 'f' ++ "e" ++ replicate 63 '0' ++ "1"

        show (N.complementInteger (N.smallInteger 0#)) `shouldBe` "-0x1"
        show (N.complementInteger (N.mkInteger True [0])) `shouldBe` "-0x1"



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
        show (N.orInteger sa sb) `shouldBe` show (G.orInteger ga gb)
    prop "Can AND two Integers." $ \ (GNP ga na, GNP gb nb) ->
        show (N.andInteger na nb) `shouldBe` show (G.andInteger ga gb)
    prop "Can XOR two Integers." $ \ (GNP ga na, GNP gb nb) ->
        show (N.xorInteger na nb) `shouldBe` show (G.xorInteger ga gb)
-}


--------------------------------------------------------------------------------

data GmpSimplePair
    = GSP G.Integer S.Integer
    deriving Show

instance Arbitrary GmpSimplePair where
    arbitrary = do
        bool <- arbitrary
        if bool
            then do
                i <- arbitrary
                return $! GSP (G.smallInteger (unboxInt i)) (S.smallInteger (unboxInt i))
            else do
                sign <- arbitrary
                pos <- positive32bits <$> arbitrary
                return $! GSP (G.mkInteger sign pos) (S.mkInteger sign pos)

data GmpNewPair
    = GNP G.Integer N.Integer
    deriving Show

instance Arbitrary GmpNewPair where
    arbitrary = do
        bool <- arbitrary
        if bool
            then do
                i <- arbitrary
                return $! GNP (G.smallInteger (unboxInt i)) (N.smallInteger (unboxInt i))
            else do
                sign <- arbitrary
                pos <- positive32bits <$> arbitrary
                return $! GNP (G.mkInteger sign pos) (N.mkInteger sign pos)

