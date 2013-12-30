{-# LANGUAGE FlexibleInstances, MagicHash, ScopedTypeVariables #-}

import Prelude hiding (Integer)

import Control.Applicative ((<$>))
import Data.Bits ((.&.))
import GHC.Base
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck.Arbitrary


import qualified GMP.Integer as G
import qualified New.Integer as N
import qualified Simple.Integer as S

import New.GHC.Integer.Prim
import New.GHC.Integer.Sign
import New.GHC.Integer.Type


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


    it "Can multiply two Integers A." $ do
        show (G.timesInteger (G.mkInteger True [1, 2, 4]) (G.mkInteger True [1, 2])) `shouldBe` "+0x1000000020000000200000001"
        show (N.timesInteger (N.mkInteger True [1, 2, 4]) (N.mkInteger True [1, 2])) `shouldBe` "+0x1000000020000000200000001"
    it "Can multiply two Integers B." $ do
        show (G.timesInteger (G.mkInteger True [1, 2, 4, 8]) (G.mkInteger True [1, 2, 4, 8])) `shouldBe` "+0x1000000020000000300000004000000030000000200000001"
        show (N.timesInteger (N.mkInteger True [1, 2, 4, 8]) (N.mkInteger True [1, 2, 4, 8])) `shouldBe` "+0x1000000020000000300000004000000030000000200000001"
    it "Can multiply two Integers C." $ do
        show (G.timesInteger (G.mkInteger True [0x7ffffffe, 0x7ffffffe, 4]) (G.mkInteger True [0x7ffffffe, 0x7ffffffe, 4])) `shouldBe` "+0x18ffffffebffffffb4000000200000004"
        show (N.timesInteger (N.mkInteger True [0x7ffffffe, 0x7ffffffe, 4]) (N.mkInteger True [0x7ffffffe, 0x7ffffffe, 4])) `shouldBe` "+0x18ffffffebffffffb4000000200000004"
    it "Can multiply two Integers D." $ do
        show (G.timesInteger (G.mkInteger False [0, 0x7fffffff]) (G.mkInteger False [0, 0xfffffffe])) `shouldBe` "+0x1fffffff800000008000000000000000"
        show (N.timesInteger (N.mkInteger False [0, 0x7fffffff]) (N.mkInteger False [0, 0xfffffffe])) `shouldBe` "+0x1fffffff800000008000000000000000"
    it "Can multiply two Integers E." $ do
        show (G.timesInteger (G.mkInteger False [1, 0x7fffffff]) (G.mkInteger False [1, 0xfffffffe])) `shouldBe` "+0x1fffffff800000013ffffffe80000001"
        show (N.timesInteger (N.mkInteger False [1, 0x7fffffff]) (N.mkInteger False [1, 0xfffffffe])) `shouldBe` "+0x1fffffff800000013ffffffe80000001"
    it "Can multiply two Integers F." $ do
        show (G.timesInteger (G.mkInteger False [0x3b129743, 0x6b866650]) (G.mkInteger False [0x18865e53,0x6295e0a])) `shouldBe` "+0xa5a19af9c4da2c1eaac6f46fa3a4b9"
        show (N.timesInteger (N.mkInteger False [0x3b129743, 0x6b866650]) (N.mkInteger False [0x18865e53,0x6295e0a])) `shouldBe` "+0xa5a19af9c4da2c1eaac6f46fa3a4b9"

    it "Can multiply two Integers G." $ do
        let a = [1, 1, 6]
        show (N.timesInteger (N.mkInteger True a) (N.mkInteger True a)) `shouldBe` show (G.timesInteger (G.mkInteger True a) (G.mkInteger True a))

{-
================================================================================
timesInteger
expected: "+0xf800000067fffffe05fffffcf3fffff09000000bc000000520000024bffffff8ffffffec00000000"
 but got: "+0xf800000067fffffe05fffffbfbfffff29000000bc000000520000024bffffff8ffffffec00000000"
(GNP +0xf7ffffff6ffffffd600000013ffffff300000014 +0xf7ffffff6ffffffd600000013ffffff300000014,GNP +0x100000001000000013ffffffeffffffff00000000 +0x100000001000000013ffffffeffffffff00000000)

expected: "-0xffffff9400000b140000013000002750000064800000ddfffffff57ffffe4800000222000002f4000000c00000000"
 but got: "-0xab0000051200000bdfffffff57ffffe4800000222000002f4000000c00000000"
(GNP -0x1ffffff83ffffffc00000000 -0x1ffffff83ffffffc00000000,GNP +0x7fffffe900000007ffffffa7ffffff57fffffe90000000e0000002fffffff9fffffffd +0x7fffffe900000007ffffffa7ffffff57fffffe90000000e0000002fffffff9fffffffd)

expected: "-0xebffff433ffffca0fffffadbdffff4433ffff7527fff24acfffbbf8dfff6deb00000a8f8002add5000335f80000bdac00003f67fff1ac400048c0400077d3ffff51ddfffdd3ba0002ff680001fecffff9fc200000000"
 but got: "-0xebffff3e6800fc92f00006468001003cbfffde307ffee135fffb05bffffabe0000018bd80013ede000590e000042c03fff59b77ffddaf6000340340003f13ffff51ddfffdd3ba0002ff680001fecffff9fc200000000"
(GNP +0xffffff2e00000098000003affffff20fffffe1ffffffd5bfffff8e800000e4000001e3fffffe67fffff8e000000c2 +0xffffff2e00000098000003affffff20fffffe1ffffffd5bfffff8e800000e4000001e3fffffe67fffff8e000000c2,GNP -0xec000004d800000e1000001e7fffffeb7fffffb4ffffff1d000001b7fffffdf7fffff8100000000 -0xec000004d800000e1000001e7fffffeb7fffffb4ffffff1d000001b7fffffdf7fffff8100000000)

expected: "-0x33bfffffecbffffbc37ffff399fffff39fffffe6c7ffff6df7fffec54ffffdb6fffffa73fffffbbc00000000"
 but got: "-0x6bfffffe300000035a000005c3ffffcfc7ffff6df7fffec54ffffdb6fffffa73fffffbbc00000000"
(GNP -0x11ffffffb5ffffff180000012ffffffddffffff6800000003fffffd6ffffffcc -0x11ffffffb5ffffff180000012ffffffddffffff6800000003fffffd6ffffffcc,GNP +0x2e000000ac000001500000000 +0x2e000000ac000001500000000)

================================================================================
plusInteger

expected: "-0x21321bef7737d0f3"
 but got: "+0x1decde41088c82f0d"
(GNP -0x37991cc71c0f0ec5 -0x37991cc71c0f0ec5,GNP +0x166700d7a4d73dd2 +0x166700d7a4d73dd2)

================================================================================
minusInteger
expected: "+0x667b91c8cad7812"
 but got: "-0x1f99846e3735287ee"
(GNP -0x126808840730755a -0x126808840730755a,GNP -0x18cfc1a093dded6c -0x18cfc1a093dded6c)
================================================================================
ltInteger
expected: False
 but got: True
(GNP 0x0 0x0,GNP 0x0 0x0)
expected: False
 but got: True
(GNP 0x0 0x0,GNP 0x0 0x0)

geInteger
expected: True
 but got: False
(GNP 0x0 0x0,GNP 0x0 0x0)
expected: True
 but got: False
(GNP 0x0 0x0,GNP 0x0 0x0)

================================================================================
shiftRInteger
expected: "-0x5c"
 but got: "-0x5d"
(GNP -0xb8 -0xb8,1153)

expected: "-0x6f95cc9"
 but got: "-0x6f95cca"
(GNP -0x1be57324 -0x1be57324,2)

expected: "-0x4fffffff30000001a000000280000000"
 but got: "-0x4fffffff30000001a000000280000001"
(GNP -0x9ffffffe600000034000000500000000 -0x9ffffffe600000034000000500000000,1)

expected: "-0x1"
 but got: "-0x2"
(GNP -0x2 -0x2,1)
================================================================================

-}

    prop "Can multiply two Integers." $ \ (GNP ga sa, GNP gb sb) ->
        show (N.timesInteger sa sb) `shouldBe` show (G.timesInteger ga gb)

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

        "1 " ++ show (N.shiftRInteger (N.smallInteger (-1#)) 1#) `shouldBe` "1 -0x1"
        show (N.shiftRInteger (N.smallInteger (-2#)) 1#) `shouldBe` "-0x1"
        show (N.shiftRInteger (N.smallInteger (-3#)) 1#) `shouldBe` "-0x2"
        show (N.shiftRInteger (N.smallInteger (-4#)) 1#) `shouldBe` "-0x2"
        show (N.shiftRInteger (N.smallInteger (-5#)) 1#) `shouldBe` "-0x3"
        show (N.shiftRInteger (N.smallInteger (-6#)) 1#) `shouldBe` "-0x3"
        show (N.shiftRInteger (N.smallInteger (-7#)) 1#) `shouldBe` "-0x4"

        "A " ++ show (N.shiftRInteger (N.smallInteger (-3#)) 3#) `shouldBe` "A -0x1"
        "B " ++ show (N.shiftRInteger (N.smallInteger (-1#)) 1271#) `shouldBe` "B -0x1"
        "C " ++ show (N.shiftRInteger (N.smallInteger (-1123123123223#)) 127#) `shouldBe` "C -0x1"

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

{-
    prop "Can OR two Integers." $ \ (GNP ga sa, GNP gb sb) ->
        show (N.orInteger sa sb) `shouldBe` show (G.orInteger ga gb)
    prop "Can AND two Integers." $ \ (GNP ga na, GNP gb nb) ->
        show (N.andInteger na nb) `shouldBe` show (G.andInteger ga gb)
    prop "Can XOR two Integers." $ \ (GNP ga na, GNP gb nb) ->
        show (N.xorInteger na nb) `shouldBe` show (G.xorInteger ga gb)
-}


--------------------------------------------------------------------------------

boxIntHash :: Int# -> Int
boxIntHash i = I# i

unboxInt :: Int -> Int#
unboxInt (I# i) = i

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
                pos <- pos32bits <$> arbitrary
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
                pos <- pos32bits <$> arbitrary
                return $! GNP (G.mkInteger sign pos) (N.mkInteger sign pos)

-- The mkInteger functions expect values in range [0, 0x7fffffff].
pos32bits :: [Int] -> [Int]
pos32bits = map (0x7fffffff .&.)
