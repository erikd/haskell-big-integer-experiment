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
    prop "Can subtract HalfWords." $ \ (h1, h2) ->
        let f1 = makeFullWord (minusHalfWord h1 h2)
            f2 = ((promoteHalfWord h1) - (promoteHalfWord h2)) .&. 0x1ffffffff
        in f1 `shouldBe` f2
    prop "Can multiply HalfWords." $ \ (h1, h2) ->
        let f1 = makeFullWord (timesHalfWord h1 h2)
            f2 = (promoteHalfWord h1) * (promoteHalfWord h2)
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

    it "Can complement an Integer." $
        show (N.complementInteger (N.smallInteger 0#)) `shouldBe` "-0x1"

    prop "Can complement an Integer." $ \ (GNP g s) ->
        show (N.complementInteger s) `shouldBe` show (G.complementInteger g)

    prop "Can negate an Integer." $ \ (GNP g s) ->
        show (N.negateInteger s) `shouldBe` show (G.negateInteger g)

    prop "Can shiftL Integers by up to 128 bits." $ \ (GNP g s, int) -> do
        let bits = unboxInt (int .&. 128)
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
        let a = [0x1,0x40000001,0xf]
        show (N.timesInteger (N.mkInteger True a) (N.mkInteger True a)) `shouldBe` show (G.timesInteger (G.mkInteger True a) (G.mkInteger True a))




{-
    prop "Can multiply two Integers." $ \ (GNP ga sa, GNP gb sb) ->
        show (N.timesInteger sa sb) `shouldBe` show (G.timesInteger ga gb)

    prop "Can multiply two Integers." $ \ (GNP ga sa, GNP gb sb) ->
        show (N.timesInteger sa sb) `shouldBe` show (G.timesInteger ga gb)

    it "can make two" $ do
        show (G.mkInteger False [0x3b129743, 0x6b866650]) `shouldBe` "-0x35c333283b129743"
        show (G.mkInteger False [0x18865e53,0x6295e0a]) `shouldBe` "-0x314af0518865e53"



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
                pos <- (take 3 . pos32bits) <$> arbitrary
                return $! GNP (G.mkInteger sign pos) (N.mkInteger sign pos)

-- The mkInteger functions expect values in range [0, 0x7fffffff].
pos32bits :: [Int] -> [Int]
pos32bits = map (0x7fffffff .&.)
