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
            f2 = (promoteHalfWord h1) - (promoteHalfWord h2)
        in f1 `shouldBe` f2
    prop "Can multiply HalfWords." $ \ (h1, h2) ->
        let f1 = makeFullWord (timesHalfWord h1 h2)
            f2 = (promoteHalfWord h1) * (promoteHalfWord h2)
        in f2 `shouldBe` f1


testNewInteger :: Spec
testNewInteger = do

    prop "Can convert from Int." $ \ i ->
        show (N.smallInteger (unboxInt i)) `shouldBe` show (G.smallInteger (unboxInt i))
    prop "Can convert to Int." $ \ (GNP g s) ->
        show (boxIntHash (N.integerToInt s)) `shouldBe` show (boxIntHash (G.integerToInt g))
    prop "Can negate an Integer." $ \ (GNP g s) ->
        show (N.negateInteger s) `shouldBe` show (G.negateInteger g)
    prop "Can complement an Integer." $ \ (GNP g s) ->
        show (N.complementInteger s) `shouldBe` show (G.complementInteger g)


    it "Can add two Integers." $ do
        show (N.plusInteger (N.mkInteger True [0x7fffff]) (N.smallInteger 1#)) `shouldBe` "+0x800000"
        show (N.plusInteger (N.mkInteger True [0x7fffffff, 0x7fffffff]) (N.smallInteger 1#)) `shouldBe` "+0x4000000000000000"
        show (N.plusInteger (N.mkInteger True [0, 0x80000000]) (N.smallInteger 1#)) `shouldBe` "+0x4000000000000001"


    prop "Can OR two Integers." $ \ (GNP ga na, GNP gb nb) ->
        show (N.orInteger na nb) `shouldBe` show (G.orInteger ga gb)

{-
    prop "Can AND two Integers." $ \ (GNP ga na, GNP gb nb) ->
        show (G.andInteger ga gb) `shouldBe` show (N.andInteger na nb)
    prop "Can XOR two Integers." $ \ (GNP ga na, GNP gb nb) ->
        show (G.xorInteger ga gb) `shouldBe` show (N.xorInteger na nb)
-}

{-
    prop "Can create Integers." $ \ (GNP g s) ->
        show g == show s
-}

{-
    it "Can shiftL -2 0" $ do
        show (N.shiftLInteger (N.mkInteger False [2]) 0#) `shouldBe`
                show (G.shiftLInteger (G.mkInteger False [2]) 0#)

    it "Can shiftL -0xffffffff 0" $
        show (N.shiftLInteger (N.mkInteger True [0xffffffff]) 0#) `shouldBe`
                show (G.shiftLInteger (G.mkInteger True [0xffffffff]) 0#)

    it "Can shiftL 0 1" $ do
        show (N.shiftLInteger (N.mkInteger True [0]) 1#) `shouldBe`
                show (G.shiftLInteger (G.mkInteger True [0]) 1#)
-}
{-
    prop "Can shiftL Integers." $ \ (GNP g s, int) -> do
        let bits = unboxInt (int .&. 31)
        show (G.shiftLInteger g bits) == show (N.shiftLInteger s bits)
-}



{-




    it "Can shiftL 0x7fffffff00000001 2" $ do
        show (N.shiftLInteger (N.mkInteger True [0x7fffffff00000001]) 2#) `shouldBe`
                show (G.shiftLInteger (G.mkInteger True [0x7fffffff00000001]) 2#)


    it "Can load False 0x7fffffff00000001" $ do
        show (N.mkInteger False [0x7fffffff00000001]) `shouldBe` "-0x7fffffff00000001"

    it "Can shiftL 0x7fffffff00000001 1" $ do
        show (N.shiftLInteger (N.mkInteger False [0x7fffffff00000001]) 1#) `shouldBe`
                show (G.shiftLInteger (G.mkInteger False [0x7fffffff00000001]) 1#)

    it "Can shiftL 0x7fffffff00000001 2" $ do
        show (N.shiftLInteger (N.mkInteger False [0x7fffffff00000001]) 2#) `shouldBe`
                show (G.shiftLInteger (G.mkInteger False [0x7fffffff00000001]) 2#)


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
