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
    prop "Can create Integers." $ \ (GNP g s) ->
        show g == show s

    prop "Can complement an Integer." $ \ (GNP g s) ->
        show (N.complementInteger s) `shouldBe` show (G.complementInteger g)

    prop "Can AND two positive Integers." $ \ (GNP ga sa, GNP gb sb) ->
        show (N.andInteger (N.absInteger sa) (N.absInteger sb)) `shouldBe` show (G.andInteger (G.absInteger ga) (G.absInteger gb))
    prop "Can OR two positive Integers." $ \ (GNP ga sa, GNP gb sb) ->
        show (N.orInteger (N.absInteger sa) (N.absInteger sb)) `shouldBe` show (G.orInteger (G.absInteger ga) (G.absInteger gb))


    prop "Can compare > two Integers." $ \ (GNP ga sa, GNP gb sb) ->
        N.gtInteger sa sb `shouldBe` G.gtInteger ga gb
    prop "Can compare < two Integers." $ \ (GNP ga sa, GNP gb sb) ->
        N.ltInteger sa sb `shouldBe` G.ltInteger ga gb
    prop "Can compare >= two Integers." $ \ (GNP ga sa, GNP gb sb) ->
        N.geInteger sa sb `shouldBe` G.geInteger ga gb
    prop "Can compare <= two Integers." $ \ (GNP ga sa, GNP gb sb) ->
        N.leInteger sa sb `shouldBe` G.leInteger ga gb

    prop "Can add two positive Integers." $ \ (GNP ga sa, GNP gb sb) ->
        show (N.plusInteger (N.absInteger sa) (N.absInteger sb)) `shouldBe` show (G.plusInteger (G.absInteger ga) (G.absInteger gb))
    prop "Can subtract two positive Integers." $ \ (GNP ga sa, GNP gb sb) ->
        show (N.minusInteger (N.absInteger sa) (N.absInteger sb)) `shouldBe` show (G.minusInteger (G.absInteger ga) (G.absInteger gb))

    prop "Can add two Integers." $ \ (GNP ga sa, GNP gb sb) ->
        show (N.plusInteger sa sb) `shouldBe` show (G.plusInteger ga gb)
    prop "Can subtract two Integers." $ \ (GNP ga sa, GNP gb sb) ->
        show (N.minusInteger sa sb) `shouldBe` show (G.minusInteger ga gb)


{-
    prop "Can OR two Integers." $ \ (GNP ga sa, GNP gb sb) ->
        show (N.orInteger sa sb) `shouldBe` show (G.orInteger ga gb)
-}



{-
    prop "Can negate an Integer." $ \ (GNP g s) ->
        show (N.negateInteger s) `shouldBe` show (G.negateInteger g)

    it "Can add two Small Integers." $ do
        show (N.plusInteger (N.smallInteger 0x7fffffffffffffff#) (N.smallInteger 1#)) `shouldBe` "+0xfffffffffffffffe"
        show (N.plusInteger (N.smallInteger 0x7fffffffffffffff#) (N.smallInteger 0x7fffffffffffffff#)) `shouldBe` "+0xfffffffffffffffe"


    it "Can add a Small Integer to a Large." $ do
        show (N.plusInteger (N.mkInteger True [0x7fffff]) (N.smallInteger 1#)) `shouldBe` "+0x800000"
        show (N.plusInteger (N.mkInteger True [0x7fffffff, 0x7fffffff]) (N.smallInteger 1#)) `shouldBe` "+0x4000000000000000"
        show (N.plusInteger (N.mkInteger True [0, 0x80000000]) (N.smallInteger 1#)) `shouldBe` "+0x4000000000000001"

        show (N.plusInteger (N.smallInteger (-1#)) (N.mkInteger False [0, 1]) ) `shouldBe` "-0x80000001"

        show (N.plusInteger (N.mkInteger False [0, 0x7f]) (N.smallInteger 1#)) `shouldBe` "-0x3f7fffffff"


    prop "Can complement an Integer." $ \ (GNP g s) ->
        show (N.complementInteger s) `shouldBe` show (G.complementInteger g)

    prop "Can add two positive Large Integers." $ \ (GNP ga na, GNP gb nb) ->
        show (N.plusInteger (N.absInteger na) (N.absInteger nb)) `shouldBe` show (G.plusInteger (G.absInteger ga) (G.absInteger gb))


    it "Can add two Integers." $ do
        show (N.minusInteger (N.mkInteger False [11, 0x2]) (N.smallInteger (-7#))) `shouldBe` "-0x100000004"


    it "Can subtract a Small Integer from a Large." $ do
        show (N.minusInteger (N.mkInteger True [0x080000]) (N.smallInteger 1#)) `shouldBe` "+0x7ffff"
        show (N.minusInteger (N.mkInteger True [0, 1]) (N.smallInteger 1#)) `shouldBe` "+0x7fffffff"
        show (N.minusInteger (N.mkInteger True [0x7fffffff, 0x7fffffff]) (N.smallInteger 1#)) `shouldBe` "+0x3ffffffffffffffe"
        show (N.minusInteger (N.mkInteger True [0, 0x80000000]) (N.smallInteger 2#)) `shouldBe` "+0x3ffffffffffffffe"

    it "Can subtract two positive Large Integers." $ do
        show (N.minusInteger (N.mkInteger True [0,2]) (N.mkInteger True [0,1])) `shouldBe` "+0x80000000"
        show (N.minusInteger (N.mkInteger True [0,3]) (N.mkInteger True [0,1])) `shouldBe` "+0x100000000"
        show (N.minusInteger (N.mkInteger True [0,0,1]) (N.mkInteger True [0,1])) `shouldBe` "+0x3fffffff80000000"
        show (N.minusInteger (N.mkInteger False [1]) (N.mkInteger False [2])) `shouldBe` "+0x0"
        show (N.minusInteger (N.mkInteger False [0,11]) (N.mkInteger True [0,0,2])) `shouldBe` "-0x8000000580000000"


    prop "Can subtract two positive Large Integers." $ \ (GNP ga na, GNP gb nb) ->
        show (N.minusInteger (N.absInteger na) (N.absInteger nb)) `shouldBe` show (G.minusInteger (G.absInteger ga) (G.absInteger gb))

    prop "Can add two Large Integers." $ \ (GNP ga na, GNP gb nb) ->
        show (N.plusInteger na nb) `shouldBe` show (G.plusInteger ga gb)



    prop "Can OR two Integers." $ \ (GNP ga na, GNP gb nb) ->
        show (N.orInteger na nb) `shouldBe` show (G.orInteger ga gb)
-}

{-
    it "Can complement an Integer." $ do
        property $ 2 `shouldBe` (1 :: Int)
        property $ 1 `shouldBe` (1 :: Int)
-}

{-
    prop "Can AND two Integers." $ \ (GNP ga na, GNP gb nb) ->
        show (G.andInteger ga gb) `shouldBe` show (N.andInteger na nb)
    prop "Can XOR two Integers." $ \ (GNP ga na, GNP gb nb) ->
        show (G.xorInteger ga gb) `shouldBe` show (N.xorInteger na nb)

    it "Can shiftL -2 0" $ do
        show (N.shiftLInteger (N.mkInteger False [2]) 0#) `shouldBe`
                show (G.shiftLInteger (G.mkInteger False [2]) 0#)

    it "Can shiftL -0xffffffff 0" $
        show (N.shiftLInteger (N.mkInteger True [0xffffffff]) 0#) `shouldBe`
                show (G.shiftLInteger (G.mkInteger True [0xffffffff]) 0#)

    it "Can shiftL 0 1" $ do
        show (N.shiftLInteger (N.mkInteger True [0]) 1#) `shouldBe`
                show (G.shiftLInteger (G.mkInteger True [0]) 1#)

    prop "Can shiftL Integers." $ \ (GNP g s, int) -> do
        let bits = unboxInt (int .&. 31)
        show (G.shiftLInteger g bits) == show (N.shiftLInteger s bits)

    it "Can shiftL 0x7fffffff00000001 2" $ do
        show (N.shiftLInteger (N.mkInteger True [0x7fffffff00000001]) 2#) `shouldBe`
                show (G.shiftLInteger (G.mkInteger True [0x7fffffff00000001]) 2#)

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
