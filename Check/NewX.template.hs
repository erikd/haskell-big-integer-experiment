{-# LANGUAGE CPP, FlexibleInstances, ScopedTypeVariables #-}
module Check.NewX
    ( GmpNewPair (..)
    , testNewInteger
    ) where

import Prelude hiding (Integer)

import Data.Bits ((.&.), shiftR)
import Data.List (intercalate)
import Numeric (showHex)
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Modifiers

import qualified GMP.Integer as G
import qualified NewX.GHC.Integer.Type as X
import qualified NewX.Integer as X

import Common.GHC.Integer.Prim

import Check.Helpers

#define NewX   1

import GHC.Int (Int32)


testNewInteger :: Spec
testNewInteger = do
    prop "Can convert from Int." $ \ i ->
        show (X.smallInteger (unboxInt i)) `shouldBe` show (G.smallInteger (unboxInt i))
    prop "Can convert to Int." $ \ i ->
        boxInt# (X.integerToInt (X.smallInteger (unboxInt i))) `shouldBe` i

    it "Can create Integers." $
        show (X.mkInteger True [0x7fffffff, 0x7fffffff, 0x3f]) `shouldBe` "+0xfffffffffffffffff"
    prop "Can create Integers." $ \ (GNP g s) ->
        show g == show s

    prop "Can complement an Integer." $ \ (GNP g s) ->
        show (X.complementInteger s) `shouldBe` show (G.complementInteger g)
    prop "Can negate an Integer." $ \ (GNP g s) ->
        show (X.negateInteger s) `shouldBe` show (G.negateInteger g)
    prop "Can take abs of an Integer." $ \ (GNP g s) ->
        show (X.absInteger s) `shouldBe` show (G.absInteger g)

    it "Can shiftL known Integers." $ do
        let s = X.smallInteger 0x12345#
        show (X.shiftLInteger s 4#) `shouldBe` "+0x123450"
        show (X.shiftLInteger s 64#) `shouldBe` "+0x123450000000000000000"
        show (X.shiftLInteger s 68#) `shouldBe` "+0x1234500000000000000000"
        show (X.shiftLInteger (X.mkInteger False [0x7fffffff]) 127#) `shouldBe` "-0x3fffffff80000000000000000000000000000000"
        let big = X.mkInteger True [ 1, 1, 1, 1, 1, 1, 1, 1, 1]
        show (X.shiftLInteger big 0#) `shouldBe` show big

    prop "Can shiftL Integers by up to 256 bits." $ \ (GNP g s, int) -> do
        let bits = unboxInt (int .&. 0x7f)
        show (X.shiftLInteger s bits) `shouldBe` show (G.shiftLInteger g bits)

    prop "Can compare two Integers equal." $ \ (GNP ga sa, GNP gb sb) ->
        (X.eqInteger sa sb, X.eqInteger sa sa) `shouldBe` (G.eqInteger ga gb, G.eqInteger ga ga)
    prop "Can compare two Integers not equal." $ \ (GNP ga sa, GNP gb sb) ->
        (X.neqInteger sa sb, X.neqInteger sa sa) `shouldBe` (G.neqInteger ga gb, G.neqInteger ga ga)

    prop "Can compare > two Integers." $ \ (GNP ga sa, GNP gb sb) ->
        X.gtInteger sa sb `shouldBe` G.gtInteger ga gb
    prop "Can compare < two Integers." $ \ (GNP ga sa, GNP gb sb) ->
        X.ltInteger sa sb `shouldBe` G.ltInteger ga gb
    prop "Can compare >= two Integers." $ \ (GNP ga sa, GNP gb sb) ->
        X.geInteger sa sb `shouldBe` G.geInteger ga gb
    prop "Can compare <= two Integers." $ \ (GNP ga sa, GNP gb sb) ->
        X.leInteger sa sb `shouldBe` G.leInteger ga gb

    prop "Can add two Integers." $ \ (GNP ga sa, GNP gb sb) ->
        show (X.plusInteger sa sb) `shouldBe` show (G.plusInteger ga gb)

    prop "Can subtract two Integers." $ \ (GNP ga sa, GNP gb sb) ->
        show (X.minusInteger sa sb) `shouldBe` show (G.minusInteger ga gb)

    it "Can AND known pairs of positive Integers." $ do
        let a1 = [1,0,0,0,0,0,0,0,0,0,0,1]
            b1 = [1,0,0,0,0,0,0,0,0,0,0,0,0,0,1]
        show (X.andInteger (X.mkInteger True a1) (X.mkInteger True b1)) `shouldBe` "+0x1"
        let a2 = [0,0,0,0,0,0,0,0,0,0,0,1]
            b2 = [0,0,0,0,0,0,0,0,0,0,0,0,0,0,1]
        show (X.andInteger (X.mkInteger True a2) (X.mkInteger True b2)) `shouldBe` "0x0"

    prop "Can AND two positive Integers." $ \ (GNP ga sa, GNP gb sb) ->
        show (X.andInteger (X.absInteger sa) (X.absInteger sb)) `shouldBe` show (G.andInteger (G.absInteger ga) (G.absInteger gb))
    prop "Can OR two positive Integers." $ \ (GNP ga sa, GNP gb sb) ->
        show (X.orInteger (X.absInteger sa) (X.absInteger sb)) `shouldBe` show (G.orInteger (G.absInteger ga) (G.absInteger gb))

    it "Can multiply two small Integers." $ do
        show (X.timesInteger (X.smallInteger 0x100#) (X.smallInteger 0x22#)) `shouldBe` "+0x2200"
        show (X.timesInteger (X.smallInteger -0x100#) (X.smallInteger 0x22#)) `shouldBe` "-0x2200"
        show (X.timesInteger (X.smallInteger 0x100#) (X.smallInteger -0x22#)) `shouldBe` "-0x2200"
        show (X.timesInteger (X.smallInteger -0x100#) (X.smallInteger -0x22#)) `shouldBe` "+0x2200"

    it "Can multiply large Integer by small." $ do
        show (X.timesInteger (X.mkInteger True [0,2]) (X.smallInteger 0x22#)) `shouldBe` "+0x2200000000"
        show (X.timesInteger (X.mkInteger False [0,2]) (X.smallInteger 0x22#)) `shouldBe` "-0x2200000000"
        show (X.timesInteger (X.mkInteger True [0,2]) (X.smallInteger -0x22#)) `shouldBe` "-0x2200000000"
        show (X.timesInteger (X.mkInteger False [0,2]) (X.smallInteger -0x22#)) `shouldBe` "+0x2200000000"

    it "Can multiply two Integers (old failures)." $ do
        show (X.timesInteger (X.mkInteger True [1, 2, 4]) (X.mkInteger True [1, 2])) `shouldBe` "+0x1000000020000000200000001"
        show (X.timesInteger (X.mkInteger True [1, 2, 4, 8]) (X.mkInteger True [1, 2, 4, 8])) `shouldBe` "+0x1000000020000000300000004000000030000000200000001"
        show (X.timesInteger (X.mkInteger True [0x7ffffffe, 0x7ffffffe, 4]) (X.mkInteger True [0x7ffffffe, 0x7ffffffe, 4])) `shouldBe` "+0x18ffffffebffffffb4000000200000004"
        show (X.timesInteger (X.mkInteger False [0, 0x7fffffff]) (X.mkInteger False [0, 0xfffffffe])) `shouldBe` "+0x1fffffff800000008000000000000000"
        show (X.timesInteger (X.mkInteger False [1, 0x7fffffff]) (X.mkInteger False [1, 0xfffffffe])) `shouldBe` "+0x1fffffff800000013ffffffe80000001"
        show (X.timesInteger (X.mkInteger False [0x3b129743, 0x6b866650]) (X.mkInteger False [0x18865e53,0x6295e0a])) `shouldBe` "+0xa5a19af9c4da2c1eaac6f46fa3a4b9"
        show (X.timesInteger (X.mkInteger True [1, 1, 6]) (X.mkInteger True [1, 1, 6])) `shouldBe` "+0x240000001800000034000000100000001"
        show (X.timesInteger (X.mkInteger True [ 0, 0, 0, 4, 8, 1 ]) (X.mkInteger True [ 0, 0, 0, 8 ])) `shouldBe` "+0x800000080000000800000000000000000000000000000000000000000000000"
        show (X.timesInteger (X.mkInteger True [0, 2, 4, 8, 16, 32]) (X.mkInteger True [0,2])) `shouldBe` "+0x1000000010000000100000001000000010000000000000000"
        show (X.timesInteger (X.mkInteger True [0, 1, 1, 1, 1 , 1]) (X.mkInteger True [0, 1, 0, 0, 1])) `shouldBe` "+0x8000000100000002000000080000001000000010000000200000004000000000000000"
        let a4 = [0x7ffffc3d,0x7ffffdc4,0x7ffffeab]
            b4 = [0x7fffff03,0x7ffffc71,0x7fffff6e,0x7ffffd6d,0x7fffff7a,0x294]
        show (X.timesInteger (X.mkInteger True a4) (X.mkInteger True b4)) `shouldBe` "+0x294fff9232dffebaeebffd6dbf80086cfb001f4c78002d7cc4007c9bc8003b7b7"
        let a5 = [1,0,4,0,16,0,64,0,256,0,1024]
            b5 = [1,0,0,0,32,0,192,0,1024]
        show (X.timesInteger (X.mkInteger True a5) (X.mkInteger True b5)) `shouldBe` "+0x4000000000000000700000000000000090000000000000009000000000000000a000000000000000a0000000000000006000000000000000300000000000000010000000000000001"
        let a6 = [0x1,0,0x3,0,0xc]
            b6 = [0x2,0,0x3,0,0xe]
        show (X.timesInteger (X.mkInteger True a6) (X.mkInteger True b6)) `shouldBe` show (G.timesInteger (G.mkInteger True a6) (G.mkInteger True b6))

    prop "Can multiply two Integers." $ \ (GNP ga sa, GNP gb sb) ->
        show (X.timesInteger sa sb) `shouldBe` show (G.timesInteger ga gb)

    it "Tests that have failed once for no good reason." $ do
        let a1 = [0x1c0f0ec5,0x6f32398e]
            b1 = [0x24d73dd2,0x2cce01af]
        show (X.plusInteger (X.mkInteger False a1) (X.mkInteger True b1)) `shouldBe` show (G.plusInteger (G.mkInteger False a1) (G.mkInteger True b1))
        let a2 = [0x730755a,0x24d01108]
            b2 = [0x13dded6c,0x319f8341]
        show (X.minusInteger (X.mkInteger False a2) (X.mkInteger True b2)) `shouldBe` show (G.minusInteger (G.mkInteger False a2) (G.mkInteger True b2))
        let a3 = [0x4,0x0,0x0,0x1]
            b3 = [0x0,0x1,0x0,0x1]
        show (X.minusInteger (X.mkInteger True a3) (X.mkInteger True b3)) `shouldBe` show (G.minusInteger (G.mkInteger True a3) (G.mkInteger True b3))


    it "Can calculate product [1..n]." $ do
        show (foldl1 X.timesInteger $ map (\x -> X.smallInteger (unboxInt x)) [1..10])
            `shouldBe` show (foldl1 G.timesInteger $ map (\x -> G.smallInteger (unboxInt x)) [1..10])
        show (foldl1 X.timesInteger $ map (\x -> X.smallInteger (unboxInt x)) [1..100])
            `shouldBe` show (foldl1 G.timesInteger $ map (\x -> G.smallInteger (unboxInt x)) [1..100])

    it "Can shiftR known Integers." $ do
        show (X.shiftRInteger (X.smallInteger 0x12345#) 4#) `shouldBe` "+0x1234"
        show (X.shiftRInteger (X.mkInteger True [0, 0, 4]) 0#) `shouldBe` "+0x10000000000000000"
        show (X.shiftRInteger (X.mkInteger True [0, 0, 4]) 4#) `shouldBe` "+0x1000000000000000"
        show (X.shiftRInteger (X.mkInteger True [0, 0, 4]) 64#) `shouldBe` "+0x1"
        show (X.shiftRInteger (X.mkInteger True [0, 0, 8]) 65#) `shouldBe` "+0x1"
        show (X.shiftRInteger (X.mkInteger True [0x7fffffff, 0x7fffffff, 3]) 64#) `shouldBe` "0x0"

        show (X.shiftRInteger (X.smallInteger (-1#)) 1#) `shouldBe` "-0x1"
        show (X.shiftRInteger (X.smallInteger (-2#)) 1#) `shouldBe` "-0x1"
        show (X.shiftRInteger (X.smallInteger (-3#)) 1#) `shouldBe` "-0x2"
        show (X.shiftRInteger (X.smallInteger (-4#)) 1#) `shouldBe` "-0x2"
        show (X.shiftRInteger (X.smallInteger (-5#)) 1#) `shouldBe` "-0x3"
        show (X.shiftRInteger (X.smallInteger (-6#)) 1#) `shouldBe` "-0x3"
        show (X.shiftRInteger (X.smallInteger (-7#)) 1#) `shouldBe` "-0x4"

        show (X.shiftRInteger (X.smallInteger (-3#)) 3#) `shouldBe` "-0x1"
        show (X.shiftRInteger (X.smallInteger (-1#)) 1271#) `shouldBe` "-0x1"
        show (X.shiftRInteger (X.smallInteger (-1123123123223#)) 127#) `shouldBe` "-0x1"

        show (X.shiftRInteger (X.mkInteger False [0x7ffffffd,0x2]) 2#) `shouldBe` "-0x60000000"
        show (X.shiftRInteger (X.mkInteger False [0,0x2]) 1#) `shouldBe` "-0x80000000"
        show (X.shiftRInteger (X.mkInteger False [0x16a3153f,0xb08fa82]) 64#) `shouldBe` "-0x1"
        show (X.shiftRInteger (X.mkInteger False [0x7fffe63e,0x7ffff16e,0x122e,0x7ffff58f]) 155#) `shouldBe` "-0x1"
        show (X.shiftRInteger (X.mkInteger True [0, 0, 0x594b]) 77#) `shouldBe` "0x0"


    prop "Can shiftR Integers by up to 256 bits." $ \ (GNP g s, shift) -> do
        let bits = shiftCount shift
        show (X.shiftRInteger s bits) `shouldBe` show (G.shiftRInteger g bits)

    it "Get correct result at boundaries." $ do
        let maxSmall = X.wordToInteger (unboxWord 0xffffffffffffffff)
            oneSmall = X.wordToInteger (unboxWord 1)
            twoSmall = X.wordToInteger (unboxWord 2)
        show (X.plusInteger maxSmall oneSmall) `shouldBe` "+0x10000000000000000"
        show (X.plusInteger oneSmall maxSmall) `shouldBe` "+0x10000000000000000"
        show (X.timesInteger maxSmall twoSmall) `shouldBe` "+0x1fffffffffffffffe"
        show (X.timesInteger twoSmall maxSmall) `shouldBe` "+0x1fffffffffffffffe"

        let allBitsSet = X.mkInteger True (replicate 8 0x7fffffff ++ [0xff])
        show (X.timesInteger allBitsSet allBitsSet) `shouldBe` "+0x" ++ replicate 63 'f' ++ "e" ++ replicate 63 '0' ++ "1"

        show (X.complementInteger (X.smallInteger 0#)) `shouldBe` "-0x1"
        show (X.complementInteger (X.mkInteger True [])) `shouldBe` "-0x1"
        show (X.complementInteger (X.mkInteger True [0])) `shouldBe` "-0x1"

        show (X.mkInteger True [1,0,0,0,0,0,0,0,0,0,0,0,0,0,1]) `shouldBe` show (G.mkInteger True [1,0,0,0,0,0,0,0,0,0,0,0,0,0,1])

    it "mkInteger results are minimal." $ do
        X.isMinimal (X.mkInteger True [1, 2]) `shouldBe` True
        X.isMinimal (X.mkInteger True [1, 2, 4]) `shouldBe` True

    prop "mkInteger results are minimal (QC)." $ \ (GNP _ a) ->
        X.isMinimal a `shouldBe` True

    it "Addition results are minimal." $
        X.isMinimal (X.plusInteger (X.mkInteger True [0x3,0x0,0x4]) (X.mkInteger False [0x3,0x2])) `shouldBe` True

    prop "Addition results are minimal (QC)." $ \ (GNP _ a, GNP _ b) ->
        X.isMinimal (X.plusInteger a b) `shouldBe` True

    prop "Muliplication results are minimal." $ \ (GNP _ a, GNP _ b) ->
        X.isMinimal (X.timesInteger a b) `shouldBe` True

    prop "ShiftL results are minimal." $ \ (GNP _ s, int) -> do
        let bits = unboxInt (int .&. 0x7f)
        X.isMinimal (X.shiftLInteger s bits) `shouldBe` True

    it "Can encode to Double." $ do
        boxDouble# (X.encodeDoubleInteger (X.smallInteger 3333#) 0#) `shouldBe` boxDouble# (G.encodeDoubleInteger (G.smallInteger 3333#) 0#)
        boxDouble# (X.encodeDoubleInteger (X.mkInteger True [1,2,4,8]) 0#) `shouldBe` boxDouble# (G.encodeDoubleInteger (G.mkInteger True [1,2,4,8]) 0#)
        let a = [0x1f,0x8,0x14,0x4,0x1e,0x5,0x8,0xd,0x4,0x1,0x18,0xb,0xa,0xb,0x3,0xc,0xd,0xd,0x1a,0x1,0x1d,0x4,0x8,0x4,0x11,0x2,0x5,0x6,0x2,0x13]
        boxDouble# (X.encodeDoubleInteger (X.mkInteger True a) (-1542#)) `shouldBe` boxDouble# (G.encodeDoubleInteger (G.mkInteger True a) (-1542#))

    prop "Can encode to Double (QC)." $ \ (GNP g n) (int :: Int32) -> do
        let i = unboxInt (fromIntegral int)
        boxDouble# (X.encodeDoubleInteger n i) `shouldBe` boxDouble# (G.encodeDoubleInteger g i)

    prop "Can decode Double to (Integer, Int)." $ \ d ->
        let (# xb, xe #) = X.decodeDoubleInteger (unboxDouble d)
            (# gb, ge #) = G.decodeDoubleInteger (unboxDouble d)
        in show (xb, boxInt# xe) `shouldBe` show (gb, boxInt# ge)

#if New4
    prop "Can quotRemInteger small Integers." $ \ (a, NonZero b) -> do
        let (ga, gb) = (G.smallInteger (unboxInt a), G.smallInteger (unboxInt b))
            (xa, xb) = (X.smallInteger (unboxInt a), X.smallInteger (unboxInt b))
        show (boxTuple (X.quotRemInteger xa xb)) `shouldBe` show (boxTuple (G.quotRemInteger ga gb))

    prop "Can divModInteger small Integers." $ \ (a, NonZero b) -> do
        let (ga, gb) = (G.smallInteger (unboxInt a), G.smallInteger (unboxInt b))
            (xa, xb) = (X.smallInteger (unboxInt a), X.smallInteger (unboxInt b))
        show (boxTuple (X.divModInteger xa xb)) `shouldBe` show (boxTuple (G.divModInteger ga gb))

    it "Can quotRemInteger big by small Integers." $ do
        let a = [ 0, 0, 0x5 ]
            (xa, ga) = (X.mkInteger True a, G.mkInteger True a)
            (xb, gb) = (X.smallInteger (unboxInt 1), G.smallInteger (unboxInt 1))
        show (boxTuple (X.quotRemInteger xa xb)) `shouldBe` show (boxTuple (G.quotRemInteger ga gb))

    it "Can divModInteger big by small Integers." $ do
        let a = [ 0, 0, 0x5 ]
            (xa, ga) = (X.mkInteger True a, G.mkInteger True a)
            (xb, gb) = (X.smallInteger (unboxInt 1), G.smallInteger (unboxInt 1))
        show (boxTuple (X.divModInteger xa xb)) `shouldBe` show (boxTuple (G.divModInteger ga gb))

    prop "Can quotRemInteger big by small Integers (QC)." $ \ (GNP ga xa, NonZero b) -> do
        let (gb, xb) = (G.smallInteger (unboxInt b), X.smallInteger (unboxInt b))
        show (boxTuple (X.quotRemInteger xa xb)) `shouldBe` show (boxTuple (G.quotRemInteger ga gb))

    prop "Can quotRemInteger big by small Integers (QC)." $ \ (GNP ga xa, NonZero b) -> do
        let (gb, xb) = (G.smallInteger (unboxInt b), X.smallInteger (unboxInt b))
        show (boxTuple (X.quotRemInteger xa xb)) `shouldBe` show (boxTuple (G.quotRemInteger ga gb))

    prop "Can divModInteger big by small Integers (QC)." $ \ (GNP ga xa, NonZero b) -> do
        let (gb, xb) = (G.smallInteger (unboxInt b), X.smallInteger (unboxInt b))
        show (boxTuple (X.divModInteger xa xb)) `shouldBe` show (boxTuple (G.divModInteger ga gb))

    it "Can quotRemInteger big Integers." $ do
        let a = [0xb,0x12,0x5,0xa,0x5,0x8,0x2,0x11]
            b = [0x12,0xf]
        show (boxTuple (X.quotRemInteger (X.mkInteger True a) (X.mkInteger True b))) `shouldBe` show (boxTuple (G.quotRemInteger (G.mkInteger True a) (G.mkInteger True b)))

    prop "Can quotRemInteger big Integers (QC)." $ \ (GNP ga xa, GNP gb xb) ->
        show (boxTuple (X.quotRemInteger xa xb)) `shouldBe` show (boxTuple (G.quotRemInteger ga gb))

    prop "Can divModInteger big Integers (QC)." $ \ (GNP ga xa, GNP gb xb) ->
        show (boxTuple (X.divModInteger xa xb)) `shouldBe` show (boxTuple (G.divModInteger ga gb))
#endif


--------------------------------------------------------------------------------

data GmpNewPair
    = GNP G.Integer X.Integer

instance Show GmpNewPair where
    show (GNP g x) =
        if show g /= show x
            then error $ "show GmpNewPair error " ++ show g ++ " /= " ++ show x
            else toMakeInteger x


toMakeInteger :: X.Integer -> String
toMakeInteger xi =
    let s = X.hexShow xi
        nonNeg = case head s of
                    '-' -> False
                    _ -> True
        i = readInteger (if head s == '0' then s else tail s)
    in "mkInteger " ++ show nonNeg ++ " [" ++ wrds i ++ "]"
  where
    wrds i =
        intercalate "," . map (\w -> "0x" ++ showHex w "") $ decompose i
    decompose x
        | x <= 0 = []
        | otherwise =
            x .&. 0x7fffffff : decompose (x `shiftR` 31)

instance Arbitrary GmpNewPair where
    arbitrary = do
        bool <- arbitrary
        if bool
            then do
                i <- fmap getNonZero arbitrary
                return $! GNP (G.smallInteger (unboxInt i)) (X.smallInteger (unboxInt i))
            else do
                sign <- arbitrary
                pos <- fmap (positive32bits . take 30 . nonEmptyNonZero) arbitrary
                return $! GNP (G.mkInteger sign pos) (X.mkInteger sign pos)
    shrink (GNP g x) =
        map (\i -> GNP (G.shiftLInteger g (unboxInt i)) (X.shiftLInteger x (unboxInt i))) [1..50]

newtype NonEmptyNonZero a = NonEmptyNonZero { nonEmptyNonZero :: [a] }

instance (Arbitrary a, Eq a, Num a, Ord a) => Arbitrary (NonEmptyNonZero a) where
    arbitrary = do
        x <- fmap getNonZero arbitrary
        xs <- arbitrary
        return $ NonEmptyNonZero (x:xs)

    shrink (NonEmptyNonZero xs) =
        [ NonEmptyNonZero xs'
        | xs' <- shrink xs
        , not (null xs')
        ]
