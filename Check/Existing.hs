{-# LANGUAGE FlexibleInstances, ScopedTypeVariables #-}

module Check.Existing
    ( testExistingInteger
    ) where

import Prelude hiding (Integer)

import Control.Applicative ((<$>))
import Data.Bits ((.&.))
import GHC.Int
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck.Arbitrary

import Common.GHC.Integer.Prim

import qualified GMP.Integer as G
import qualified Simple.Integer as S

import Check.Helpers


testExistingInteger :: Spec
testExistingInteger = do
    prop "Can convert from Int." $ \ i ->
        show (G.smallInteger (unboxInt i)) `shouldBe` show (S.smallInteger (unboxInt i))
    prop "Can convert to Int." $ \ (GSP g s) ->
        show (boxInt# (G.integerToInt g)) `shouldBe` show (boxInt# (S.integerToInt s))
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
        show (boxInt# (G.hashInteger g)) `shouldBe` show (boxInt# (S.hashInteger s))
    prop "Can shiftL Integers." $ \ (GSP g s, int) ->
        let bits = unboxInt (int .&. 31)
        in show (G.shiftLInteger g bits) `shouldBe` show (S.shiftLInteger s bits)
    prop "Can encode to Double." $ \ (GSP g s) (int :: Int32) -> do
        let i = fromIntegral int
        boxDouble# (S.encodeDoubleInteger s (unboxInt i)) `shouldBe` boxDouble# (G.encodeDoubleInteger g (unboxInt i))
    prop "Can quotRem Integers." $ \ (GSP ga sa, GSP gb sb) -> do
        if show sb /= "0x0"
            then showUT2 (S.quotRemInteger sa sb) `shouldBe` showUT2 (G.quotRemInteger ga gb)
            else ("1", "2") `shouldBe` ("1", "2")
    prop "Can divMod Integers." $ \ (GSP ga sa, GSP gb sb) -> do
        if show sb /= "0x0"
            then showUT2 (S.divModInteger sa sb) `shouldBe` showUT2 (G.divModInteger ga gb)
            else ("1", "2") `shouldBe` ("1", "2")

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
