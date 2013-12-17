{-# LANGUAGE FlexibleInstances, MagicHash #-}

import Prelude hiding (Integer)

import Control.Applicative ((<$>))
import GHC.Base
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck.Arbitrary

import qualified GMP.Integer as G
import qualified Simple.Integer as S

main :: IO ()
main = hspec $
    describe "Comparing GMP and Simple Integer operations:" testInteger


testInteger :: Spec
testInteger = do
    prop "Can create Integers." $ \ (GSP g s) ->
        show g == show s
    prop "Can add Integers." $ \ (GSP ga sa, GSP gb sb) ->
        show (ga + gb) == show (sa + sb)
    prop "Can subtract Integers." $ \ (GSP ga sa, GSP gb sb) ->
        show (ga - gb) == show (sa - sb)
    prop "Can multiply Integers." $ \ (GSP ga sa, GSP gb sb) ->
        show (ga * gb) == show (sa * sb)
    prop "Can negate Integers." $ \ (GSP g s) ->
        show (G.negateInteger g) == show (S.negateInteger s)
            && show (g + G.negateInteger g) == "0"
            && show (s + S.negateInteger s) == "0"
    prop "Can convert to Int." $ \ (GSP g s) ->
        show (boxIntHash (G.integerToInt g)) == show (boxIntHash (S.integerToInt s))
    prop "Can hash an Integer." $ \ (GSP g s) ->
        show (boxIntHash (G.hashInteger g)) == show (boxIntHash (S.hashInteger s))

--------------------------------------------------------------------------------

boxIntHash :: Int# -> Int
boxIntHash i = I# i

data GmpSimplePair
    = GSP G.Integer S.Integer
    deriving Show

instance Arbitrary GmpSimplePair where
    arbitrary = do
        sign <- arbitrary
        pos <- map abs <$> arbitrary
        return $! GSP (G.mkInteger sign pos) (S.mkInteger sign pos)

