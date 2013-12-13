{-# LANGUAGE MagicHash #-}

import Prelude hiding (Integer)

import Test.Hspec
import Test.Hspec.QuickCheck

import qualified GMP.Integer as G
import qualified Simple.Integer as S

main :: IO ()
main = hspec $
    describe "Comparing GMP and Simple Integer operations:" testInteger


testInteger :: Spec
testInteger = do
    prop "Can create Integers." $ \ lst ->
        let pos = concat . replicate 10 $ map abs lst
        in show (G.mkInteger True pos) == show (S.mkInteger True pos)
    prop "Can add Integers." $ \ a b ->
        let ap = concat . replicate 7 $ map abs a
            bp = concat . replicate 7 $ map abs b
            ga = G.mkInteger True ap
            gb = G.mkInteger True bp
            sa = S.mkInteger True ap
            sb = S.mkInteger True bp
        in show (ga + gb) == show (sa + sb)
