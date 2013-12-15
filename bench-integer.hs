{-# LANGUAGE CPP, MagicHash #-}

import Prelude hiding (Integer)

import GHC.Base

import qualified Criterion.Main as C
import qualified System.Random as R
import qualified Test.QuickCheck as QC
import qualified Test.QuickCheck.Gen as QC

import qualified GMP.Integer as G
import qualified Simple.Integer as S

main :: IO ()
main = do
    lhs <- makeInput
    rhs <- makeInput
    let glhs = G.mkInteger True lhs
        grhs = G.mkInteger True rhs
        slhs = S.mkInteger True lhs
        srhs = S.mkInteger True rhs
    C.defaultMain
        [ C.bgroup "Negation"
            [ C.bench "GMP"     $ C.whnf G.negateInteger grhs
            , C.bench "Simple"  $ C.whnf S.negateInteger srhs
            ]
        , C.bgroup "ToInt"
            [ C.bench "GMP"     $ C.whnf gmpToInt rhs
            , C.bench "Simple"  $ C.whnf simpleToInt rhs
            ]
        , C.bgroup "Equality"
            [ C.bench "GMP"     $ C.whnf (G.eqInteger glhs) grhs
            , C.bench "Simple"  $ C.whnf (S.eqInteger slhs) srhs
            ]
        , C.bgroup "Addition"
            [ C.bench "GMP"     $ C.whnf (G.plusInteger glhs) grhs
            , C.bench "Simple"  $ C.whnf (S.plusInteger slhs) srhs
            ]
        , C.bgroup "Subtraction"
            [ C.bench "GMP"     $ C.whnf (G.minusInteger glhs) grhs
            , C.bench "Simple"  $ C.whnf (S.minusInteger slhs) srhs
            ]
        , C.bgroup "Multiplication"
            [ C.bench "GMP"     $ C.whnf (G.timesInteger glhs) grhs
            , C.bench "Simple"  $ C.whnf (S.timesInteger slhs) srhs
            ]
        ]


gmpToInt :: [Int] -> Int
gmpToInt x = boxIntHash (G.integerToInt (G.mkInteger True x))

simpleToInt :: [Int] -> Int
simpleToInt x = boxIntHash (S.integerToInt (S.mkInteger True x))

makeInput :: IO [Int]
makeInput = do
    gen <- R.newStdGen
    let (len, gen2) = R.randomR (10, 100) gen
    return . fmap abs $ QC.unGen (QC.vector len) gen2 10000


boxIntHash :: Int# -> Int
boxIntHash i = I# i
