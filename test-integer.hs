{-# LANGUAGE MagicHash #-}

import Prelude hiding (Integer)

import qualified GMP.Integer as G
import qualified Simple.Integer as S

main :: IO ()
main = do
    print $ G.mkInteger True [2, 1, 1, 1, 3, 4, 5, 6, 7, 8, 1, 2, 3, 4, 5, 6]
    print $ S.mkInteger True [2, 1, 1, 1, 3, 4, 5, 6, 7, 8, 1, 2, 3, 4, 5, 6]

