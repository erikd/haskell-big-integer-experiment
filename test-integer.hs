{-# LANGUAGE MagicHash #-}

import Prelude hiding (Integer)

import GHC.Integer

import Support ()

main :: IO ()
main = print $ mkInteger True [2, 1, 1, 1, 3, 4, 5, 6, 7, 8, 1, 2, 3, 4, 5, 6]

