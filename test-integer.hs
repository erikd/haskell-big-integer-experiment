{-# LANGUAGE MagicHash #-}

import Prelude hiding (Integer, toInteger)

import GHC.Integer

main :: IO ()
main = print $ mkInteger False [42] == mkInteger False [42]

