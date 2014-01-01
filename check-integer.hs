{-# LANGUAGE FlexibleInstances, ScopedTypeVariables #-}

import Test.Hspec

import Check.Existing
import Check.New1
import Check.New2


main :: IO ()
main = hspec $ do
    describe "Comparing GMP and Simple Integer operations:" testExistingInteger
    describe "Testing low level primitives for New1 Integer library:" testNew1Internal
    describe "Testing low level primitives for New2 Integer library:" testNew2Internal
    describe "Comparing GMP and New1 Integer operations:" testNew1Integer
    describe "Comparing GMP and New2 Integer operations:" testNew2Integer

