{-# LANGUAGE FlexibleInstances, ScopedTypeVariables #-}

import Test.Hspec

import Check.Common
import Check.Existing

import qualified Check.New1 as New1
import qualified Check.New2 as New2
import qualified Check.New3 as New3
import qualified Check.New4 as New4


main :: IO ()
main = hspec $ do
    describe "Comparing GMP and Simple Integer operations:" testExistingInteger
    describe "Testing low level common primitives:" testCommon
    describe "Comparing GMP and New1 Integer operations:" New1.testNewInteger
    describe "Comparing GMP and New2 Integer operations:" New2.testNewInteger
    describe "Comparing GMP and New3 Integer operations:" New3.testNewInteger
    describe "Comparing GMP and New4 Integer operations:" New4.testNewInteger

