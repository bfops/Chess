#!/usr/bin/env runhaskell
module Main where 

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Test.QuickCheck
import Test.HUnit

main = defaultMain tests

tests = [
        testGroup "Mock tests"  [
                       testProperty "addition is commutative" prop_addition_is_commutative
            ]
        ]

-- Just a dummy test to check the framework.
prop_addition_is_commutative :: Int -> Int -> Int -> Bool
prop_addition_is_commutative a b c = (a + (b + c)) == ((a + b) + c)
