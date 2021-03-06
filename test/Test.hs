module Main (main) where 

import Prelewd

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Test.QuickCheck
import Test.HUnit

import Storage.List

import qualified Game.Gameplay

{-# ANN module "HLint: ignore" #-}

main = defaultMain tests

tests = [
        testGroup "Sorting Group 1" [
                testProperty "sort1" prop_sort1,
                testProperty "sort2" prop_sort2,
                testProperty "sort3" prop_sort3
            ],
        testGroup "Sorting Group 2" [
                testGroup "Nested Group 1" [
                       testProperty "sort4" prop_sort4,
                       testProperty "sort5" prop_sort5,
                       testProperty "sort6" prop_sort6
                     ],
                testCase "sort7" test_sort7
            ],
        testGroup "Game logic" Game.Gameplay.tests
    ]


prop_sort1 xs = sort xs == sortBy compare xs
  where types = (xs :: [Int])

prop_sort2 xs =
        (not (null xs)) ==>
        (head (sort xs) == Just (minimum xs))
  where types = (xs :: [Int])

prop_sort3 xs = (not (null xs)) ==>
        last (sort xs) == Just (maximum xs)
  where types = (xs :: [Int])

prop_sort4 xs ys =
        (not (null xs)) ==>
        (not (null ys)) ==>
        (head (sort (xs <> ys)) == min (Just $ minimum xs) (Just $ minimum ys))
  where types = (xs :: [Int], ys :: [Int])

prop_sort5 xs ys =
        (not (null xs)) ==>
        (not (null ys)) ==>
        ((head . reverse . sort $ xs <> ys) == max (Just $ maximum xs) (Just $ maximum ys))
  where types = (xs :: [Int], ys :: [Int])

prop_sort6 xs ys =
        (not (null xs)) ==>
        (not (null ys)) ==>
        (last (sort (xs <> ys)) == max (Just $ maximum xs) (Just $ maximum ys))
  where types = (xs :: [Int], ys :: [Int])

test_sort7 = sort [8, 7, 2, 5, 4, 9, 6, 1, 0, 3] @?= [0..9]
