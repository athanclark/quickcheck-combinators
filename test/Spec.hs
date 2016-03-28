module Spec where

import Test.QuickCheck.CombinatorsSpec

import Test.Tasty


main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Testing..."
  [spec]
