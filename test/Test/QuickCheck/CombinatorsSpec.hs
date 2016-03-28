module Test.QuickCheck.CombinatorsSpec (spec) where

import Test.QuickCheck.Combinators

import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.QuickCheck
import Test.QuickCheck.Instances


spec :: TestTree
spec = testGroup "Test.QuickCheck.Combinators"
  [ QC.testProperty "`someFunction` should pass"
      someFunction
  ]

someFunction :: Bool -> Property
someFunction x = not (not $ x) === x
