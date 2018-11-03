# quickcheck-combinators

Simply wrap the type you want to generate (assuming it satisfies all the necessary constraints) to refine the terms generated:


```haskell
{-# LANGUAGE DataKinds #-}

import Data.Set (Set)
import Test.QuickCheck
import Test.QuickCheck.Instances
import GHC.TypeLits

instance Arbitrary LinearEquation where
  arbitrary = do
    vars <- arbitrary :: Gen (AtLeast 3 Set String)
    -- ...
```
