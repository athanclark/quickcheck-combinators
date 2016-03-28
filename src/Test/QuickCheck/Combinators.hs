{-# LANGUAGE
    DataKinds
  , ScopedTypeVariables
  , FlexibleContexts
  , KindSignatures
  #-}

module Test.QuickCheck.Combinators where

import GHC.TypeLits
import Data.Proxy

import Test.QuickCheck


newtype AtLeast (n :: Nat) f x = AtLeast (f x)

instance ( Foldable f
         , Arbitrary x
         , Arbitrary (f x)
         , KnownNat n
         ) => Arbitrary (AtLeast (n :: Nat) f x) where
  arbitrary = do
    let n' = fromIntegral $ natVal (Proxy :: Proxy n)
    (fs :: f x) <- arbitrary `suchThat`
                     (\fs' -> length fs' >= n')
    return (AtLeast fs)
