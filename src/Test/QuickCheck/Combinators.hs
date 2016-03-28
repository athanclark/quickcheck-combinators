{-# LANGUAGE
    DataKinds
  , ScopedTypeVariables
  , FlexibleContexts
  , KindSignatures
  , GeneralizedNewtypeDeriving
  #-}

module Test.QuickCheck.Combinators where

import GHC.TypeLits
import Data.Proxy

import Test.QuickCheck


newtype AtLeast (n :: Nat) f x = AtLeast (f x)
  deriving (Show, Read, Eq, Ord, Enum)

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


newtype AtMost (n :: Nat) f x = AtMost (f x)
  deriving (Show, Read, Eq, Ord, Enum)

instance ( Foldable f
         , Arbitrary x
         , Arbitrary (f x)
         , KnownNat n
         ) => Arbitrary (AtMost (n :: Nat) f x) where
  arbitrary = do
    let n' = fromIntegral $ natVal (Proxy :: Proxy n)
    (fs :: f x) <- arbitrary `suchThat`
                     (\fs' -> length fs' <= n')
    return (AtMost fs)

newtype Between (n :: Nat) (m :: Nat) f x = Between (f x)
  deriving (Show, Read, Eq, Ord, Enum)

instance ( Foldable f
         , Arbitrary x
         , Arbitrary (f x)
         , KnownNat n
         , KnownNat m
         ) => Arbitrary (Between (n :: Nat) (m :: Nat) f x) where
  arbitrary = do
    let n' = fromIntegral $ natVal (Proxy :: Proxy n)
        m' = fromIntegral $ natVal (Proxy :: Proxy m)
    (fs :: f x) <- arbitrary `suchThat`
                     (\fs' -> length fs' >= n'
                           && length fs' <= m')
    return (Between fs)


newtype NonEmpty f x = NonEmpty (AtLeast 1 f x)
  deriving (Show, Read, Eq, Ord, Enum, Arbitrary)
