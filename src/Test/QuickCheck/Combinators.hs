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

import Data.Maybe (fromMaybe)
import Data.Unfoldable (Unfoldable, fromList)
import Control.Monad (replicateM)

import Test.QuickCheck



-- | Generate with a minimum, inclusive size as @n :: Nat@
newtype AtLeast (n :: Nat) t x = AtLeast (t x)
  deriving (Show, Read, Eq, Ord, Enum)

instance ( Unfoldable t
         , Monoid (t x)
         , Arbitrary x
         , Arbitrary (t x)
         , KnownNat n
         ) => Arbitrary (AtLeast (n :: Nat) t x) where
  arbitrary = sized $ \m' -> do
    let n' = fromIntegral $ natVal (Proxy :: Proxy n)
    k <- choose (n', m')
    (ts :: t x) <- (fromMaybe mempty . fromList) <$> replicateM k arbitrary
    return (AtLeast ts)

-- | Generate with a maximum, inclusive size as @n :: Nat@
newtype AtMost (n :: Nat) t x = AtMost (t x)
  deriving (Show, Read, Eq, Ord, Enum)

instance ( Unfoldable t
         , Monoid (t x)
         , Arbitrary x
         , Arbitrary (t x)
         , KnownNat m
         ) => Arbitrary (AtMost (m :: Nat) t x) where
  arbitrary = sized $ \m'' -> do
    let m' = fromIntegral $ natVal (Proxy :: Proxy m)
    k <- choose (0, min m' m'')
    (ts :: t x) <- (fromMaybe mempty . fromList) <$> replicateM k arbitrary
    return (AtMost ts)

-- | Generate between the inclusive range of @n :: Nat@ and @m :: Nat@
newtype Between (n :: Nat) (m :: Nat) t x = Between (t x)
  deriving (Show, Read, Eq, Ord, Enum)

instance ( Unfoldable t
         , Monoid (t x)
         , Arbitrary x
         , Arbitrary (t x)
         , KnownNat n
         , KnownNat m
         ) => Arbitrary (Between (n :: Nat) (m :: Nat) t x) where
  arbitrary = sized $ \m'' -> do
    let n' = fromIntegral $ natVal (Proxy :: Proxy n)
        m' = fromIntegral $ natVal (Proxy :: Proxy m)
    k <- choose (n', min m' m'')
    (ts :: t x) <- (fromMaybe mempty . fromList) <$> replicateM k arbitrary
    return (Between ts)

-- | Convenience for @AtLeast 1@
type NonMempty = AtLeast 1
