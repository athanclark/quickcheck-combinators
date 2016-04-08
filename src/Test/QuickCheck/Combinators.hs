{-# LANGUAGE
    DataKinds
  , ScopedTypeVariables
  , FlexibleContexts
  , KindSignatures
  , GeneralizedNewtypeDeriving
  , ConstraintKinds
  , UndecidableInstances
  #-}

module Test.QuickCheck.Combinators where

import GHC.TypeLits
import Data.Proxy

import Data.Maybe (fromMaybe)
import Data.Unfoldable.Restricted (UnfoldableR, fromList)
import Control.Monad (replicateM)

import Test.QuickCheck



-- | Generate with a minimum, inclusive size as @n :: Nat@
newtype AtLeast (n :: Nat) t a = AtLeast (t a)
  deriving (Show, Read, Eq, Ord, Enum)

instance ( UnfoldableR p t
         , Monoid (t a)
         , Arbitrary a
         , Arbitrary (t a)
         , KnownNat n
         , p a
         ) => Arbitrary (AtLeast (n :: Nat) t a) where
  arbitrary = sized $ \m' -> do
    let n' = fromIntegral $ natVal (Proxy :: Proxy n)
    k <- choose (n', m')
    (ts :: t x) <- (fromMaybe mempty . fromList) <$> replicateM k arbitrary
    return (AtLeast ts)

-- | Generate with a maximum, inclusive size as @n :: Nat@
newtype AtMost (n :: Nat) t a = AtMost (t a)
  deriving (Show, Read, Eq, Ord, Enum)

instance ( UnfoldableR p t
         , Monoid (t a)
         , Arbitrary a
         , Arbitrary (t a)
         , KnownNat m
         , p a
         ) => Arbitrary (AtMost (m :: Nat) t a) where
  arbitrary = sized $ \m'' -> do
    let m' = fromIntegral $ natVal (Proxy :: Proxy m)
    k <- choose (0, min m' m'')
    (ts :: t x) <- (fromMaybe mempty . fromList) <$> replicateM k arbitrary
    return (AtMost ts)

-- | Generate between the inclusive range of @n :: Nat@ and @m :: Nat@
newtype Between (n :: Nat) (m :: Nat) t a = Between (t a)
  deriving (Show, Read, Eq, Ord, Enum)

instance ( UnfoldableR p t
         , Monoid (t a)
         , Arbitrary a
         , Arbitrary (t a)
         , KnownNat n
         , KnownNat m
         , p a
         ) => Arbitrary (Between (n :: Nat) (m :: Nat) t a) where
  arbitrary = sized $ \m'' -> do
    let n' = fromIntegral $ natVal (Proxy :: Proxy n)
        m' = fromIntegral $ natVal (Proxy :: Proxy m)
    k <- choose (n', min m' m'')
    (ts :: t x) <- (fromMaybe mempty . fromList) <$> replicateM k arbitrary
    return (Between ts)

-- | Convenience for @AtLeast 1@
type NonMempty = AtLeast 1
