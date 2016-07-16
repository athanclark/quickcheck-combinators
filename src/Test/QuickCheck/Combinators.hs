{-# LANGUAGE
    DataKinds
  , ScopedTypeVariables
  , FlexibleContexts
  , FlexibleInstances
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

import qualified Data.List as L (sort)



-- | Generate with a minimum, inclusive size as @n :: Nat@
newtype AtLeast (n :: Nat) t a = AtLeast (t a)
  deriving (Show, Read, Eq, Ord, Enum)

instance ( UnfoldableR p t
         , Monoid (t a)
         , Arbitrary a
         , KnownNat n
         , p a
         ) => Arbitrary (AtLeast (n :: Nat) t a) where
  arbitrary = sized $ \m' -> do
    let n' = fromIntegral $ natVal (Proxy :: Proxy n)
    k  <- choose (min n' m', max n' m')
    ts <- fromMaybe mempty . fromList <$> replicateM k arbitrary
    return . AtLeast $ ts

instance ( Arbitrary a
         , Ord a
         , UnfoldableR p []
         , p a
         , KnownNat n) => Arbitrary (AtLeast (n :: Nat) OrderedList a) where
  arbitrary = sized $ \m -> do
    let n' = fromIntegral $ natVal (Proxy :: Proxy n)
        mkOrd = Ordered . L.sort . fromMaybe mempty . fromList
    k  <- choose (min n' m, max n' m)
    ts <- mkOrd <$> replicateM k arbitrary
    return . AtLeast $ ts

-- | Generate with a maximum, inclusive size as @n :: Nat@
newtype AtMost (n :: Nat) t a = AtMost (t a)
  deriving (Show, Read, Eq, Ord, Enum)

instance ( UnfoldableR p t
         , Monoid (t a)
         , Arbitrary a
         , KnownNat m
         , p a
         ) => Arbitrary (AtMost (m :: Nat) t a) where
  arbitrary = sized $ \m'' -> do
    let m' = fromIntegral $ natVal (Proxy :: Proxy m)
    k <- choose (0, min m' m'')
    ts <- fromMaybe mempty . fromList <$> replicateM k arbitrary
    return . AtMost $ ts

instance ( Arbitrary a
         , Ord a
         , UnfoldableR p []
         , p a
         , KnownNat n) => Arbitrary (AtMost (n :: Nat) OrderedList a) where
  arbitrary = sized $ \m -> do
    let n' = fromIntegral $ natVal (Proxy :: Proxy n)
        mkOrd = Ordered . L.sort . fromMaybe mempty . fromList
    k <- choose (0, min n' m)
    ts <- mkOrd <$> replicateM k arbitrary
    return . AtMost $ ts

-- | Generate between the inclusive range of @n :: Nat@ and @m :: Nat@
newtype Between (n :: Nat) (m :: Nat) t a = Between (t a)
  deriving (Show, Read, Eq, Ord, Enum)

instance ( UnfoldableR p t
         , Monoid (t a)
         , Arbitrary a
         , KnownNat n
         , KnownNat m
         , p a
         ) => Arbitrary (Between (n :: Nat) (m :: Nat) t a) where
  arbitrary = sized $ \m'' -> do
    let n' = fromIntegral $ natVal (Proxy :: Proxy n)
        m' = fromIntegral $ natVal (Proxy :: Proxy m)
    k <- choose (n', min m' m'')
    ts <- fromMaybe mempty . fromList <$> replicateM k arbitrary
    return . Between $ ts

instance ( Arbitrary a
         , Ord a
         , KnownNat n
         , UnfoldableR p []
         , p a
         , KnownNat m) => Arbitrary (Between (n :: Nat) (m :: Nat) OrderedList a) where
  arbitrary = sized $ \s -> do
    let n' = fromIntegral $ natVal (Proxy :: Proxy n)
        m' = fromIntegral $ natVal (Proxy :: Proxy m)
        mkOrd = Ordered . L.sort . fromMaybe mempty . fromList
    k <- choose (n', min m' s)
    ts <- mkOrd <$> replicateM k arbitrary
    return . Between $ ts

-- | Convenience for @AtLeast 1@
type NonMempty = AtLeast 1
