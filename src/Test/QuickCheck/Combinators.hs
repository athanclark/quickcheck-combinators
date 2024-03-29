{-# LANGUAGE
    DataKinds
  , ScopedTypeVariables
  , FlexibleContexts
  , FlexibleInstances
  , KindSignatures
  , GeneralizedNewtypeDeriving
  , ConstraintKinds
  , UndecidableInstances
  , DeriveDataTypeable
  , DeriveGeneric
  , DeriveFunctor
  , DeriveFoldable
  , DeriveTraversable
  , MonoLocalBinds
  #-}

module Test.QuickCheck.Combinators where

import GHC.TypeLits (Nat, KnownNat, natVal)
import Data.Proxy (Proxy (..))

import Data.Maybe (fromMaybe)
import Data.Unfoldable.Restricted (UnfoldableR, fromList)
import Data.Constraint.Unit (Unit)
import Control.Monad (replicateM)

import Test.QuickCheck (OrderedList (..), Arbitrary (..), choose, sized)

import qualified Data.List as L (sort)

import Data.Data (Data, Typeable)
import GHC.Generics (Generic)



-- | Generate with a minimum, inclusive size as @n :: Nat@
newtype AtLeast (n :: Nat) t a = AtLeast
  { getAtLeast :: t a
  } deriving (Show, Read, Eq, Ord, Enum, Data, Typeable, Generic, Functor
             , Applicative, Monad, Foldable, Traversable, Semigroup, Monoid)

instance
         ( UnfoldableR p t
         , Monoid (t a)
         , Arbitrary a
         , KnownNat n
         , p a
         ) => Arbitrary (AtLeast (n :: Nat) t a) where
  arbitrary = sized $ \s -> do
    let n' = fromIntegral (natVal (Proxy :: Proxy n))
    k  <- choose (n', max s n')
    ts <- fromMaybe mempty . fromList <$> replicateM k arbitrary
    return (AtLeast ts)

instance {-# OVERLAPPING #-}
         ( Arbitrary a
         , Ord a
         , UnfoldableR Unit []
         , Unit a
         , KnownNat n) => Arbitrary (AtLeast (n :: Nat) OrderedList a) where
  arbitrary = sized $ \s -> do
    let n' = fromIntegral (natVal (Proxy :: Proxy n))
        mkOrd = Ordered . L.sort . fromMaybe mempty . fromList
    k  <- choose (n', max s n')
    ts <- mkOrd <$> replicateM k arbitrary
    return (AtLeast ts)

-- | Generate with a maximum, inclusive size as @n :: Nat@
newtype AtMost (n :: Nat) t a = AtMost
  { getAtMost :: t a
  } deriving (Show, Read, Eq, Ord, Enum, Data, Typeable, Generic, Functor
             , Applicative, Monad, Foldable, Traversable, Semigroup, Monoid)

instance ( UnfoldableR p t
         , Monoid (t a)
         , Arbitrary a
         , KnownNat m
         , p a
         ) => Arbitrary (AtMost (m :: Nat) t a) where
  arbitrary = sized $ \s -> do
    let m' = fromIntegral (natVal (Proxy :: Proxy m))
    k <- choose (0, min m' s)
    ts <- fromMaybe mempty . fromList <$> replicateM k arbitrary
    return (AtMost ts)

instance {-# OVERLAPPING #-}
         ( Arbitrary a
         , Ord a
         , UnfoldableR Unit []
         , Unit a
         , KnownNat n) => Arbitrary (AtMost (n :: Nat) OrderedList a) where
  arbitrary = sized $ \s -> do
    let m' = fromIntegral $ natVal (Proxy :: Proxy n)
        mkOrd = Ordered . L.sort . fromMaybe mempty . fromList
    k <- choose (0, min m' s)
    ts <- mkOrd <$> replicateM k arbitrary
    return (AtMost ts)

-- | Generate between the inclusive range of @n :: Nat@ and @m :: Nat@
newtype Between (n :: Nat) (m :: Nat) t a = Between
  { getBetween :: t a
  } deriving (Show, Read, Eq, Ord, Enum, Data, Typeable, Generic, Functor
             , Applicative, Monad, Foldable, Traversable, Semigroup, Monoid)

instance ( UnfoldableR p t
         , Monoid (t a)
         , Arbitrary a
         , KnownNat n
         , KnownNat m
         , p a
         ) => Arbitrary (Between (n :: Nat) (m :: Nat) t a) where
  arbitrary = sized $ \s -> do
    let n' = fromIntegral (natVal (Proxy :: Proxy n))
        m' = fromIntegral (natVal (Proxy :: Proxy m))
    k <- choose (n', max n' (min m' s))
    ts <- fromMaybe mempty . fromList <$> replicateM k arbitrary
    return (Between ts)

instance {-# OVERLAPPING #-}
         ( Arbitrary a
         , Ord a
         , KnownNat n
         , UnfoldableR Unit []
         , Unit a
         , KnownNat m) => Arbitrary (Between (n :: Nat) (m :: Nat) OrderedList a) where
  arbitrary = sized $ \s -> do
    let n' = fromIntegral (natVal (Proxy :: Proxy n))
        m' = fromIntegral (natVal (Proxy :: Proxy m))
        mkOrd = Ordered . L.sort . fromMaybe mempty . fromList
    k <- choose (n', max n' (min m' s))
    ts <- mkOrd <$> replicateM k arbitrary
    return (Between ts)

-- | Convenience for @AtLeast 1@
type NonMempty = AtLeast 1
