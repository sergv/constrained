----------------------------------------------------------------------------
-- |
-- Module      :  Data.Functor.Constrained
-- Copyright   :  (c) Sergey Vinokurov 2019
-- License     :  BSD-2 (see LICENSE)
-- Maintainer  :  sergey@debian
----------------------------------------------------------------------------

{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DefaultSignatures   #-}
{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module Data.Functor.Constrained
  ( CFunctor(..)
  , module Data.Constrained
  ) where

import Control.Applicative (ZipList(..))
import Data.Coerce
import Data.Functor.Compose (Compose(..))
import Data.Functor.Const (Const(..))
import Data.Functor.Identity (Identity(..))
import Data.Functor.Product (Product(..))
import Data.Functor.Sum (Sum(..))
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Monoid as Monoid
import qualified Data.Semigroup as Semigroup

import Data.Constrained (Constrained(..), NoConstraints)

-- | Like 'Functor' but allows elements to have constraints on them.
-- Laws are the same:
--
-- > cmap id      == id
-- > cmap (f . g) == cmap f . cmap g
class Constrained f => CFunctor f where
  cmap :: (Constraints f a, Constraints f b) => (a -> b) -> f a -> f b

  {-# INLINE cmap_ #-}
  cmap_ :: (Constraints f a, Constraints f b) => a -> f b -> f a
  cmap_ = cmap . const

  {-# INLINE cmap #-}
  default cmap
    :: (Functor f, Constraints f a, Constraints f b)
    => (a -> b)
    -> f a
    -> f b
  cmap = fmap

instance CFunctor [] where
  {-# INLINE cmap_ #-}
  cmap_ = (<$)

instance CFunctor NonEmpty where
  {-# INLINE cmap_ #-}
  cmap_ = (<$)

instance CFunctor Identity where
  {-# INLINE cmap_ #-}
  cmap_ = (<$)

instance CFunctor ((,) a) where
  {-# INLINE cmap_ #-}
  cmap_ = (<$)

instance CFunctor Maybe where
  {-# INLINE cmap_ #-}
  cmap_ = (<$)

instance CFunctor (Either a) where
  {-# INLINE cmap_ #-}
  cmap_ = (<$)

instance CFunctor (Const a) where
  {-# INLINE cmap_ #-}
  cmap_ = (<$)

instance CFunctor ZipList where
  {-# INLINE cmap_ #-}
  cmap_ = (<$)

instance CFunctor Semigroup.Min where
  {-# INLINE cmap_ #-}
  cmap_ = (<$)

instance CFunctor Semigroup.Max where
  {-# INLINE cmap_ #-}
  cmap_ = (<$)

instance CFunctor Semigroup.First where
  {-# INLINE cmap_ #-}
  cmap_ = (<$)

instance CFunctor Semigroup.Last where
  {-# INLINE cmap_ #-}
  cmap_ = (<$)

instance CFunctor Semigroup.Dual where
  {-# INLINE cmap_ #-}
  cmap_ = (<$)

instance CFunctor Semigroup.Sum where
  {-# INLINE cmap_ #-}
  cmap_ = (<$)

instance CFunctor Semigroup.Product where
  {-# INLINE cmap_ #-}
  cmap_ = (<$)

#if MIN_VERSION_base(4,12,0)
instance CFunctor f => CFunctor (Monoid.Ap f) where
  {-# INLINE cmap #-}
  {-# INLINE cmap_ #-}
  cmap
    :: forall a b. (Constraints (Monoid.Ap f) a, Constraints (Monoid.Ap f) b)
    => (a -> b) -> Monoid.Ap f a -> Monoid.Ap f b
  cmap = coerce (cmap :: (a -> b) -> f a -> f b)

  cmap_
    :: forall a b. (Constraints (Monoid.Ap f) a, Constraints (Monoid.Ap f) b)
    => a -> Monoid.Ap f b -> Monoid.Ap f a
  cmap_ = coerce (cmap_ :: a -> f b -> f a)
#endif

instance CFunctor f => CFunctor (Monoid.Alt f) where
  {-# INLINE cmap #-}
  {-# INLINE cmap_ #-}
  cmap
    :: forall a b. (Constraints (Monoid.Alt f) a, Constraints (Monoid.Alt f) b)
    => (a -> b) -> Monoid.Alt f a -> Monoid.Alt f b
  cmap = coerce (cmap :: (a -> b) -> f a -> f b)

  cmap_
    :: forall a b. (Constraints (Monoid.Alt f) a, Constraints (Monoid.Alt f) b)
    => a -> Monoid.Alt f b -> Monoid.Alt f a
  cmap_ = coerce (cmap_ :: a -> f b -> f
   a)


instance (CFunctor f, CFunctor g) => CFunctor (Compose f g) where
  {-# INLINE cmap #-}
  cmap f (Compose x) = Compose (cmap (cmap f) x)

instance (CFunctor f, CFunctor g) => CFunctor (Product f g) where
  {-# INLINE cmap #-}
  cmap f (Pair x y) = Pair (cmap f x) (cmap f y)

instance (CFunctor f, CFunctor g) => CFunctor (Sum f g) where
  {-# INLINE cmap #-}
  cmap f (InL x) = InL (cmap f x)
  cmap f (InR y) = InR (cmap f y)
