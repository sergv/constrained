----------------------------------------------------------------------------
-- |
-- Module      :  Data.Traversable.Constrained
-- Copyright   :  (c) Sergey Vinokurov 2019
-- License     :  BSD-2 (see LICENSE)
-- Maintainer  :  sergey@debian
----------------------------------------------------------------------------

{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE DefaultSignatures    #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Traversable.Constrained
  ( CTraversable(..)
  , cfor
  , module Data.Constrained
  ) where

import Control.Applicative (ZipList(..))
import Data.Functor.Compose (Compose(..))
import Data.Functor.Const (Const(..))
import Data.Functor.Identity (Identity(..))
import Data.Functor.Product as Product
import Data.Functor.Sum (Sum(..))
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Monoid as Monoid
import qualified Data.Semigroup as Semigroup

import Data.Constrained (Constrained(..))
import Data.Foldable.Constrained
import Data.Functor.Constrained

-- | Like 'Functor' but allows elements to have constraints on them.
-- Laws are the same:
--
-- > cmap id      == id
-- > cmap (f . g) == cmap f . cmap g
--
-- NB There's no aplicative version because Vectors from the
-- http://hackage.haskell.org/package/vector package only support
-- monadic traversals. Since they're one of the main motivation for
-- this package, 'Applicative' version of traversals will not exist.
class (CFunctor f, CFoldable f) => CTraversable f where
  ctraverse
    :: (Constraints f a, Constraints f b, Monad m)
    => (a -> m b) -> f a -> m (f b)

  {-# INLINE csequence #-}
  csequence
    :: (Constraints f a, Constraints f (m a), Monad m)
    => f (m a) -> m (f a)
  csequence = ctraverse id

  {-# INLINE ctraverse #-}
  default ctraverse
    :: (Constraints f a, Constraints f b, Monad m, Traversable f)
    => (a -> m b) -> f a -> m (f b)
  ctraverse = traverse

instance CTraversable [] where
  {-# INLINE csequence #-}
  csequence = sequenceA

instance CTraversable NonEmpty where
  {-# INLINE csequence #-}
  csequence = sequenceA

instance CTraversable Identity where
  {-# INLINE csequence #-}
  csequence = sequenceA

instance CTraversable ((,) a) where
  {-# INLINE csequence #-}
  csequence = sequenceA

instance CTraversable Maybe where
  {-# INLINE csequence #-}
  csequence = sequenceA

instance CTraversable (Either a) where
  {-# INLINE csequence #-}
  csequence = sequenceA

instance CTraversable (Const a) where
  {-# INLINE csequence #-}
  csequence = sequenceA

instance CTraversable ZipList where
  {-# INLINE csequence #-}
  csequence = sequenceA

instance CTraversable Semigroup.Min where
  {-# INLINE csequence #-}
  csequence = sequenceA

instance CTraversable Semigroup.Max where
  {-# INLINE csequence #-}
  csequence = sequenceA

instance CTraversable Semigroup.First where
  {-# INLINE csequence #-}
  csequence = sequenceA

instance CTraversable Semigroup.Last where
  {-# INLINE csequence #-}
  csequence = sequenceA

instance CTraversable Semigroup.Dual where
  {-# INLINE csequence #-}
  csequence = sequenceA

instance CTraversable Semigroup.Sum where
  {-# INLINE csequence #-}
  csequence = sequenceA

instance CTraversable Semigroup.Product where
  {-# INLINE csequence #-}
  csequence = sequenceA

instance CTraversable f => CTraversable (Monoid.Ap f) where
  {-# INLINE ctraverse #-}
  {-# INLINE csequence #-}
  ctraverse f = fmap Monoid.Ap . ctraverse f . Monoid.getAp
  csequence = fmap Monoid.Ap . csequence . Monoid.getAp

instance CTraversable f => CTraversable (Monoid.Alt f) where
  {-# INLINE ctraverse #-}
  {-# INLINE csequence #-}
  ctraverse f = fmap Monoid.Alt . ctraverse f . Monoid.getAlt
  csequence = fmap Monoid.Alt . csequence . Monoid.getAlt

instance (CTraversable f, CTraversable g) => CTraversable (Compose f g) where
  {-# INLINABLE ctraverse #-}
  ctraverse f = fmap Compose . ctraverse (ctraverse f) . getCompose

instance (CTraversable f, CTraversable g) => CTraversable (Product f g) where
  {-# INLINABLE ctraverse #-}
  ctraverse f (Pair x y) = Pair <$> ctraverse f x <*> ctraverse f y

instance (CTraversable f, CTraversable g) => CTraversable (Sum f g) where
  {-# INLINABLE ctraverse #-}
  ctraverse f (InL x) = InL <$> ctraverse f x
  ctraverse f (InR y) = InR <$> ctraverse f y

{-# INLINE cfor #-}
cfor
  :: (CTraversable f, Constraints f a, Constraints f b, Monad m)
  => f a -> (a -> m b) -> m (f b)
cfor = flip ctraverse
