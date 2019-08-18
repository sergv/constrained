----------------------------------------------------------------------------
-- |
-- Module      :  Data.Functor.Constrained
-- Copyright   :  (c) Sergey Vinokurov 2019
-- License     :  BSD-2 (see LICENSE)
-- Maintainer  :  sergey@debian
----------------------------------------------------------------------------

{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeFamilies      #-}

module Data.Functor.Constrained
  ( CFunctor(..)
  , module Data.Constrained
  ) where

import Data.Functor.Const (Const(..))
import Data.Functor.Identity (Identity(..))
import Data.List.NonEmpty (NonEmpty(..))

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
