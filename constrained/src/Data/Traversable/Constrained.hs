----------------------------------------------------------------------------
-- |
-- Module      :  Data.Traversable.Constrained
-- Copyright   :  (c) Sergey Vinokurov 2019
-- License     :  BSD-2 (see LICENSE)
-- Maintainer  :  sergey@debian
----------------------------------------------------------------------------

{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DefaultSignatures      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeFamilies           #-}

module Data.Traversable.Constrained
  ( CTraversable(..)
  , module Data.Constrained
  ) where

import Data.Functor.Const (Const(..))
import Data.Functor.Identity (Identity(..))
import Data.Kind
import Data.List.NonEmpty (NonEmpty(..))

import Data.Constrained (Constrained(..), NoConstraints)
import Data.Foldable.Constrained
import Data.Functor.Constrained

-- | Like 'Functor' but allows elements to have constraints on them.
-- Laws are the same:
--
-- > cmap id      == id
-- > cmap (f . g) == cmap f . cmap g
--
-- Parameter 'c' provides constraints on the \'applicative function\'
-- that can be used to perform the traversal. Typical values for it are
-- 'Applicative' and 'Monad'.
--
-- E.g. standard 'Traversable' is recovered when 'c' is 'Applicative'.
-- Vectors from the http://hackage.haskell.org/package/vector package
-- only support monadic traversals, so for them it's 'Monad'. Other
-- values for 'c' are definitely possible too.
class (CFunctor f, CFoldable f) => CTraversable (c :: (Type -> Type) -> Constraint) (f :: Type -> Type) | f -> c where
  ctraverse
    :: (Constraints f a, Constraints f b, c m)
    => (a -> m b) -> f a -> m (f b)

  {-# INLINE csequence #-}
  csequence
    :: (Constraints f a, Constraints f (m a), c m)
    => f (m a) -> m (f a)
  csequence = ctraverse id

  {-# INLINE ctraverse #-}
  default ctraverse
    :: (Constraints f a, Constraints f b, c ~ Applicative, c m, Traversable f)
    => (a -> m b) -> f a -> m (f b)
  ctraverse = traverse

instance CTraversable Applicative [] where
  {-# INLINE csequence #-}
  csequence = sequenceA

instance CTraversable Applicative NonEmpty where
  {-# INLINE csequence #-}
  csequence = sequenceA

instance CTraversable Applicative Identity where
  {-# INLINE csequence #-}
  csequence = sequenceA

instance CTraversable Applicative ((,) a) where
  {-# INLINE csequence #-}
  csequence = sequenceA

instance CTraversable Applicative Maybe where
  {-# INLINE csequence #-}
  csequence = sequenceA

instance CTraversable Applicative (Either a) where
  {-# INLINE csequence #-}
  csequence = sequenceA

instance CTraversable Applicative (Const a) where
  {-# INLINE csequence #-}
  csequence = sequenceA
