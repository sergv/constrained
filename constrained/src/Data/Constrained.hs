----------------------------------------------------------------------------
-- |
-- Module      :  Data.Constrained
-- Copyright   :  (c) Sergey Vinokurov 2019
-- License     :  BSD-2 (see LICENSE)
-- Maintainer  :  sergey@debian
----------------------------------------------------------------------------

{-# LANGUAGE CPP                     #-}
{-# LANGUAGE ConstraintKinds         #-}
{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE MultiParamTypeClasses   #-}
{-# LANGUAGE PolyKinds               #-}
{-# LANGUAGE TypeFamilies            #-}
{-# LANGUAGE UndecidableInstances    #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Data.Constrained
  ( Constrained(..)
  , NoConstraints
  ) where

import Control.Applicative (ZipList(..))
import Data.Functor.Compose (Compose(..))
import Data.Functor.Const (Const(..))
import Data.Functor.Identity (Identity(..))
import Data.Functor.Product (Product(..))
import Data.Functor.Sum (Sum(..))
import Data.Kind
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Monoid as Monoid
import qualified Data.Semigroup as Semigroup

-- | NB 'Constraints' is associated with a typeclass in order to
-- improve inference. Whenever a typeclass constraint will be present, instance
-- is guaranteed to exist and typechecker is going to take advantage of that.
class Constrained (f :: k2 -> k1) where
  type Constraints (f :: k2 -> k1) :: k2 -> Constraint

-- | Used to specify values for 'Constraints' type family to indicate
-- absence of any constraints (i.e. empty 'Constraint').
class NoConstraints (a :: k)
instance NoConstraints a

class (Constraints f a, Constraints g a) => UnionConstraints (f :: k1 -> k2) (g :: k1 -> k2) (a :: k1)
instance (Constraints f a, Constraints g a) => UnionConstraints f g a

class (Constraints f (g a), Constraints g a) => ComposeConstraints (f :: k2 -> k1) (g :: k3 -> k2) (a :: k3)
instance (Constraints f (g a), Constraints g a) => ComposeConstraints f g a

instance Constrained [] where
  type Constraints [] = NoConstraints

instance Constrained NonEmpty where
  type Constraints NonEmpty = NoConstraints

instance Constrained Identity where
  type Constraints Identity = NoConstraints

instance Constrained ((,) a) where
  type Constraints ((,) a) = NoConstraints

instance Constrained Maybe where
  type Constraints Maybe = NoConstraints

instance Constrained (Either a) where
  type Constraints (Either a) = NoConstraints

instance Constrained (Const a) where
  type Constraints (Const a) = NoConstraints

instance Constrained ZipList where
  type Constraints ZipList = NoConstraints

instance Constrained Semigroup.Min where
  type Constraints Semigroup.Min = NoConstraints

instance Constrained Semigroup.Max where
  type Constraints Semigroup.Max = NoConstraints

instance Constrained Semigroup.First where
  type Constraints Semigroup.First = NoConstraints

instance Constrained Semigroup.Last where
  type Constraints Semigroup.Last = NoConstraints

instance Constrained Semigroup.Dual where
  type Constraints Semigroup.Dual = NoConstraints

instance Constrained Semigroup.Sum where
  type Constraints Semigroup.Sum = NoConstraints

instance Constrained Semigroup.Product where
  type Constraints Semigroup.Product = NoConstraints

#if MIN_VERSION_base(4,12,0)
instance Constrained f => Constrained (Monoid.Ap f) where
  type Constraints (Monoid.Ap f) = Constraints f
#endif

instance Constrained f => Constrained (Monoid.Alt f) where
  type Constraints (Monoid.Alt f) = Constraints f

instance (Constrained f, Constrained g) => Constrained (Compose f g) where
  type Constraints (Compose f g) = ComposeConstraints f g

instance (Constrained f, Constrained g) => Constrained (Product f g) where
  type Constraints (Product f g) = UnionConstraints f g

instance (Constrained f, Constrained g) => Constrained (Sum f g) where
  type Constraints (Sum f g) = UnionConstraints f g
