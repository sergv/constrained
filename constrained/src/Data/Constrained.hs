----------------------------------------------------------------------------
-- |
-- Module      :  Data.Constrained
-- Copyright   :  (c) Sergey Vinokurov 2019
-- License     :  BSD-2 (see LICENSE)
-- Maintainer  :  sergey@debian
----------------------------------------------------------------------------

{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds         #-}
{-# LANGUAGE TypeFamilies      #-}

module Data.Constrained
  ( Constrained(..)
  , NoConstraints
  ) where

import Data.Functor.Const (Const(..))
import Data.Functor.Identity (Identity(..))
import Data.Kind
import Data.List.NonEmpty (NonEmpty(..))

-- | NB 'Constraints' is associated with a typeclass in order to
-- improve inference. Whenever a typeclass constraint will be present, instance
-- is guaranteed to exist and typechecker is going to take advantage of that.
class Constrained (f :: k1 -> k2) where
  type Constraints (f :: k1 -> k2) :: k1 -> Constraint

class NoConstraints (a :: k)
instance NoConstraints a


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
