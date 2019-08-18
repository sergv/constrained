----------------------------------------------------------------------------
-- |
-- Module      :  Data.Constrained.Instances
-- Copyright   :  (c) Sergey Vinokurov 2019
-- License     :  BSD-2 (see LICENSE)
-- Maintainer  :  sergey@debian
----------------------------------------------------------------------------

{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies    #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Constrained.Instances () where

import qualified Data.Array as A
import qualified Data.Array.IArray as AIA
import qualified Data.Array.Unboxed as AU
import Data.IntMap (IntMap)
import Data.Map (Map)
import Data.Set (Set)

import qualified Data.Vector as V
import qualified Data.Vector.Primitive as VP
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Unboxed as VU

import Data.Constrained

instance Constrained (A.Array i) where
  type Constraints (A.Array i) = NoConstraints

instance Constrained (AU.UArray i) where
  type Constraints (AU.UArray i) = AIA.IArray AU.UArray

instance Constrained IntMap where
  type Constraints IntMap = NoConstraints

instance Constrained (Map k) where
  type Constraints (Map k) = NoConstraints

instance Constrained Set where
  type Constraints Set = Ord

instance Constrained V.Vector where
  type Constraints V.Vector = NoConstraints

instance Constrained VP.Vector where
  type Constraints VP.Vector = VP.Prim

instance Constrained VS.Vector where
  type Constraints VS.Vector = VS.Storable

instance Constrained VU.Vector where
  type Constraints VU.Vector = VU.Unbox

