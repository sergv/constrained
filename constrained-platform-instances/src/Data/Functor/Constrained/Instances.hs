----------------------------------------------------------------------------
-- |
-- Module      :  Data.Functor.Constrained.Instances
-- Copyright   :  (c) Sergey Vinokurov 2019
-- License     :  BSD-2 (see LICENSE)
-- Maintainer  :  sergey@debian
----------------------------------------------------------------------------

{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies    #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Functor.Constrained.Instances () where

import qualified Data.Array as A
import qualified Data.Array.IArray as AIA
import qualified Data.Array.Unboxed as AU
import Data.IntMap (IntMap)
import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Set as S

import qualified Data.Vector as V
import qualified Data.Vector.Primitive as VP
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Unboxed as VU

import Data.Functor.Constrained
import Data.Constrained.Instances ()

instance CFunctor (A.Array i)

instance AU.Ix i => CFunctor (AU.UArray i) where
  {-# INLINE cmap  #-}
  cmap = AIA.amap

instance CFunctor IntMap where
  {-# INLINE cmap_ #-}
  cmap_ = (<$)

instance CFunctor (Map k) where
  {-# INLINE cmap_ #-}
  cmap_ = (<$)

instance CFunctor Set where
  {-# INLINE cmap  #-}
  {-# INLINE cmap_ #-}
  cmap = S.map
  cmap_ = const . S.singleton

instance CFunctor V.Vector where
  {-# INLINE cmap_ #-}
  cmap_ = (<$)

instance CFunctor VP.Vector where
  {-# INLINE cmap  #-}
  {-# INLINE cmap_ #-}
  cmap = VP.map
  cmap_ x v = VP.replicate (VP.length v) x

instance CFunctor VS.Vector where
  {-# INLINE cmap  #-}
  {-# INLINE cmap_ #-}
  cmap = VS.map
  cmap_ x v = VS.replicate (VS.length v) x

instance CFunctor VU.Vector where
  {-# INLINE cmap  #-}
  {-# INLINE cmap_ #-}
  cmap = VU.map
  cmap_ x v = VU.replicate (VU.length v) x
