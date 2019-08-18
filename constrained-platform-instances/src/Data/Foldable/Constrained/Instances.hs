----------------------------------------------------------------------------
-- |
-- Module      :  Data.Foldable.Constrained.Instances
-- Copyright   :  (c) Sergey Vinokurov 2019
-- License     :  BSD-2 (see LICENSE)
-- Maintainer  :  sergey@debian
----------------------------------------------------------------------------

{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies    #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Foldable.Constrained.Instances () where

import qualified Data.Array as A
import qualified Data.Array.Unboxed as AU
import qualified Data.Foldable as F
import Data.IntMap (IntMap)
import Data.Map (Map)
import Data.Set (Set)

import qualified Data.Vector as V
import qualified Data.Vector.Primitive as VP
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Unboxed as VU

import Data.Foldable.Constrained
import Data.Constrained.Instances ()

instance CFoldable (A.Array i) where
  {-# INLINE cfold    #-}
  {-# INLINE cfoldMap #-}
  {-# INLINE cfoldr   #-}
  {-# INLINE cfoldr'  #-}
  {-# INLINE cfoldl   #-}
  {-# INLINE cfoldl'  #-}
  {-# INLINE cfoldr1  #-}
  {-# INLINE cfoldl1  #-}
  {-# INLINE ctoList  #-}
  {-# INLINE cnull    #-}
  {-# INLINE clength  #-}
  {-# INLINE celem    #-}
  {-# INLINE cmaximum #-}
  {-# INLINE cminimum #-}
  {-# INLINE csum     #-}
  {-# INLINE cproduct #-}
  cfold    = F.fold
  cfoldMap = F.foldMap
  cfoldr   = F.foldr
  cfoldr'  = F.foldr'
  cfoldl   = F.foldl
  cfoldl'  = F.foldl'
  cfoldr1  = F.foldr1
  cfoldl1  = F.foldl1
  ctoList  = F.toList
  cnull    = F.null
  clength  = F.length
  celem    = F.elem
  cmaximum = F.maximum
  cminimum = F.minimum
  csum     = F.sum
  cproduct = F.product

instance AU.Ix i => CFoldable (AU.UArray i) where
  {-# INLINE cfoldMap #-}
  {-# INLINE ctoList  #-}
  cfoldMap f = F.foldMap f . AU.elems
  ctoList    = AU.elems

instance CFoldable IntMap where
  {-# INLINE cfold    #-}
  {-# INLINE cfoldMap #-}
  {-# INLINE cfoldr   #-}
  {-# INLINE cfoldr'  #-}
  {-# INLINE cfoldl   #-}
  {-# INLINE cfoldl'  #-}
  {-# INLINE cfoldr1  #-}
  {-# INLINE cfoldl1  #-}
  {-# INLINE ctoList  #-}
  {-# INLINE cnull    #-}
  {-# INLINE clength  #-}
  {-# INLINE celem    #-}
  {-# INLINE cmaximum #-}
  {-# INLINE cminimum #-}
  {-# INLINE csum     #-}
  {-# INLINE cproduct #-}
  cfold    = F.fold
  cfoldMap = F.foldMap
  cfoldr   = F.foldr
  cfoldr'  = F.foldr'
  cfoldl   = F.foldl
  cfoldl'  = F.foldl'
  cfoldr1  = F.foldr1
  cfoldl1  = F.foldl1
  ctoList  = F.toList
  cnull    = F.null
  clength  = F.length
  celem    = F.elem
  cmaximum = F.maximum
  cminimum = F.minimum
  csum     = F.sum
  cproduct = F.product

instance CFoldable (Map k) where
  {-# INLINE cfold    #-}
  {-# INLINE cfoldMap #-}
  {-# INLINE cfoldr   #-}
  {-# INLINE cfoldr'  #-}
  {-# INLINE cfoldl   #-}
  {-# INLINE cfoldl'  #-}
  {-# INLINE cfoldr1  #-}
  {-# INLINE cfoldl1  #-}
  {-# INLINE ctoList  #-}
  {-# INLINE cnull    #-}
  {-# INLINE clength  #-}
  {-# INLINE celem    #-}
  {-# INLINE cmaximum #-}
  {-# INLINE cminimum #-}
  {-# INLINE csum     #-}
  {-# INLINE cproduct #-}
  cfold    = F.fold
  cfoldMap = F.foldMap
  cfoldr   = F.foldr
  cfoldr'  = F.foldr'
  cfoldl   = F.foldl
  cfoldl'  = F.foldl'
  cfoldr1  = F.foldr1
  cfoldl1  = F.foldl1
  ctoList  = F.toList
  cnull    = F.null
  clength  = F.length
  celem    = F.elem
  cmaximum = F.maximum
  cminimum = F.minimum
  csum     = F.sum
  cproduct = F.product

instance CFoldable Set where
  {-# INLINE cfold    #-}
  {-# INLINE cfoldMap #-}
  {-# INLINE cfoldr   #-}
  {-# INLINE cfoldr'  #-}
  {-# INLINE cfoldl   #-}
  {-# INLINE cfoldl'  #-}
  {-# INLINE cfoldr1  #-}
  {-# INLINE cfoldl1  #-}
  {-# INLINE ctoList  #-}
  {-# INLINE cnull    #-}
  {-# INLINE clength  #-}
  {-# INLINE celem    #-}
  {-# INLINE cmaximum #-}
  {-# INLINE cminimum #-}
  {-# INLINE csum     #-}
  {-# INLINE cproduct #-}
  cfold    = F.fold
  cfoldMap = F.foldMap
  cfoldr   = F.foldr
  cfoldr'  = F.foldr'
  cfoldl   = F.foldl
  cfoldl'  = F.foldl'
  cfoldr1  = F.foldr1
  cfoldl1  = F.foldl1
  ctoList  = F.toList
  cnull    = F.null
  clength  = F.length
  celem    = F.elem
  cmaximum = F.maximum
  cminimum = F.minimum
  csum     = F.sum
  cproduct = F.product

instance CFoldable V.Vector where
  {-# INLINE cfold    #-}
  {-# INLINE cfoldMap #-}
  {-# INLINE cfoldr   #-}
  {-# INLINE cfoldr'  #-}
  {-# INLINE cfoldl   #-}
  {-# INLINE cfoldl'  #-}
  {-# INLINE cfoldr1  #-}
  {-# INLINE cfoldl1  #-}
  {-# INLINE ctoList  #-}
  {-# INLINE cnull    #-}
  {-# INLINE clength  #-}
  {-# INLINE celem    #-}
  {-# INLINE cmaximum #-}
  {-# INLINE cminimum #-}
  {-# INLINE csum     #-}
  {-# INLINE cproduct #-}
  cfold    = F.fold
  cfoldMap = F.foldMap
  cfoldr   = F.foldr
  cfoldr'  = F.foldr'
  cfoldl   = F.foldl
  cfoldl'  = F.foldl'
  cfoldr1  = F.foldr1
  cfoldl1  = F.foldl1
  ctoList  = F.toList
  cnull    = F.null
  clength  = F.length
  celem    = F.elem
  cmaximum = F.maximum
  cminimum = F.minimum
  csum     = F.sum
  cproduct = F.product

instance CFoldable VP.Vector where
  {-# INLINE cfoldr   #-}
  {-# INLINE cfoldr'  #-}
  {-# INLINE cfoldl   #-}
  {-# INLINE cfoldl'  #-}
  {-# INLINE cfoldr1  #-}
  {-# INLINE cfoldl1  #-}
  {-# INLINE ctoList  #-}
  {-# INLINE cnull    #-}
  {-# INLINE clength  #-}
  {-# INLINE celem    #-}
  {-# INLINE cmaximum #-}
  {-# INLINE cminimum #-}
  {-# INLINE csum     #-}
  {-# INLINE cproduct #-}
  cfoldr   = VP.foldr
  cfoldr'  = VP.foldr'
  cfoldl   = VP.foldl
  cfoldl'  = VP.foldl'
  cfoldr1  = VP.foldr1
  cfoldl1  = VP.foldl1
  ctoList  = VP.toList
  cnull    = VP.null
  clength  = VP.length
  celem    = VP.elem
  cmaximum = VP.maximum
  cminimum = VP.minimum
  csum     = VP.sum
  cproduct = VP.product

instance CFoldable VS.Vector where
  {-# INLINE cfoldr   #-}
  {-# INLINE cfoldr'  #-}
  {-# INLINE cfoldl   #-}
  {-# INLINE cfoldl'  #-}
  {-# INLINE cfoldr1  #-}
  {-# INLINE cfoldl1  #-}
  {-# INLINE ctoList  #-}
  {-# INLINE cnull    #-}
  {-# INLINE clength  #-}
  {-# INLINE celem    #-}
  {-# INLINE cmaximum #-}
  {-# INLINE cminimum #-}
  {-# INLINE csum     #-}
  {-# INLINE cproduct #-}
  cfoldr   = VS.foldr
  cfoldr'  = VS.foldr'
  cfoldl   = VS.foldl
  cfoldl'  = VS.foldl'
  cfoldr1  = VS.foldr1
  cfoldl1  = VS.foldl1
  ctoList  = VS.toList
  cnull    = VS.null
  clength  = VS.length
  celem    = VS.elem
  cmaximum = VS.maximum
  cminimum = VS.minimum
  csum     = VS.sum
  cproduct = VS.product

instance CFoldable VU.Vector where
  {-# INLINE cfoldr   #-}
  {-# INLINE cfoldr'  #-}
  {-# INLINE cfoldl   #-}
  {-# INLINE cfoldl'  #-}
  {-# INLINE cfoldr1  #-}
  {-# INLINE cfoldl1  #-}
  {-# INLINE ctoList  #-}
  {-# INLINE cnull    #-}
  {-# INLINE clength  #-}
  {-# INLINE celem    #-}
  {-# INLINE cmaximum #-}
  {-# INLINE cminimum #-}
  {-# INLINE csum     #-}
  {-# INLINE cproduct #-}
  cfoldr   = VU.foldr
  cfoldr'  = VU.foldr'
  cfoldl   = VU.foldl
  cfoldl'  = VU.foldl'
  cfoldr1  = VU.foldr1
  cfoldl1  = VU.foldl1
  ctoList  = VU.toList
  cnull    = VU.null
  clength  = VU.length
  celem    = VU.elem
  cmaximum = VU.maximum
  cminimum = VU.minimum
  csum     = VU.sum
  cproduct = VU.product
