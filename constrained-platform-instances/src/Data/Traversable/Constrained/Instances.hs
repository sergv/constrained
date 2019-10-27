----------------------------------------------------------------------------
-- |
-- Module      :  Data.Traversable.Constrained.Instances
-- Copyright   :  (c) Sergey Vinokurov 2019
-- License     :  BSD-2 (see LICENSE)
-- Maintainer  :  sergey@debian
----------------------------------------------------------------------------

{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Traversable.Constrained.Instances () where

import qualified Data.Array as A
import Data.IntMap (IntMap)
import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Set as S

import qualified Data.Vector as V
import qualified Data.Vector.Primitive as VP
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Unboxed as VU

import Data.Constrained.Instances ()
import Data.Foldable.Constrained.Instances ()
import Data.Functor.Constrained.Instances ()
import Data.Traversable.Constrained

instance A.Ix i => CTraversable (A.Array i) where
  {-# INLINE csequence #-}
  csequence = sequenceA

instance CTraversable IntMap where
  {-# INLINE csequence #-}
  csequence = sequenceA

instance CTraversable (Map k) where
  {-# INLINE csequence #-}
  csequence = sequenceA

instance CTraversable Set where
  ctraverse f = S.foldr (\x rest -> S.insert <$> f x <*> rest) (pure S.empty)

instance CTraversable V.Vector where
  {-# INLINE csequence #-}
  csequence = sequenceA

instance CTraversable VP.Vector where
  {-# INLINE ctraverse #-}
  ctraverse = VP.mapM

instance CTraversable VS.Vector where
  {-# INLINE ctraverse #-}
  ctraverse = VS.mapM

instance CTraversable VU.Vector where
  {-# INLINE ctraverse #-}
  ctraverse = VU.mapM
