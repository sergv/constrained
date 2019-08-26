----------------------------------------------------------------------------
-- |
-- Module      :  Data.Foldable.Constrained
-- Copyright   :  (c) Sergey Vinokurov 2019
-- License     :  BSD-2 (see LICENSE)
-- Maintainer  :  sergey@debian
----------------------------------------------------------------------------

{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module Data.Foldable.Constrained
  ( CFoldable(..)
  , cfoldrM
  , cfoldlM
  , ctraverse_
  , cfor_
  , cmapM_
  , cforM_
  , csequenceA_
  , csequence_
  , casum
  , cmsum
  , cconcat
  , cconcatMap
  , cand
  , cor
  , cany
  , call
  , cmaximumBy
  , cminimumBy
  , cnotElem
  , cfind
  , module Data.Constrained
  ) where

import Prelude
  (Bool(..), id, (.), Int, Eq(..), Ord(..), Num(..), ($), ($!), flip, errorWithoutStackTrace, not)

import Control.Applicative
import Control.Monad hiding (mapM_)
import Data.Either
import qualified Data.Foldable as F
import Data.Functor.Compose (Compose(..))
import Data.Functor.Const (Const(..))
import Data.Functor.Identity (Identity(..))
import Data.Functor.Product as Product
import Data.Functor.Sum as Sum
import Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe
import Data.Monoid
import Data.Ord
import Data.Semigroup (Max(..), Min(..), Option(..), (<>))
import GHC.Base (build)

import Data.Constrained (Constrained(..))

-- | Like 'Functor' but allows elements to have constraints on them.
-- Laws are the same:
--
-- > cmap id      == id
-- > cmap (f . g) == cmap f . cmap g
class Constrained f => CFoldable f where
  {-# MINIMAL cfoldMap | cfoldr #-}

  -- | Combine the elements of a structure using a monoid.
  cfold :: (Monoid m, Constraints f m) => f m -> m
  cfold = cfoldMap id
  {-# INLINABLE cfold #-}

  -- | Map each element of the structure to a monoid,
  -- and combine the results.
  cfoldMap :: (Monoid m, Constraints f a) => (a -> m) -> f a -> m
  cfoldMap f = cfoldr (mappend . f) mempty
  -- This INLINE allows more list functions to fuse. See Trac #9848.
  {-# INLINE cfoldMap #-}

  -- | Right-associative fold of a structure.
  --
  -- In the case of lists, 'foldr', when applied to a binary operator, a
  -- starting value (typically the right-identity of the operator), and a
  -- list, reduces the list using the binary operator, from right to left:
  --
  -- > foldr f z [x1, x2, ..., xn] == x1 `f` (x2 `f` ... (xn `f` z)...)
  --
  -- Note that, since the head of the resulting expression is produced by
  -- an application of the operator to the first element of the list,
  -- 'foldr' can produce a terminating expression from an infinite list.
  --
  -- For a general 'Foldable' structure this should be semantically identical
  -- to,
  --
  -- @foldr f z = 'List.foldr' f z . 'toList'@
  --
  cfoldr :: Constraints f a => (a -> b -> b) -> b -> f a -> b
  cfoldr f z t = appEndo (cfoldMap (Endo . f) t) z

  -- | Right-associative fold of a structure, but with strict application of
  -- the operator.
  --
  cfoldr' :: Constraints f a => (a -> b -> b) -> b -> f a -> b
  cfoldr' f z0 xs = cfoldl f' id xs z0
    where
      f' k x z = k $! f x z

  -- | Left-associative fold of a structure.
  --
  -- In the case of lists, 'foldl', when applied to a binary
  -- operator, a starting value (typically the left-identity of the operator),
  -- and a list, reduces the list using the binary operator, from left to
  -- right:
  --
  -- > foldl f z [x1, x2, ..., xn] == (...((z `f` x1) `f` x2) `f`...) `f` xn
  --
  -- Note that to produce the outermost application of the operator the
  -- entire input list must be traversed. This means that 'foldl'' will
  -- diverge if given an infinite list.
  --
  -- Also note that if you want an efficient left-fold, you probably want to
  -- use 'foldl'' instead of 'foldl'. The reason for this is that latter does
  -- not force the "inner" results (e.g. @z `f` x1@ in the above example)
  -- before applying them to the operator (e.g. to @(`f` x2)@). This results
  -- in a thunk chain @O(n)@ elements long, which then must be evaluated from
  -- the outside-in.
  --
  -- For a general 'Foldable' structure this should be semantically identical
  -- to,
  --
  -- @foldl f z = 'List.foldl' f z . 'toList'@
  --
  cfoldl :: Constraints f a => (b -> a -> b) -> b -> f a -> b
  cfoldl f z t = appEndo (getDual (cfoldMap (Dual . Endo . flip f) t)) z
  -- There's no point mucking around with coercions here,
  -- because flip forces us to build a new function anyway.

  -- | Left-associative fold of a structure but with strict application of
  -- the operator.
  --
  -- This ensures that each step of the fold is forced to weak head normal
  -- form before being applied, avoiding the collection of thunks that would
  -- otherwise occur. This is often what you want to strictly reduce a finite
  -- list to a single, monolithic result (e.g. 'length').
  --
  -- For a general 'Foldable' structure this should be semantically identical
  -- to,
  --
  -- @foldl f z = 'List.foldl'' f z . 'toList'@
  --
  cfoldl' :: Constraints f a => (b -> a -> b) -> b -> f a -> b
  cfoldl' f z0 xs = cfoldr f' id xs z0
    where
      f' x k z = k $! f z x

  -- | A variant of 'foldr' that has no base case,
  -- and thus may only be applied to non-empty structures.
  --
  -- @'foldr1' f = 'List.foldr1' f . 'toList'@
  cfoldr1 :: Constraints f a => (a -> a -> a) -> f a -> a
  cfoldr1 f xs =
    fromMaybe (errorWithoutStackTrace "foldr1: empty structure")
      (cfoldr mf Nothing xs)
    where
      mf x m = Just $ case m of
        Nothing -> x
        Just y  -> f x y

  -- | A variant of 'foldl' that has no base case,
  -- and thus may only be applied to non-empty structures.
  --
  -- @'foldl1' f = 'List.foldl1' f . 'toList'@
  cfoldl1 :: Constraints f a => (a -> a -> a) -> f a -> a
  cfoldl1 f xs =
    fromMaybe (errorWithoutStackTrace "foldl1: empty structure")
      (cfoldl mf Nothing xs)
    where
      mf m y = Just $ case m of
        Nothing -> y
        Just x  -> f x y

  -- | List of elements of a structure, from left to right.
  ctoList :: Constraints f a => f a -> [a]
  ctoList t = build (\ c n -> cfoldr c n t)
  {-# INLINE ctoList #-}

  -- | Test whether the structure is empty. The default implementation is
  -- optimized for structures that are similar to cons-lists, because there
  -- is no general way to do better.
  cnull :: Constraints f a => f a -> Bool
  cnull = cfoldr (\_ _ -> False) True
  {-# INLINE cnull #-}

  -- | Returns the size/length of a finite structure as an 'Int'.  The
  -- default implementation is optimized for structures that are similar to
  -- cons-lists, because there is no general way to do better.
  clength :: Constraints f a => f a -> Int
  clength = cfoldl' (\c _ -> c + 1) 0

  -- | Does the element occur in the structure?
  celem :: (Eq a, Constraints f a) => a -> f a -> Bool
  celem = cany . (==)
  {-# INLINE celem #-}

  -- | The largest element of a non-empty structure.
  cmaximum :: forall a. (Ord a, Constraints f a) => f a -> a
  cmaximum
    = maybe (errorWithoutStackTrace "maximum: empty structure") getMax
    . getOption
    . cfoldMap (Option . Just . Max)
  {-# INLINABLE cmaximum #-}

  -- | The least element of a non-empty structure.
  cminimum :: forall a. (Ord a, Constraints f a) => f a -> a
  cminimum
    = maybe (errorWithoutStackTrace "maximum: empty structure") getMin
    . getOption
    . cfoldMap (Option . Just . Min)
  {-# INLINABLE cminimum #-}

  -- | The 'sum' function computes the sum of the numbers of a structure.
  csum :: (Num a, Constraints f a) => f a -> a
  csum = getSum . cfoldMap Sum
  {-# INLINABLE csum #-}

  -- | The 'product' function computes the product of the numbers of a
  -- structure.
  cproduct :: (Num a, Constraints f a) => f a -> a
  cproduct = getProduct . cfoldMap Product
  {-# INLINABLE cproduct #-}


-- | Monadic fold over the elements of a structure,
-- associating to the right, i.e. from right to left.
cfoldrM :: (CFoldable f, Monad m, Constraints f a) => (a -> b -> m b) -> b -> f a -> m b
cfoldrM f z0 xs = cfoldl f' return xs z0
  where
    f' k x z = f x z >>= k

-- | Monadic fold over the elements of a structure,
-- associating to the left, i.e. from left to right.
cfoldlM :: (CFoldable f, Monad m, Constraints f a) => (b -> a -> m b) -> b -> f a -> m b
cfoldlM f z0 xs = cfoldr f' return xs z0
  where
    f' x k z = f z x >>= k

-- | Map each element of a structure to an action, evaluate these
-- actions from left to right, and ignore the results. For a version
-- that doesn't ignore the results see 'Data.Traversable.traverse'.
ctraverse_ :: (CFoldable f, Applicative f, Constraints f a) => (a -> f b) -> f a -> f ()
ctraverse_ f = cfoldr ((*>) . f) (pure ())

-- | 'for_' is 'traverse_' with its arguments flipped. For a version
-- that doesn't ignore the results see 'Data.Traversable.for'.
--
-- >>> for_ [1..4] print
-- 1
-- 2
-- 3
-- 4
cfor_ :: (CFoldable f, Applicative f, Constraints f a) => f a -> (a -> f b) -> f ()
{-# INLINE cfor_ #-}
cfor_ = flip ctraverse_

-- | Map each element of a structure to a monadic action, evaluate
-- these actions from left to right, and ignore the results. For a
-- version that doesn't ignore the results see
-- 'Data.Traversable.mapM'.
--
-- As of base 4.8.0.0, 'mapM_' is just 'traverse_', specialized to
-- 'Monad'.
cmapM_ :: (CFoldable f, Monad m, Constraints f a) => (a -> m b) -> f a -> m ()
cmapM_ f = cfoldr ((>>) . f) (return ())

-- | 'forM_' is 'mapM_' with its arguments flipped. For a version that
-- doesn't ignore the results see 'Data.Traversable.forM'.
--
-- As of base 4.8.0.0, 'forM_' is just 'for_', specialized to 'Monad'.
cforM_ :: (CFoldable f, Monad m, Constraints f a) => f a -> (a -> m b) -> m ()
{-# INLINE cforM_ #-}
cforM_ = flip cmapM_

-- | Evaluate each action in the structure from left to right, and
-- ignore the results. For a version that doesn't ignore the results
-- see 'Data.Traversable.sequenceA'.
csequenceA_ :: (CFoldable f, Applicative m, Constraints f (m a)) => f (m a) -> m ()
csequenceA_ = cfoldr (*>) (pure ())

-- | Evaluate each monadic action in the structure from left to right,
-- and ignore the results. For a version that doesn't ignore the
-- results see 'Data.Traversable.sequence'.
--
-- As of base 4.8.0.0, 'sequence_' is just 'sequenceA_', specialized
-- to 'Monad'.
csequence_ :: (CFoldable f, Monad m, Constraints f a, Constraints f (m a)) => f (m a) -> m ()
csequence_ = cfoldr (>>) (return ())

-- | The sum of a collection of actions, generalizing 'concat'.
--
-- asum [Just "Hello", Nothing, Just "World"]
-- Just "Hello"
casum :: (CFoldable f, Alternative m, Constraints f (m a)) => f (m a) -> m a
{-# INLINE casum #-}
casum = cfoldr (<|>) empty

-- | The sum of a collection of actions, generalizing 'concat'.
-- As of base 4.8.0.0, 'msum' is just 'asum', specialized to 'MonadPlus'.
cmsum :: (CFoldable f, MonadPlus m, Constraints f (m a)) => f (m a) -> m a
{-# INLINE cmsum #-}
cmsum = casum

-- | The concatenation of all the elements of a container of lists.
cconcat :: (CFoldable f, Constraints f [a]) => f [a] -> [a]
cconcat xs = build (\c n -> cfoldr (\x y -> cfoldr c y x) n xs)
{-# INLINE cconcat #-}

-- | Map a function over all the elements of a container and concatenate
-- the resulting lists.
cconcatMap :: (CFoldable f, Constraints f a) => (a -> [b]) -> f a -> [b]
cconcatMap f xs = build (\c n -> cfoldr (\x b -> cfoldr c b (f x)) n xs)
{-# INLINE cconcatMap #-}

-- These use foldr rather than cfoldMap to avoid repeated concatenation.

-- | 'and' returns the conjunction of a container of Bools.  For the
-- result to be 'True', the container must be finite; 'False', however,
-- results from a 'False' value finitely far from the left end.
cand :: (CFoldable f, Constraints f Bool) => f Bool -> Bool
cand = getAll . cfoldMap All

-- | 'or' returns the disjunction of a container of Bools.  For the
-- result to be 'False', the container must be finite; 'True', however,
-- results from a 'True' value finitely far from the left end.
cor :: (CFoldable f, Constraints f Bool) => f Bool -> Bool
cor = getAny . cfoldMap Any

-- | Determines whether any element of the structure satisfies the predicate.
cany :: (CFoldable f, Constraints f a) => (a -> Bool) -> f a -> Bool
cany p = getAny . cfoldMap (Any . p)

-- | Determines whether all elements of the structure satisfy the predicate.
call :: (CFoldable f, Constraints f a) => (a -> Bool) -> f a -> Bool
call p = getAll . cfoldMap (All . p)

-- | The largest element of a non-empty structure with respect to the
-- given comparison function.

-- See Note [maximumBy/minimumBy space usage]
cmaximumBy :: (CFoldable f, Constraints f a) => (a -> a -> Ordering) -> f a -> a
cmaximumBy cmp = cfoldl1 max'
  where
    max' x y = case cmp x y of
      GT -> x
      _  -> y

-- | The least element of a non-empty structure with respect to the
-- given comparison function.

-- See Note [maximumBy/minimumBy space usage]
cminimumBy :: (CFoldable f, Constraints f a) => (a -> a -> Ordering) -> f a -> a
cminimumBy cmp = cfoldl1 min'
  where
    min' x y = case cmp x y of
     GT -> y
     _  -> x

-- | 'notElem' is the negation of 'elem'.
cnotElem :: (CFoldable f, Eq a, Constraints f a) => a -> f a -> Bool
cnotElem x = not . celem x

-- | The 'find' function takes a predicate and a structure and returns
-- the leftmost element of the structure matching the predicate, or
-- 'Nothing' if there is no such element.
cfind :: (CFoldable f, Constraints f a) => (a -> Bool) -> f a -> Maybe a
cfind p = getFirst . cfoldMap (\ x -> First (if p x then Just x else Nothing))

{-
Note [maximumBy/minimumBy space usage]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When the type signatures of maximumBy and minimumBy were generalized to work
over any Foldable instance (instead of just lists), they were defined using
foldr1. This was problematic for space usage, as the semantics of maximumBy
and minimumBy essentially require that they examine every element of the
data structure. Using foldr1 to examine every element results in space usage
proportional to the size of the data structure. For the common case of lists,
this could be particularly bad (see Trac #10830).

For the common case of lists, switching the implementations of maximumBy and
minimumBy to foldl1 solves the issue, as GHC's strictness analysis can then
make these functions only use O(1) stack space. It is perhaps not the optimal
way to fix this problem, as there are other conceivable data structures
(besides lists) which might benefit from specialized implementations for
maximumBy and minimumBy (see
https://ghc.haskell.org/trac/ghc/ticket/10830#comment:26 for a further
discussion). But using foldl1 is at least always better than using foldr1, so
GHC has chosen to adopt that approach for now.
-}


instance CFoldable [] where
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

instance CFoldable NonEmpty where
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

instance CFoldable Identity where
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

instance CFoldable ((,) a) where
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

instance CFoldable Maybe where
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

instance CFoldable (Either a) where
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

instance CFoldable (Const a) where
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

instance (CFoldable f, CFoldable g) => CFoldable (Compose f g) where
  {-# INLINABLE cfold    #-}
  {-# INLINABLE cfoldMap #-}
  {-# INLINABLE cfoldr   #-}
  cfold      = cfoldMap cfold . getCompose
  cfoldMap f = cfoldMap (cfoldMap f) . getCompose
  cfoldr f z = cfoldr (\ga acc -> cfoldr f acc ga) z . getCompose

instance (CFoldable f, CFoldable g) => CFoldable (Product.Product f g) where
  {-# INLINABLE cfold    #-}
  {-# INLINABLE cfoldMap #-}
  {-# INLINABLE cfoldr   #-}
  {-# INLINABLE cfoldr'  #-}
  {-# INLINABLE cfoldl   #-}
  {-# INLINABLE cfoldl'  #-}
  {-# INLINABLE cfoldr1  #-}
  {-# INLINABLE cfoldl1  #-}
  cfold       (Pair x y) = cfold x <> cfold y
  cfoldMap f  (Pair x y) = cfoldMap f x <> cfoldMap f y
  cfoldr f z  (Pair x y) = cfoldr f (cfoldr f z y) x
  cfoldr' f z (Pair x y) = cfoldr' f y' x
    where
      !y' = cfoldr' f z y
  cfoldl f z  (Pair x y) = cfoldl f (cfoldl f z y) x
  cfoldl' f z (Pair x y) = cfoldl' f x' x
    where
      !x' = cfoldl' f z y
  cfoldr1 f   (Pair x y) = cfoldl1 f x `f` cfoldl1 f y
  cfoldl1 f   (Pair x y) = cfoldr1 f y `f` cfoldr1 f x

instance (CFoldable f, CFoldable g) => CFoldable (Sum.Sum f g) where
  {-# INLINE cfold    #-}
  {-# INLINE cfoldMap #-}
  {-# INLINE cfoldr   #-}
  {-# INLINE cfoldr'  #-}
  {-# INLINE cfoldl   #-}
  {-# INLINE cfoldl'  #-}
  {-# INLINE cfoldr1  #-}
  {-# INLINE cfoldl1  #-}
  cfold       (InL x) = cfold x
  cfold       (InR y) = cfold y
  cfoldMap f  (InL x) = cfoldMap f x
  cfoldMap f  (InR y) = cfoldMap f y
  cfoldr f z  (InL x) = cfoldr f z x
  cfoldr f z  (InR y) = cfoldr f z y
  cfoldr' f z (InL x) = cfoldr' f z x
  cfoldr' f z (InR y) = cfoldr' f z y
  cfoldl f z  (InL x) = cfoldl f z x
  cfoldl f z  (InR y) = cfoldl f z y
  cfoldl' f z (InL x) = cfoldl' f z x
  cfoldl' f z (InR y) = cfoldl' f z y
  cfoldr1 f   (InL x) = cfoldr1 f x
  cfoldr1 f   (InR y) = cfoldr1 f y
  cfoldl1 f   (InL x) = cfoldl1 f x
  cfoldl1 f   (InR y) = cfoldl1 f y
