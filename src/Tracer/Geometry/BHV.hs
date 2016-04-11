{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Tracer.Geometry.BHV (BHV, create_bhv) where

import           Tracer.Geometry
import           Tracer.Geometry.AABB
import           Tracer.Vec
import           Tracer.Base
import           Tracer.Intersection
import           Data.Vector.Algorithms.Intro
import           Control.Monad.ST

import qualified Data.Vector as V

data Node a = LEAF (V.Vector a)
            | Node (AABB, Node a) (AABB, Node a)

data BHV a = BHV (Node a) (V.Vector a)

instance (Geometry a, Boundable a) => Geometry (BHV a) where
  intersect (BHV root _) ray = node_intersect root ray

instance (Boundable a) => Boundable (BHV a) where
  bound (BHV (LEAF vec) _) = getbound vec
  bound (BHV (Node (aabb1, _) (aabb2, _)) _) = merge aabb1 aabb2

instance (Transformable a, Boundable a, Geometry a) => Transformable (BHV a) where
  trans (BHV _ v) mat44 = create_bhv $ V.map (`trans` mat44) v

create_bhv :: (Geometry a, Boundable a) => V.Vector a -> BHV a
create_bhv v = BHV (create_bhv_with v 0) v

getbound :: (Boundable a) => V.Vector a -> AABB
getbound vec
  | V.null vec = AABB (tovec 0) (tovec 0)
  | otherwise = V.foldl' merge (bound $ V.head vec) (V.tail vec)

create_bhv_with :: (Geometry a, Boundable a) => V.Vector a -> Int -> Node a
create_bhv_with vec i =
  if V.length vec < 10
    then LEAF vec
    else Node (aabbleft, create_bhv_with vecleft (i + 1))
           (aabbright, create_bhv_with vecright (i + 1))
  where
    middle = (V.length vec) `div` 2
    aabbleft :: AABB
    aabbleft = getbound vecleft
    aabbright :: AABB
    aabbright = getbound vecright
    (vecleft, vecright) = V.splitAt middle $ runST $ do
      mvec <- V.unsafeThaw vec
      selectBy (cmp_center i) mvec middle
      V.unsafeFreeze mvec

cmp_center :: (Boundable geo) => Int -> geo -> geo -> Ordering
cmp_center order geo1 geo2 =
  (choose $ center geo1) `compare` (choose $ center geo2)
  where
    choose (Vector3 a b c) =
      case order `rem` 3 of
        0 -> a
        1 -> b
        _ -> c

node_intersect :: (Geometry a, Boundable a) => Node a -> Ray -> Intersection
node_intersect (LEAF vec) ray =
  V.foldl min farest_intersection $ V.map (`intersect` ray) vec

node_intersect (Node (lbox, lnode) (rbox, rnode)) ray =
  if (blinter < brinter)
    then min linter rinter
    else min rinter linter
  where
    blinter = lbox `intersect` ray
    brinter = rbox `intersect` ray
    linter = lnode `node_intersect` ray
    rinter = rnode `node_intersect` ray
