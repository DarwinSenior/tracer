{-# LANGUAGE BangPatterns #-}
module Tracer.Geometry.Sphere (Sphere(..)) where

import           Tracer.Base
import           Tracer.Geometry
import           Tracer.Geometry.AABB
import           Tracer.Vec
import           Tracer.Ray
import           Tracer.Intersection

data Sphere = Sphere !Pos !Scalar
  deriving Show

instance Geometry Sphere where
  intersect sph ray
    | first > 0 = calculate first
    | second > 0 = calculate second
    | otherwise = farest_intersection
    where
      Ray ro rd = ray
      Sphere so sr = sph
      !oc = ro - so
      (first, second) = solve (rd `dot` rd) (2 * (rd `dot` oc)) ((oc `dot` oc) - sr * sr)
      solve a b c =
        let !delta = sqrt (b * b - 4 * a * c)
        in ((-b - delta) / (2 * a), (-b + delta) / (2 * a))
      calculate dist =
        let pos = raypos ray dist
        in Intersection dist (normalize (pos - so)) pos

instance Transformable Sphere where
  trans (Sphere o r) mat44 = Sphere (o `transp` mat44) r

instance Boundable Sphere where
  bound (Sphere o r) =
    let r' = tovec r
    in AABB (o - r') (o + r')
