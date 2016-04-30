module Tracer.Geometry.Plane (Plane(..)) where

import           Tracer.Base
import           Tracer.Geometry
import           Tracer.Intersection
import           Tracer.Ray
import           Tracer.Vec

data Plane = Plane Dir Scalar

instance Geometry Plane where
  intersect plane ray =
    if dist > 0
      then Intersection dist norm pos
      else farest_intersection
    where
      Plane norm pdist = plane
      Ray ro rd = ray
      dist = (pdist - (ro `dot` norm)) / (rd `dot` norm)
      pos = raypos ray dist

instance Surfceable Plane where
  surface (Plane dir dist) (x, y) =
    dist *| dir + x *| axis1 + y *| axis2
      where axis1 = normalize (dir `cross` (Vector3 1 1 1))
            axis2 = normalize (axis1 `cross` dir)
  surface' (Plane dir dist) pos =
    (d `dot` axis1, d `dot` axis2)
      where axis1 = normalize (dir `cross` (Vector3 1 0 0))
            axis2 = normalize (axis1 `cross` dir)
            d = pos - (dir |* dist)
