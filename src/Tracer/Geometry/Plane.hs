module Tracer.Geometry.Plane (Plane(..)) where

import           Tracer.Vec
import           Tracer.Base
import           Tracer.Intersection
import           Tracer.Ray

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
