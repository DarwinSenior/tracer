module Tracer.Geometry.Circle where

import           Tracer.Base
import           Tracer.Vec
import           Tracer.Geometry
import           Tracer.Ray
import           Tracer.Intersection

data Circle = Circle !Pos !Dir !Scalar

instance Geometry Circle where
  intersect circle ray =
    if dist > 0 && insideCircle circle pos
      then Intersection dist norm pos
      else farest_intersection
    where
      Ray ro rd = ray
      Circle co norm _ = circle
      d = norm `dot` co
      dist = (d - (ro `dot` norm)) / (rd `dot` norm)
      pos = raypos ray dist

instance Transformable Circle where
  trans (Circle c norm r) m44 =
    Circle (c `transp` m44) (norm `transd` m44) r

instance Surfceable Circle where
  surface (Circle c n r) (theta, s) =
    c + (x *| axis1) + (y *| axis2)
      where
        axis1 = n `cross` (Vector3 0 0 1)
        axis2 = n `cross` axis1
        rr = (sqrt s) * r
        x = cos (theta * 2 * pi) * rr
        y = sin (theta * 2 * pi) * rr

insideCircle :: Circle -> Pos -> Bool
insideCircle (Circle c norm r) p =
  let v = (p - c)
  in (v `dot` v) < r * r && (v `dot` norm) < 1.0e-5
