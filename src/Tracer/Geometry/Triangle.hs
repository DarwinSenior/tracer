module Tracer.Geometry.Triangle where

import           Tracer.Base
import           Tracer.Vec
import           Tracer.Geometry
import           Tracer.Geometry.AABB (merge)
import           Tracer.Ray
import           Tracer.Intersection

-- Triangle has 4 field p1 p2 p3 and a norm
data Triangle = Triangle !Pos !Pos !Pos !Dir
  deriving Show

create_triangle :: Pos -> Pos -> Pos -> Triangle
create_triangle p1 p2 p3 = Triangle p1 p2 p3 n
  where
    n = (normalize (p1 - p2)) `cross` (normalize (p1 - p3))

instance Geometry Triangle where
  intersect tri ray =
    if dist > 0 && insideTriangle tri pos
      then Intersection dist norm pos
      else farest_intersection
    where
      Ray ro rd = ray
      Triangle p1 _ _ norm = tri
      d = norm `dot` p1
      dist = (d - (ro `dot` norm)) / (rd `dot` norm)
      pos = raypos ray dist

instance Boundable Triangle where
  bound (Triangle p1 p2 p3 _) = merge (merge p1 p2) p3

instance Transformable Triangle where
  trans (Triangle p1 p2 p3 n) m44 =
    Triangle (p1 `transp` m44) (p2 `transp` m44) (p3 `transp` m44) (n `transd` m44)

instance Surfceable Triangle where
  surface (Triangle p1 p2 p3 _) (rx, ry) =
    ((rx *| p1) + (ry *| p2) + ((2 - rx - ry) *| p3)) |/ 2

insideTriangle :: Triangle -> Pos -> Bool
insideTriangle (Triangle p1 p2 p3 _) p =
  let v1 = p2 - p1
      v2 = p3 - p1
      v3 = p - p1
      dot11 = v1 `dot` v1
      dot12 = v1 `dot` v2
      dot13 = v1 `dot` v3
      dot22 = v2 `dot` v2
      dot23 = v2 `dot` v3
      donom = 1.0 / (dot11 * dot22 - dot12 * dot12)
      u = (dot22 - dot13 - dot12 * dot23) * donom
      v = (dot11 * dot23 - dot12 * dot13) * donom
  in (u >= 0) && (v >= 0) && (u + v < 1)
