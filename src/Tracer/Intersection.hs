module Tracer.Intersection where

import           Tracer.Vec
import           Tracer.Base
import           Tracer.Geometry

instance Transformable Intersection where
  trans (Intersection dist norm pos) m44 =
    Intersection (dist `transs` m44) (norm `transd` m44) (pos `transp` m44)

instance Eq Intersection where
  (==) a b = (d a) == (d b)
    where
      d (Intersection dist _ _) = dist

-- for intersection, it is comparable in terms of its distance
instance Ord Intersection where
  compare a b = (d a) `compare` (d b)
    where
      d (Intersection dist _ _) = dist

inf :: Scalar
inf = 1 / 0

-- this is the default intersection
farest_intersection :: Intersection
farest_intersection = Intersection inf (tovec 0) (tovec inf)
