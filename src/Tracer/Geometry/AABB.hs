{-# LANGUAGE TypeSynonymInstances #-}

module Tracer.Geometry.AABB (AABB(..), Boundable(..), merge) where

import           Tracer.Vec
import           Tracer.Base
import           Tracer.Intersection
import           Tracer.Geometry
import           Tracer.Ray

instance Transformable AABB where
  trans (AABB amin amax) mat44 =
    merge (amin `transp` mat44) (amax `transp` mat44)

instance Boundable AABB where
  bound = id

instance Geometry AABB where
  intersect geo ray =
    if tmax > 0 && tmin > tmax
      then Intersection tmin norm pos
      else farest_intersection
    where
      (AABB bmin bmax) = geo
      (Ray o d) = ray
      min_x = (bmin - o) / d
      max_x = (bmax - o) / d
      tmin = (vfold max) $ (vzip min) min_x max_x
      tmax = (vfold min) $ (vzip max) min_x max_x
      pos = raypos ray tmin
      norm = normalize ((collide pos max_x) - (collide pos min_x))
      collide = vzip
                  (\x y -> if (x == y)
                             then 1.0
                             else 0.0)

merge :: (Boundable geoa, Boundable geob) => geoa -> geob -> AABB
merge geoa geob = AABB ((vzip min) amin bmin) ((vzip max) amax bmax)
  where
    (AABB amin amax) = bound geoa
    (AABB bmin bmax) = bound geob
