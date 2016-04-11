module Tracer.Geometry where

import Tracer.Vec
import Tracer.Base

-- AABB minpos maxpos
data AABB = AABB !Pos !Pos

class Surfceable g where
  surface :: g -> (Scalar, Scalar) -> Pos


-- a transformable is able to perform transformation
class Transformable g where
  trans :: g -> Transform -> g

-- To say that the geometry that has a visable bound
class Boundable geo where
  bound :: geo -> AABB
  center :: geo -> Pos
  center geo = (gmin + gmax) |/ 2
    where
      AABB gmin gmax = bound geo
  inside :: geo -> Pos -> Bool
  inside geo x = t1 && t2 && t3
    where
      (AABB (Vector3 a1 a2 a3) (Vector3 b1 b2 b3)) = bound geo
      Vector3 c1 c2 c3 = x
      t1 = a1 < c1 && c1 < b1
      t2 = a2 < c2 && c2 < b2
      t3 = a3 < c3 && c3 < b3

-- class Surfaceable geo where
--   parametrise :: Float -> Float -> Scatter -> Float

instance Transformable Vector3 where
  trans = transp

instance Boundable Vector3 where
  bound p = AABB p p
