module Tracer.Ray where

import           Tracer.Vec
import           Tracer.Base
import           Tracer.Geometry

-- given ray and how far it shoot, gives the position
raypos :: Ray -> Scalar -> Pos
raypos (Ray o dir) l = o + l *| dir

epsilon :: Scalar
epsilon = 1.0e-4

-- sometimes if generating a new ray, it is best that it is to eliminate the salt and pepper effect
rayshift :: Ray -> Ray
rayshift (Ray o d) = (Ray (o + epsilon *| d) d)

-- given intersetion, generate an additional ray
reflect :: Dir -> Dir -> Dir
reflect dir norm = invdir
  where
    invdir = (dir `dot` norm * 2) *| norm - dir

instance Transformable Ray where
  trans (Ray o dir) m44 = Ray (o `transp` m44) (dir `transd` m44)
