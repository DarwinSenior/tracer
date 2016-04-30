module Tracer.Ray where

import           Tracer.Base
import           Tracer.Geometry
import           Tracer.Vec

-- given ray and how far it shoot, gives the position
raypos :: Ray -> Scalar -> Pos
raypos (Ray o dir) l = o + l *| dir

epsilon :: Scalar
epsilon = 1.0e-4

-- sometimes if generating a new ray, it is best that it is to eliminate the salt and pepper effect
rayshift :: Ray -> Ray
rayshift (Ray o d) = (Ray (o + epsilon *| d) d)

-- given intersetion, generate an additional ray
-- all the rays are outward respect to the center
reflect :: Dir -> Dir -> Dir
reflect dir norm = invdir
  where
    invdir = (dir `dot` norm * 2) *| norm - dir

-- given the direction norm and the refract parameter, generate the refract ray
-- all the rays are outward respect to the center
refract :: Dir -> Dir -> Scalar -> Maybe Dir
refract dir norm n =
  let cosi = dir `dot` norm
      sint2 = n * n * (1 - cosi * cosi)
      cost = sqrt (1 - sint2)
  in if sint2 > 1.0
        then Nothing
        else Just . normalize $ (cosi * n - cost) *| norm - n *| dir



instance Transformable Ray where
  trans (Ray o dir) m44 = Ray (o `transp` m44) (dir `transd` m44)

