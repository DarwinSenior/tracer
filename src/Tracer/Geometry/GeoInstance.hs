module Tracer.Geometry.GeoInstance where

import           Tracer.Base
import           Tracer.Vec
import           Tracer.Geometry
import           Tracer.Intersection ()
import           Tracer.Ray ()

data GeoInstance a = GeoInstance Transformation a

instance (Geometry a, Transformable a) => Geometry (GeoInstance a) where
  intersect (GeoInstance mat44 geo) ray =
    (geo `intersect` (ray `trans` (inv mat44))) `trans` mat44
