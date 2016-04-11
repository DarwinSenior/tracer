
module Tracer.Geometry.GeoInstance where

import Tracer.Base
import Tracer.Geometry
import Tracer.Intersection ()
import Tracer.Ray ()

data GeoInstance a = GeoInstance Transform a

instance (Geometry a, Transformable a) => Geometry (GeoInstance a) where
  intersect (GeoInstance mat44 geo) ray =
    (geo `intersect` (ray `trans` mat44)) `trans` (inv mat44)

inv :: Transform -> Transform
inv = id
