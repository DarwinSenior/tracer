module Tracer.Material.Mirror where

import           Tracer.Base
import           Tracer.Material
import           Tracer.Ray
import           Tracer.Vec

data Mirror = Mirror

instance Material Mirror where
  brdf Mirror = purebrdf (tovec 0)
  scatter Mirror (Intersection _ norm pos) (Ray _ in_dir) =
    return $ [(1, rayshift (Ray pos (reflect (-in_dir) norm)))]
