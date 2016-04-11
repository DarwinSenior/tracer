module Tracer.Material.Mirror where

import Tracer.Base
import Tracer.Vec
import Tracer.Ray

data Mirror = Mirror

instance Material Mirror where
  brdf Mirror _ _ _ = tovec 0
  scatter Mirror (Intersection _ norm pos) (Ray _ in_dir) =
    return $ [rayshift (Ray pos (reflect (-in_dir) norm))]
