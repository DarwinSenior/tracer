module Tracer.Material.Transparent where

import           Tracer.Base
import           Tracer.Ray
import           Tracer.Vec

data Transparent = Transparent !Scalar


instance Material Transparent where
  brdf _ _ _ _ _ = tovec 0
  scatter (Transparent ita) (Intersection _ norm pos) (Ray _ indir) =
    return $ [rayshift (Ray pos (refract (-indir) norm ita))]
  is_shadowable _ = False
